pacman::p_load(igraph,
               tidyverse,
               poweRlaw,
               incidentally,
               truncnorm,
               MCMCpack,
               data.table)

generate_social_network <- function(agent_count,
                                    scalefree_m = 2,
                                    smallworld_p = 0.3) {
  generate_connected_network <- function(g) {
    while (any(degree(g) == 0)) {
      g <- add_edges(g, c(sample(V(g)[degree(g) == 0], 1), sample(V(g), 1)))
    }
    return(g)
  }
  
  list(
    network_scalefree = sample_pa(n = agent_count, m = scalefree_m, directed = FALSE),
    network_random = sample_gnp(
      n = agent_count,
      p = (2 * scalefree_m) / (agent_count - 1),
      directed = FALSE,
      loops = FALSE
    )
    # network_smallworld = sample_smallworld(
    #   dim = 1,
    #   size = agent_count,
    #   nei = scalefree_m,
    #   p = smallworld_p
    # )
  ) |>
    map(generate_connected_network) |>
    map(\(g) as_directed(g, mode = "mutual"))
}

generate_topic_preference <- function(agent_count,
                                      topic_count,
                                      dpldis_xmin = 1,
                                      dpldis_alpha = 2.5) {
  generate_matrix <- function(type) {
    preference_heterogeneity <- switch(
      type,
      powerlaw = sample(
        x = seq_len(topic_count),
        size = agent_count,
        replace = TRUE,
        prob = poweRlaw::dpldis(seq_len(topic_count), dpldis_xmin, dpldis_alpha)
      ),
      single = rep(1, agent_count)
    )
    
    topic_popularity <- rmultinom(
      n = 1,
      size = sum(preference_heterogeneity),
      prob = rep(1 / topic_count, topic_count)
    )
    
    incidentally::incidence.from.vector(R = preference_heterogeneity, C = topic_popularity)
  }
  
  return(
    list(
      preference_powerlaw = generate_matrix("powerlaw"),
      preference_single   = generate_matrix("single")
    )
  )
}

generate_threshold <- function(network_list, preference_list) {
  threshold_sampler <- list(
    threshold_uniform = \(n) runif(n, 0, 1),
    threshold_normal  = \(n) truncnorm::rtruncnorm(n, a = 0, b = 1, 0.25, 0.122)
  )
  
  config_dt <- CJ(
    preference_type = names(preference_list),
    network_type = names(network_list),
    threshold_distribution = names(threshold_sampler)
  )
  
  result_dt <- config_dt[, .(threshold = list({
    pref_mat <- preference_list[[preference_type]]
    sampler <- threshold_sampler[[threshold_distribution]]
    
    apply(pref_mat, 2, function(col) {
      val <- numeric(length(col))
      val[col == 1] <- sampler(sum(col == 1))
      val
    })
  })), by = .(preference_type, network_type, threshold_distribution)]
  
  return(result_dt[])
}


generate_neighbor_influence <- function(network_list, preference_list) {
  topic_index <- seq_len(ncol(preference_list[[1]]))
  agent_index <- seq_len(nrow(preference_list[[1]]))
  
  social_influence_list <- lapply(preference_list, \(mat) {
    mat[mat == 1] <- runif(sum(mat == 1))
    mat
  })
  
  redistribute <- function(social_influence, neighbor_weight) {
    mapply(\(ai, wei) {
      sapply(topic_index, \(ti) {
        si_ti <- social_influence[ai, ti]
        wei_ti <- wei[, ti]
        
        if (si_ti == 0 || all(wei_ti == 0)) {
          rep(0, length(wei_ti))
        } else {
          rdirichlet(1, wei_ti) * si_ti
        }
      })
    }, agent_index, neighbor_weight, SIMPLIFY = FALSE)
  }
  
  config_dt <- CJ(
    preference_type = names(preference_list),
    network_type = names(network_list),
    neighbor_influence_measure = c("measure_random", "measure_centrality")
  )
  
  result_dt <- config_dt[, .(neighbor_influence = {
    pref_mat <- preference_list[[preference_type]]
    social_inf <- social_influence_list[[preference_type]]
    net <- network_list[[network_type]]
    
    neighbor <- lapply(adjacent_vertices(net, V(net)), as.integer)
    outdegree <- degree(net, mode = "out")
    neighbor_pref <- lapply(neighbor, \(nei) pref_mat[nei, , drop = FALSE])
    
    weight <- switch(
      neighbor_influence_measure,
      "measure_random"     = neighbor_pref,
      "measure_centrality" = mapply(`*`, neighbor_pref, outdegree, SIMPLIFY = FALSE)
      
    )
    
    list(redistribute(social_inf, weight))
  }), by = .(preference_type, network_type, neighbor_influence_measure)]
  
  return(result_dt[])
}


generate_initial_adopter <- function(network_list,
                                     preference_list,
                                     initial_adopter_fraction = 0.1) {
  select_initial_adopter <- function(score_vector, k) {
    cutoff <- sort(score_vector, decreasing = TRUE)[min(k, length(score_vector))]
    candidate <- which(score_vector >= cutoff)
    sample(candidate, min(k, length(candidate)), replace = FALSE)
  }
  
  config_dt <- CJ(
    preference_type = names(preference_list),
    network_type = names(network_list),
    initial_adopter_strategy = c("initial_random", "initial_centrality")
  )
  
  result_dt <- config_dt[, .(initial_adopter = list({
    pref_mat <- preference_list[[preference_type]]
    net <- network_list[[network_type]]
    
    n_agent <- length(V(net))
    k <- ceiling(initial_adopter_fraction * n_agent)
    node_id <- seq_len(n_agent)
    
    outdegree <- degree(net, mode = "out")
    agent_pref_score <- rowSums(pref_mat)
    
    selected <- switch(
      initial_adopter_strategy,
      "initial_random"     = sample(node_id, k),
      "initial_centrality" = select_initial_adopter(outdegree, k)
    )
    
    as.integer(node_id %in% selected)
  })), by = .(preference_type, network_type, initial_adopter_strategy)]
  
  return(result_dt[])
}

setup_diffusion_context <- function(agent_count, topic_count) {
  network_list <- generate_social_network(agent_count)
  preference_list <- generate_topic_preference(agent_count, topic_count)
  
  threshold_dt <- generate_threshold(network_list, preference_list)
  neighbor_influence_dt <- generate_neighbor_influence(network_list, preference_list)
  initial_adopter_dt <- generate_initial_adopter(network_list, preference_list)
  
  config_dt <- CJ(
    preference_type = names(preference_list),
    network_type = names(network_list),
    threshold_distribution = c("threshold_uniform", "threshold_normal"),
    neighbor_influence_measure = c("measure_random", "measure_centrality"),
    initial_adopter_strategy = c("initial_random", "initial_centrality")
  )
  
  config_dt <- merge(
    config_dt,
    threshold_dt,
    by = c(
      "preference_type",
      "network_type",
      "threshold_distribution"
    )
  )
  config_dt <- merge(
    config_dt,
    neighbor_influence_dt,
    by = c(
      "preference_type",
      "network_type",
      "neighbor_influence_measure"
    )
  )
  config_dt <- merge(
    config_dt,
    initial_adopter_dt,
    by = c(
      "preference_type",
      "network_type",
      "initial_adopter_strategy"
    )
  )
  
  setcolorder(
    config_dt,
    c(
      "preference_type",
      "network_type",
      "threshold_distribution",
      "threshold",
      "neighbor_influence_measure",
      "neighbor_influence",
      "initial_adopter_strategy",
      "initial_adopter"
    )
  )
  
  return(
    list(
      topic_preference = preference_list,
      social_network   = network_list,
      agent_config     = config_dt
    )
  )
}