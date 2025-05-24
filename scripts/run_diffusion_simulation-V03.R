source("~/PhD-Rproj-01/scripts/setup_diffusion_context.R")
source("~/PhD-Rproj-01/scripts/setup_diffusion_model.R")
source("~/PhD-Rproj-01/scripts/setup_simulation_parameter.R")

pacman::p_load(igraph, tidyverse, furrr, compiler, future, future.apply, tictoc)


run_diffusion <- function(param) {
  agent_count <- unique(param$agent_count)
  topic_count <- unique(param$topic_count)
  
  # 1. Setup fresh diffusion context
  context <- setup_diffusion_context(agent_count, topic_count)
  network_neighbor <- context$social_network %>%
    lapply(., \(net) {
      map(adjacent_vertices(net, V(net)), as.integer)
    })
  network_diameter <- context$social_network %>%
    sapply(., diameter)
  
  
  # 2. Helper function to simulate one config
  run_one_diffusion <- function(config_i) {
    config <- context$agent_config[preference_type == config_i$preference_type &
                                     network_type == config_i$network_type &
                                     threshold_distribution == config_i$threshold_distribution &
                                     neighbor_influence_measure == config_i$neighbor_influence_measure &
                                     initial_adopter_strategy == config_i$initial_adopter_strategy]
    
    preference <- context$topic_preference[[config_i$preference_type]]
    neighbor <- network_neighbor[[config_i$network_type]]
    threshold <- config$threshold[[1]]
    neighbor_influence <- config$neighbor_influence[[1]]
    status <- config$initial_adopter[[1]]
    time_step <- network_diameter[[config_i$network_type]]
    
    adoption_over_time <- numeric(time_step + 1)
    adoption_over_time[1] <- sum(status)
    current_status <- status
    
    for (step in seq_len(time_step)) {
      next_status <- topic_level_LTM(
        threshold = threshold,
        neighbor = neighbor,
        neighbor_influence = neighbor_influence,
        status = current_status,
        preference = preference,
        topic_involved = config_i$topic_involved,
        topic_dependence = config_i$topic_dependence
      )
      
      current_status <- next_status
      adoption_over_time[step + 1] <- sum(current_status)
    }
    
    config_i$adoption <- list(adoption_over_time)
    return(config_i)
  }
  
  # 3. Run simulations row-wise and collect results
  result_dt <- future_lapply(seq_len(nrow(param)), \(i) {
    run_one_diffusion(param[i])
  }, future.seed = TRUE) %>%
    rbindlist()
  
  # 4. Return results and metadata
  return(list(
    results = result_dt,
    metadata = list(
      topic_preference = context$topic_preference,
      social_network = context$social_network
    )
  ))
}


iterate_diffusion <- function(param) {
  agent_count <- unique(param$agent_count)
  topic_count <- unique(param$topic_count)
  iteration <- unique(param$iteration)
  
  # Pre-initialize aggregation
  param[, average_adoption := vector("list", .N)]
  for (i in seq_len(nrow(param))) {
    param$average_adoption[[i]] <- numeric(0)
  }
  
  # Run iterations in parallel
  iteration_outputs <- future_lapply(seq_len(iteration), future.seed = TRUE, \(rep) {
    message("Running iteration ", rep)
    
    run_output <- run_diffusion(param)
    
    list(
      result_dt = run_output$results,
      metadata  = list(
        iteration_id     = rep,
        topic_preference = run_output$metadata$topic_preference,
        social_network   = run_output$metadata$social_network
      )
    )
  })
  
  max_length <- max(unlist(lapply(iteration_outputs, function(out) {
    sapply(out$result_dt$adoption, length)
  })))
  
  # Aggregate results and metadata
  input_metadata_list <- vector("list", iteration)
  for (rep in seq_along(iteration_outputs)) {
    result_dt <- iteration_outputs[[rep]]$result_dt
    metadata  <- iteration_outputs[[rep]]$metadata
    input_metadata_list[[rep]] <- data.table(
      iteration_id = metadata$iteration_id,
      topic_preference = list(metadata$topic_preference),
      social_network = list(metadata$social_network)
    )
    
    for (i in seq_len(nrow(result_dt))) {
      current_adopt <- result_dt$adoption[[i]]
      prev_avg <- param$average_adoption[[i]]
      
      # Pad vectors
      # len_max <- max(length(prev_avg), length(current_adopt))
      # if (length(prev_avg) < len_max)
      #   prev_avg <- c(prev_avg, rep(0, len_max - length(prev_avg)))
      # if (length(current_adopt) < len_max)
      #   current_adopt <- c(current_adopt, rep(current_adopt[length(current_adopt)], len_max - length(current_adopt)))
      
      if (length(prev_avg) < max_length)
        prev_avg <- c(prev_avg, rep(0, max_length - length(prev_avg)))
      if (length(current_adopt) < max_length)
        current_adopt <- c(current_adopt, rep(current_adopt[length(current_adopt)], max_length - length(current_adopt)))
      
      
      param$average_adoption[[i]] <- prev_avg + (current_adopt / iteration)
    }
  }
  
  input_metadata_dt <- rbindlist(input_metadata_list)
  
  return(list(results = param[, .(
    preference_type,
    network_type,
    threshold_distribution,
    neighbor_influence_measure,
    initial_adopter_strategy,
    topic_involved,
    topic_dependence,
    average_adoption
  )], input_metadata = input_metadata_dt))
}

run_parallel_simulation <- function(param) {
  plan(multisession, workers = parallel::detectCores() - 5)
  on.exit(plan(sequential), add = TRUE)
  
  tictoc::tic()
  result <- iterate_diffusion(param)
  tictoc::toc()
  
  return(result)
}

simulation_data <- run_parallel_simulation(param)

saveRDS(simulation_data, file = "~/PhD-Rproj-01/data/simulation_data.rds")
