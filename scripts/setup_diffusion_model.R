pacman::p_load(igraph, tidyverse, compiler)

topic_level_LTM <- function(threshold,
                            neighbor,
                            neighbor_influence,
                            status,
                            preference,
                            topic_involved,
                            topic_dependence) {
  v_preference <- preference[, seq_len(topic_involved)] %>%
    as.matrix()
  
  v_threshold <- threshold[, seq_len(topic_involved)] %>%
    as.matrix()
  
  w_influence <- lapply(neighbor_influence, \(x) {
    if (!is.matrix(x))
      x <- matrix(x, nrow = 1)
    x[, seq_len(topic_involved), drop = FALSE]
  })
  
  w_status <- lapply(neighbor, \(x) status[x])
  
  active_w_influence <- mapply(`*`, w_influence, w_status, SIMPLIFY = TRUE)
  
  next_status <- integer(length(status))
  for (v in seq_along(status)) {
    infl_mat <- active_w_influence[[v]]
    pref_mat <- matrix(
      v_preference[v, ],
      nrow = nrow(infl_mat),
      ncol = ncol(infl_mat),
      byrow = TRUE
    )
    effective_w_influence <- infl_mat * pref_mat
    
    next_status[v] <- as.integer(switch(
      topic_dependence,
      topic_independent = any((v_threshold[v, ] != 0) &
                                (
                                  v_threshold[v, ] <= colSums(effective_w_influence)
                                )),
      topic_interdependent = any((v_threshold[v, ] != 0) &
                                   (
                                     sum(v_threshold[v, ]) <= sum(effective_w_influence)
                                   ))
    ))
  }
  
  return(pmax(status, next_status))
  
}
