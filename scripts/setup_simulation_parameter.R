pacman::p_load(data.table)

agent_count <- 5000
topic_count <- 5
iteration <- 50

param <- CJ(
  preference_type = c("preference_powerlaw", "preference_single"),
  network_type = c("network_scalefree", "network_random"),
  threshold_distribution = c("threshold_uniform", "threshold_normal"),
  neighbor_influence_measure = c("measure_random", "measure_centrality"),
  initial_adopter_strategy = c("initial_random", "initial_centrality"),
  topic_involved = seq_len(topic_count),
  topic_dependence = c("topic_independent", "topic_interdependent")
)[, `:=`(agent_count = agent_count,
         topic_count = topic_count,
         iteration = iteration)]
