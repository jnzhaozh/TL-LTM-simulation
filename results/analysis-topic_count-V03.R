simulation_data <- readRDS("~/PhD-Rproj-01/data/simulation_data.rds")
source("~/PhD-Rproj-01/scripts/setup_simulation_parameter.R")
source("~/PhD-Rproj-01/scripts/setup_visualization.R")

pacman::p_load(igraph, tidyverse, patchwork, ggplot2, ggtext)

filtered_data <- simulation_data$results %>%
  filter(
    preference_type == "preference_powerlaw",
    network_type == "network_scalefree",
    threshold_distribution == "threshold_uniform",
    neighbor_influence_measure == "measure_random",
    initial_adopter_strategy == "initial_random",
    topic_dependence == "topic_interdependent"
  ) %>%
  transmute(topic_involved, average_adoption) %>%
  mutate(topic_involved = as.factor(topic_involved)) %>%
  mutate(adoption_df = map(average_adoption, ~ tibble(
    step = seq_along(.x), adoption = .x
  ))) %>%
  unnest(adoption_df)

plot_topic_count <- filtered_data %>%
  ggplot(., aes(
    x = step,
    y = adoption / agent_count,
    color = factor(topic_involved)
  )) +
  labs(# title = "Diffusion Trajectories by Topic Count",
    x = "Simulation Step", y = "Adoption (%)", color = "Topic Count") +
  scale_x_continuous(breaks = seq(1, max(filtered_data$step), by = 1)) +
  scale_color_manual(
    values = c(
      "1" = "#bababa",
      "2" = "#a1a1a1",
      "3" = "#7f7f7f",
      "4" = "#595959",
      "5" = "#262626"
      # "1" = "#b5d1ae",
      # "2" = "#80ae9a",
      # "3" = "#568b87",
      # "4" = "#326b77",
      # "5" = "#1b485e"
    ),
    breaks = c("1", "2", "3", "4", "5")
  ) +
  base_plot_layer +
  base_plot_theme



# save_dir <- path.expand("~/PhD-Rproj-01/figures")
# if (!dir.exists(save_dir))
#   dir.create(save_dir, recursive = TRUE)

ggsave(
  file.path("~/PhD-Rproj-01/figures/", "plot_topic_count.pdf"),
  plot_topic_count,
  width = 3.42,
  height = 3,
  units = "in",
  dpi = 300
)

# ggsave(
#   file.path("~/PhD-Rproj-01/figures/", "plot_topic_count.pdf"),
#   plot_topic_count,
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 300
# )

# ggsave(
#   file.path(
#     "~/PhD-Rproj-01/reports/images/",
#     "plot_topic_count.png"
#   ),
#   plot = plot_topic_count,
#   width = 8,
#   height = 5,
#   dpi = 300
# )