simulation_data <- readRDS("~/PhD-Rproj-01/data/simulation_data.rds")
source("~/PhD-Rproj-01/scripts/setup_simulation_parameter.R")
source("~/PhD-Rproj-01/scripts/setup_visualization.R")

pacman::p_load(igraph, tidyverse, patchwork, ggtext, ggplot2)

filtered_data <- simulation_data$results %>%
  filter(
    network_type == "network_random",
    threshold_distribution == "threshold_uniform",
    neighbor_influence_measure == "measure_random",
    initial_adopter_strategy == "initial_random",
    topic_dependence == "topic_interdependent"
  ) %>%
  mutate(adoption_final = map_dbl(average_adoption, ~ tail(.x, 1)) / agent_count) |>
  group_by(topic_involved, preference_type) |>
  summarise(mean_final_adoption = mean(adoption_final),
            .groups = "drop")

plot_cross_topic_preference <- filtered_data %>%
  ggplot(.,
         aes(x = topic_involved, y = mean_final_adoption, color = preference_type)) +
  labs(# title = "Final Adoption by Topic Count and Cross-Topic Affiliation",
    x = "Topic Count", y = "Adoption (%)", color = "Cross-Topic Audience Distribution") +
  scale_color_manual(
    values = c(
      "preference_powerlaw" = "#1a80bb",
      "preference_single" = "#ea801c"
    ),
    breaks = c("preference_powerlaw", "preference_single"),
    labels = c(
      "preference_powerlaw" = "Power-Law",
      "preference_single" = "Disjoint"
    )
  ) +
  base_plot_layer +
  base_plot_theme +
  guides(color = guide_legend(nrow = 2))


ggsave(
  file.path(
    "~/PhD-Rproj-01/figures/",
    "plot_cross_topic_preference.pdf"
  ),
  plot_cross_topic_preference,
  width = 3.42,
  height = 3,
  units = "in",
  dpi = 300
)

# ggsave(
#   file.path(
#     "~/PhD-Rproj-01/reports/images/",
#     "plot_cross_topic_preference.png"
#   ),
#   plot = plot_cross_topic_preference,
#   width = 8,
#   height = 5,
#   dpi = 300
# )