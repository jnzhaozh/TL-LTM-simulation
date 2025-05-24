simulation_data <- readRDS("~/PhD-Rproj-01/data/simulation_data.rds")
source("~/PhD-Rproj-01/scripts/setup_simulation_parameter.R")
source("~/PhD-Rproj-01/scripts/setup_visualization.R")

pacman::p_load(igraph, tidyverse, patchwork, ggtext)

filtered_data <- simulation_data$results |>
  filter(
    preference_type == "preference_powerlaw",
    network_type == "network_scalefree",
    threshold_distribution == "threshold_uniform",
    neighbor_influence_measure == "measure_random",
    initial_adopter_strategy == "initial_random",
  ) %>%
  mutate(adoption_final = map_dbl(average_adoption, ~ tail(.x, 1)) / agent_count) |>
  group_by(topic_involved, topic_dependence) |>
  summarise(mean_final_adoption = mean(adoption_final),
            .groups = "drop")

plot_inter_topic_dependence <- filtered_data %>%
  ggplot(.,
         aes(x = topic_involved, y = mean_final_adoption, color = topic_dependence)) +
  labs(# title = "Final Adoption by Topic Count and Inter-Topic Relationship",
    x = "Topic Count", y = "Adoption (%)", color = "Inter-Topic Dependence") +
  scale_color_manual(
    values = c(
      "topic_independent" = "#1a80bb",
      "topic_interdependent" = "#ea801c"
    ),
    labels = c(
      "topic_independent" = "Independent",
      "topic_interdependent" = "Interdependent"
    )
  ) +
  base_plot_layer +
  base_plot_theme +
  guides(color = guide_legend(nrow = 2))


ggsave(
  file.path(
    "~/PhD-Rproj-01/figures/",
    "plot_inter_topic_dependence.pdf"
  ),
  plot_inter_topic_dependence,
  width = 3.42,
  height = 3,
  units = "in",
  dpi = 300
)

# ggsave(
#   file.path("~/PhD-Rproj-01/figures/", "plot_inter_topic_dependence.pdf"),
#   plot_inter_topic_dependence,
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 300
# )
#
# ggsave(
#   file.path(
#     "~/PhD-Rproj-01/reports/images/",
#     "plot_inter_topic_dependence.png"
#   ),
#   plot = plot_inter_topic_dependence,
#   width = 8,
#   height = 5,
#   dpi = 300
# )