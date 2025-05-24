simulation_data <- readRDS("~/PhD-Rproj-01/data/simulation_data.rds")
source("~/PhD-Rproj-01/scripts/setup_simulation_parameter.R")
source("~/PhD-Rproj-01/scripts/setup_visualization.R")

pacman::p_load(igraph, tidyverse, patchwork, ggtext)

facet_order <- c(
  "network_type",
  "initial_adopter_strategy",
  "neighbor_influence_measure",
  "threshold_distribution"
)

facet_labels <- c(
  "network_type" = "(A) Network Structure: Erdos-Renyi (base) vs. Scale-Free",
  "initial_adopter_strategy" = "(B) Seeding Strategy: Random (base) vs. Degree Centrality",
  "neighbor_influence_measure" = "(C) Influence Weighting: Random (base) vs. Degree-Weighted",
  "threshold_distribution" = "(D) Threshold Distribution: Uniform (base) vs. Normal"
)


filtered_data <- simulation_data$results %>%
  filter(topic_dependence == "topic_independent",
         preference_type == "preference_powerlaw") %>%
  mutate(adoption_final = map_dbl(average_adoption, ~ tail(.x, 1)) / agent_count) %>%
  pivot_longer(
    cols = c(
      threshold_distribution,
      neighbor_influence_measure,
      initial_adopter_strategy,
      network_type
    ),
    names_to = "facet_variable",
    values_to = "facet_value"
  ) %>%
  group_by(topic_involved, facet_variable, facet_value) %>%
  summarise(mean_adoption = mean(adoption_final),
            .groups = "drop") %>%
  mutate(
    facet_variable = factor(facet_variable, levels = facet_order),
    facet_value = case_when(
      facet_value %in% c(
        "network_scalefree",
        "initial_centrality",
        "measure_centrality",
        "threshold_normal"
      ) ~ "variant",
      facet_value %in% c(
        "network_random",
        "initial_random",
        "measure_random",
        "threshold_uniform"
      ) ~ "baseline"
    ),
    facet_value = factor(facet_value, levels = c("variant", "baseline"))
  )

plot_topic_interactions <- filtered_data %>%
  ggplot(.,
         aes(
           x = topic_involved,
           y = mean_adoption,
           color = facet_value,
           group = facet_value
         )) +
  facet_wrap(
    ~ facet_variable,
    ncol = 1,
    scales = "fixed",
    labeller = labeller(facet_variable = facet_labels)
  ) +
  labs(# title = "Final Adoption by Topical and Individual Factors",
    x = "Topic Count", y = "Adoption (%)", color = "Diffusion Configuration") +
  scale_color_manual(
    values = c("baseline" = "#1a80bb", "variant" = "#ea801c"),
    labels = c("baseline" = "Baseline", "variant" = "Modified"),
    name = "Diffusion Configuration"
  ) +
  base_plot_layer +
  base_plot_theme +
  guides(color = guide_legend(nrow = 2))


ggsave(
  file.path("~/PhD-Rproj-01/figures/", "plot_topic_interactions.pdf"),
  plot_topic_interactions,
  width = 3.42,
  height = 8.45,
  units = "in",
  dpi = 300
)

# ggsave(
#   file.path(
#     "~/PhD-Rproj-01/reports/images/",
#     "plot_topic_interactions.png"
#   ),
#   plot = plot_topic_interactions,
#   width = 12,
#   height = 11,
#   dpi = 300
# )