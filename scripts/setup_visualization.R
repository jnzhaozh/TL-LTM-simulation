library(ggplot2)

# Base plot theme
base_plot_layer <- list(
  geom_line(linewidth = .6),
  geom_point(size = 1.2),
  scale_y_continuous(labels = scales::percent_format(accuracy = .1))
)

# base_plot_theme <- theme_minimal(base_size = 13, base_family = "serif") +
#   theme(
#     panel.grid.major = element_line(color = "grey90", size = 0.2),
#     panel.grid.minor = element_blank(),
#     axis.title = element_text(size = 13),
#     axis.text = element_text(size = 11),
#     strip.text = element_text(size = 13, face = "bold", hjust = 0),
#     plot.title = element_text(face = "bold", size = 14, hjust = 0),
#     legend.position = "right",
#     legend.text = element_text(size = 11)
#   )

base_plot_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    strip.text = element_text(size = 8, hjust = 0),
    # strip.text = ggtext::element_markdown(size = 8, hjust = 0, lineheight = 1.2),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )