## qb_scatter_plot.R
##
## Scatter plot of the 2026 QB class on the two stats the random forest in
## qb_model.R weighted most heavily: completion percentage and interceptions
## per game, with point color showing height.
##
## Input: output/results/QB_Final_Draft_Results.csv (run qb_model.R first)

library(ggplot2)
library(ggrepel)

qb_results <- read.csv("output/results/QB_Final_Draft_Results.csv")

qb_custom_colors <- c(
  "Fernando Mendoza" = "maroon",
  "Taylen Green" = "maroon",
  "Ty Simpson" = "maroon",
  "Diego Pavia" = "gold",
  "Carson Beck" = "orange",
  "Joey Aguilar" = "orange",
  "Sawyer Robertson" = "darkgreen",
  "Behren Morton" = "darkred",
  "Mark Gronowski" = "black",
  "Luke Altmyer" = "darkblue",
  "Garrett Nussmeier" = "purple4",
  "Jalon Daniels" = "blue",
  "Cade Klubnik" = "orange",
  "Miller Moss" = "red"
)

qb_scatter <- ggplot(qb_results, aes(x = completion_pct, y = passing_int_pg)) +
  geom_point(aes(fill = height),
             size = 5,
             alpha = 0.8,
             stroke = 1,
             color = "black",
             shape = 21) +
  geom_text_repel(aes(label = name, color = name),
                  fontface = "bold",
                  size = 3.5,
                  box.padding = 0.5,
                  point.padding = 0.5,
                  segment.color = "grey50") +
  scale_fill_gradient(low = "white", high = "blue", name = "Height (Inches)") +
  scale_color_manual(values = qb_custom_colors, guide = "none") +
  theme_minimal() +
  labs(
    title = "QB CHART OF MOST IMPORTANT FEATURES",
    subtitle = "Top 3 Modeling Features: Accuracy, Height, and Interceptions",
    x = "Completion Percentage (%)",
    y = "Passing Interceptions Per Game"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(qb_scatter)
