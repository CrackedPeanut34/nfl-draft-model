## wr_scatter_plot.R
##
## Scatter plot of the 2026 WR class on the two stats the random forest in
## wr_model.R weighted most heavily: receiving TDs/game and yards per
## reception, with point color showing team SRS.
##
## Input: output/results/WR_Final_Draft_Results.csv (run wr_model.R first)

library(ggplot2)
library(ggrepel)
library(dplyr)

wr_results <- read.csv("output/results/WR_Final_Draft_Results.csv")

wr_custom_colors <- c(
  "Elijah Sarratt" = "maroon",
  "Omar Cooper Jr." = "maroon",
  "Makai Lemon" = "darkred",
  "Carnell Tate" = "red",
  "KC Concepcion" = "red",
  "Denzel Boston" = "purple",
  "Malachi Fields" = "navy",
  "Chris Brazzell II" = "orange",
  "Chris Bell" = "red3",
  "Germie Bernard" = "maroon",
  "Ja'Kobi Lane" = "darkred",
  "Zachariah Branch" = "orangered",
  "Jordyn Tyson" = "gold",
  "Antonio Williams" = "orange"
)

wr_scatter <- ggplot(wr_results, aes(x = receiving_td_pg, y = receiving_ypr)) +
  geom_point(aes(fill = SRS),
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
  scale_fill_gradient(low = "white", high = "blue", name = "Team SRS") +
  scale_color_manual(values = wr_custom_colors, guide = "none") +
  theme_minimal() +
  labs(
    title = "WR CHART OF MOST IMPORTANT FEATURES",
    subtitle = "Top 3 Modeling Features: TDs/G, Yards Per Catch, and SRS",
    x = "Receiving Touchdowns Per Game (Scoring)",
    y = "Receiving Yards Per Catch (Efficiency)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(wr_scatter)
