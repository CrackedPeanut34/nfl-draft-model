## rb_scatter_plot.R
##
## Scatter plot of the 2026 RB class on the two stats the random forest in
## rb_model.R weighted most heavily: rushing yards/game and rushing TDs/game,
## with point color showing team SRS.
##
## Input: output/results/RB_Final_Draft_Results.csv (run rb_model.R first)

library(ggplot2)
library(ggrepel)

rb_results <- read.csv("output/results/RB_Final_Draft_Results.csv")

rb_custom_colors <- c(
  "Jeremiyah Love" = "navy",
  "Jonah Coleman" = "purple",
  "Kaytron Allen" = "navy",
  "Emmett Johnson" = "red",
  "Nicholas Singleton" = "navy",
  "Jadarian Price" = "navy",
  "J'Mari Taylor" = "orange",
  "Seth McGowan" = "blue",
  "Mike Washington Jr." = "maroon",
  "Desmond Claiborne" = "gold",
  "Le'Veon Moss" = "maroon",
  "Kaelon Black" = "maroon",
  "Jaydn Ott" = "maroon",
  "Terion Stewart" = "maroon"
)

rb_scatter <- ggplot(rb_results, aes(x = rushing_yds_pg, y = rushing_td_pg)) +
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
  scale_color_manual(values = rb_custom_colors, guide = "none") +
  theme_minimal() +
  labs(
    title = "RB CHART OF MOST IMPORTANT FEATURES",
    subtitle = "Top 3 Modeling Features: Yds/G, TD/G, and SRS",
    x = "Rushing Yards Per Game",
    y = "Rushing Touchdowns Per Game"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(rb_scatter)
