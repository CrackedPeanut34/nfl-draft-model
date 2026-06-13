## qb_voronoi_map.R
##
## Voronoi "territory map" of the 2026 QB class on passing yards/game vs.
## passing TDs/game. Each prospect's tile size reflects how statistically
## isolated they are from their peers on these two axes.
##
## passing_yds_pg / passing_td_pg below are a snapshot of each prospect's
## per-game stats at the time this chart was built (see qb_model.R for the
## live calculation from QB_Test.csv).

library(tidyverse)
library(ggforce)
library(ggrepel)

qb_custom_colors <- c(
  "Fernando Mendoza" = "maroon",
  "Taylen Green" = "maroon",
  "Ty Simpson" = "maroon",
  "Diego Pavia" = "gold",
  "Carson Beck" = "orange",
  "Joey Aguilar" = "orange",
  "Sawyer Robertson" = "darkgreen",
  "Behren Morton" = "darkred",
  "Mark Gronowski" = "yellow",
  "Luke Altmyer" = "darkblue",
  "Garrett Nussmeier" = "purple4",
  "Jalon Daniels" = "blue",
  "Cade Klubnik" = "orange",
  "Miller Moss" = "red"
)

qb_voronoi_df <- data.frame(
  name = c("Fernando Mendoza", "Diego Pavia", "Carson Beck", "Taylen Green", "Ty Simpson",
           "Miller Moss", "Garrett Nussmeier", "Cade Klubnik", "Luke Altmyer", "Joey Aguilar",
           "Sawyer Robertson", "Jalon Daniels", "Behren Morton", "Mark Gronowski"),
  passing_yds_pg = c(220.9, 272.2, 255.0, 226.2, 244.5, 223.3, 214.1, 226.4, 225.0, 230.0, 250.0, 210.0, 240.0, 200.0),
  passing_td_pg = c(2.56, 2.23, 2.0, 1.58, 1.83, 1.33, 1.33, 1.23, 1.83, 2.0, 2.5, 1.83, 1.83, 1.5)
)

voronoi_plot <- ggplot(qb_voronoi_df, aes(x = passing_yds_pg, y = passing_td_pg)) +
  geom_voronoi_tile(aes(fill = name), alpha = 0.25, colour = "white", linewidth = 0.5) +
  geom_point(aes(color = name), size = 3) +
  geom_text_repel(aes(label = name, color = name),
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5) +
  scale_fill_manual(values = qb_custom_colors, guide = "none") +
  scale_color_manual(values = qb_custom_colors, guide = "none") +
  theme_minimal() +
  labs(
    title = "2026 QB SCHEMATIC GRAVITY MAP",
    subtitle = "Territory size represents statistical uniqueness and 'Unicorn' potential",
    x = "Passing Yards Per Game (Volume)",
    y = "Passing Touchdowns Per Game (Impact)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "grey30", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(voronoi_plot)
