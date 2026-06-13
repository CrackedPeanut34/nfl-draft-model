## qb_rankings_plot.R
##
## "Draft board" lollipop chart for the top 14 QB prospects, ranked by the
## KNN-comp projected pick (qb_knn_comparison.R's pred_pick_knn). Each
## prospect is shown with their college logo and projected overall pick.
##
## Output: a ggplot object (printed to the plot pane / saved as a PDF).

library(tidyverse)
library(ggtext)
library(ggpath)

# Snapshot of pred_pick_knn from qb_knn_comparison.R at the time this chart
# was built, paired with each prospect's college logo for the chart.
qb_results_df <- data.frame(
  name = c("Taylen Green", "Fernando Mendoza", "Sawyer Robertson", "Joey Aguilar",
           "Ty Simpson", "Diego Pavia", "Behren Morton", "Carson Beck",
           "Jalon Daniels", "Luke Altmyer", "Cade Klubnik", "Miller Moss",
           "Garrett Nussmeier", "Mark Gronowski"),
  pred_pick_knn = c(64.5, 67.8, 69.8, 77.0,
                    117.0, 117.8, 124.7, 147.7,
                    149.2, 153.0, 164.8, 174.5,
                    184.0, 186.5),
  logo_path = file.path("output", "logos", c(
    "RightHog.png",                          # Green (Arkansas)
    "Indiana_Hoosiers_logo.svg.png",         # Mendoza
    "Baylor-Bears-logo.png",                 # Robertson
    "Tennessee_Volunteers_logo.svg.png",     # Aguilar
    "Alabama_Athletics_logo.svg.png",        # Simpson
    "Vanderbilt-Commodores-Logo.png",        # Pavia
    "Texas_Tech_Athletics_logo.svg.png",     # Morton
    "miami.png",                             # Beck
    "Kansas_Jayhawks_1946_logo.svg.png",     # Daniels
    "Illinois-Fighting-Illini-Logo.png",     # Altmyer
    "Clemson_Tigers_logo.svg.png",           # Klubnik
    "Louisville_Cardinals_logo.svg.png",     # Moss
    "images-1.png",                          # Nussmeier (LSU)
    "Iowa_Hawkeyes_logo.svg.png"             # Gronowski
  ))
)

qb_custom_colors <- c(
  "Fernando Mendoza" = "maroon", "Taylen Green" = "maroon", "Ty Simpson" = "maroon",
  "Diego Pavia" = "gold", "Carson Beck" = "orange", "Joey Aguilar" = "orange",
  "Sawyer Robertson" = "darkgreen", "Behren Morton" = "darkred", "Mark Gronowski" = "yellow",
  "Luke Altmyer" = "darkblue", "Garrett Nussmeier" = "purple4", "Jalon Daniels" = "blue",
  "Cade Klubnik" = "orange", "Miller Moss" = "red"
)

qb_plot <- ggplot(qb_results_df, aes(y = reorder(name, -pred_pick_knn), x = pred_pick_knn)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  geom_segment(aes(yend = name, x = 0, xend = pred_pick_knn, color = name), linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(color = name), size = 5) +

  # Logo column sits to the left of the axis (negative x), names just inside it
  geom_from_path(aes(x = -260, path = logo_path), width = 0.05) +
  geom_text(aes(x = -220, label = name), hjust = 0, fontface = "bold", size = 4) +
  geom_text(aes(label = round(pred_pick_knn, 1)), nudge_x = 30, size = 3.5, fontface = "bold") +

  scale_color_manual(values = qb_custom_colors) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-280, 260), breaks = seq(0, 250, 50)) +
  labs(
    title = "QB MODEL OUTPUT",
    subtitle = "Via K Nearest Neighbors Regression",
    y = "",
    x = "Projected Overall Draft Position"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15))
  )

print(qb_plot)
