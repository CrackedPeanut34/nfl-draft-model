## wr_rankings_plot.R
##
## "Draft board" lollipop chart for the top 14 WR prospects, ranked by the
## linear model's projected pick (wr_model.R's pred_pick_lm). Each prospect
## is shown with their college logo and projected overall pick.
##
## Output: a ggplot object (printed to the plot pane / saved as a PDF).

library(tidyverse)
library(ggtext)
library(ggpath)

# Snapshot of pred_pick_lm from wr_model.R at the time this chart was built,
# paired with each prospect's college logo for the chart.
wr_results_df <- data.frame(
  name = c("Elijah Sarratt", "Omar Cooper Jr.", "Makai Lemon", "Carnell Tate",
           "KC Concepcion", "Denzel Boston", "Malachi Fields", "Chris Brazzell II",
           "Chris Bell", "Germie Bernard", "Ja'Kobi Lane", "Zachariah Branch",
           "Jordyn Tyson", "Antonio Williams"),
  pred_pick_lm = c(91.5, 92.6, 93.0, 97.4, 101.1, 103.5, 103.7, 107.3, 118.9, 120.2, 122.1, 124.1, 127.8, 134),
  logo_path = file.path("output", "logos", c(
    "Indiana_Hoosiers_logo.svg.png",          # Sarratt
    "Indiana_Hoosiers_logo.svg.png",          # Cooper Jr.
    "USC_Trojans_logo.svg.png",               # Lemon
    "Ohio_State_Buckeyes_logo.svg.png",       # Tate
    "NC_State_Wolfpack_logo.svg.png",         # Concepcion
    "Washington_Huskies_logo.svg.png",        # Boston
    "Notre_Dame_Fighting_Irish_logo.svg.png", # Fields
    "Tennessee_Volunteers_logo.svg.png",      # Brazzell II
    "Louisville_Cardinals_logo.svg.png",      # Bell
    "Alabama_Athletics_logo.svg.png",         # Bernard
    "USC_Trojans_logo.svg.png",               # Lane
    "Georgia_Athletics_logo.svg.png",         # Branch
    "Arizona_State_Sun_Devils_logo.svg.png",  # Tyson
    "Clemson_Tigers_logo.svg.png"             # Williams
  ))
)

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

wr_plot <- ggplot(wr_results_df, aes(y = reorder(name, -pred_pick_lm), x = pred_pick_lm)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  geom_segment(aes(yend = name, x = 0, xend = pred_pick_lm, color = name), linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(color = name), size = 5) +

  # Logo column sits to the left of the axis (negative x), names just inside it
  geom_from_path(aes(x = -105, path = logo_path), width = 0.038) +
  geom_text(aes(x = -95, label = name), hjust = 0, fontface = "bold", size = 4) +
  geom_text(aes(label = round(pred_pick_lm, 1)), nudge_x = 16, size = 3.5, fontface = "bold") +

  scale_color_manual(values = wr_custom_colors) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-130, 160), breaks = seq(0, 150, 50)) +
  labs(
    title = "WR MODEL OUTPUT",
    subtitle = "Via Multiple Linear Regression",
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

print(wr_plot)
