## rb_rankings_plot.R
##
## "Draft board" lollipop chart for the top 14 RB prospects, ranked by the
## random forest's projected pick (rb_model.R's pred_pick_rf). Each prospect
## is shown with their college logo and projected overall pick.
##
## Output: a ggplot object (printed to the plot pane / saved as a PDF).

library(tidyverse)
library(ggtext)
library(ggpath)

# Snapshot of pred_pick_rf from rb_model.R at the time this chart was built,
# paired with each prospect's college logo for the chart.
rb_results_df <- data.frame(
  name = c("Jeremiyah Love", "Jadarian Price", "Kaytron Allen", "Emmett Johnson",
           "Jonah Coleman", "Nicholas Singleton", "Mike Washington Jr.", "Seth McGowan",
           "Kaelon Black", "J'Mari Taylor", "Desmond Claiborne", "Le'Veon Moss",
           "Jaydn Ott", "Terion Stewart"),
  pred_pick_rf = c(66.9, 108.5, 112.1, 116.9,
                   121.0, 132.5, 140.8, 147.1,
                   155.4, 155.4, 156.5, 157.4,
                   197.9, 201.6),
  logo_path = file.path("output", "logos", c(
    "Notre_Dame_Fighting_Irish_logo.svg.png", # Love
    "Notre_Dame_Fighting_Irish_logo.svg.png", # Price
    "Penn_State_Nittany_Lions_logo.svg.png",  # Allen
    "Nebraska_Cornhuskers_logo.svg.png",      # Johnson
    "Washington_Huskies_logo.svg.png",        # Coleman
    "Penn_State_Nittany_Lions_logo.svg.png",  # Singleton
    "RightHog.png",                           # Washington Jr.
    "uk.png",                                 # McGowan
    "Indiana_Hoosiers_logo.svg.png",          # Black
    "Virginia_Cavaliers_logo.svg.png",        # Taylor
    "wake.png",                               # Claiborne
    "Texas_A&M_University_logo.svg.png",      # Moss
    "Oklahoma_Sooners_logo.svg.png",          # Ott
    "Virginia_Tech_Hokies_logo.svg.png"       # Stewart
  ))
)

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

rb_plot <- ggplot(rb_results_df, aes(y = reorder(name, -pred_pick_rf), x = pred_pick_rf)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  geom_segment(aes(yend = name, x = 0, xend = pred_pick_rf, color = name), linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(color = name), size = 5) +

  # Logo column sits to the left of the axis (negative x), names just inside it
  geom_from_path(aes(x = -300, path = logo_path), width = 0.05) +
  geom_text(aes(x = -260, label = name), hjust = 0, fontface = "bold", size = 4) +
  geom_text(aes(label = round(pred_pick_rf, 1)), nudge_x = 35, size = 3.5, fontface = "bold") +

  scale_color_manual(values = rb_custom_colors) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(-330, 300), breaks = seq(0, 300, 100)) +
  labs(
    title = "RB MODEL OUTPUT",
    subtitle = "Via Random Forest Regression",
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

print(rb_plot)
