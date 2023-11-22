# load libraries ----
library(tidyverse)
library(janitor)
library(extrafont)
library(gganimate)
library(gifski)
library(Cairo)
library(patchwork)

# font_import() # run this once
loadfonts(device = "win")
CairoWin()

# load data ----
df <- read_csv("herodotus/data/F_app_and_height_vs_delta.csv") |> 
  clean_names() |> 
  filter(delta_cm < 100)

theme_set(
  theme_minimal(base_size = 32) + 
    theme(
      text = element_text(family = "Segoe UI Light"), #, face = "italic")
      panel.border = element_rect(color = "black", linewidth = 1, fill = NA)
    )
)


# final row has some NAs

# plot 1 ----
(gg1 <- df |> 
   ggplot(
     aes(x = delta_cm,
         y = f_app_over_g_kg)
   ) +
   geom_line(linewidth = 2, color = "#EA7580", lineend = "round") + 
   labs(
     x = expression(paste(delta, " (cm)")),
     y = expression(paste(F["app"]/g, " (kg)"))
   )) 

# Potential for adding labels ----
highlight_point <- function(gg, data, value){
  df1 <- data |> 
    filter(delta_cm %in% value)
  
  gg +
    geom_segment(data = df1, aes(
      x = delta_cm, xend = delta_cm,
      y = -Inf, yend = f_app_over_g_kg),
      linetype = "dashed") +
    geom_segment(data = df1, aes(
      x = -Inf, xend = delta_cm,
      y = f_app_over_g_kg, yend = f_app_over_g_kg),
      linetype = "dashed") +
    geom_point(data = df1, size = 5, color = "#EA7580", alpha = 0.5) + 
    geom_label(data = df1,
      aes(x = delta_cm, y = f_app_over_g_kg, 
          label = str_c("x: ", delta_cm, "\n", 
                        "y: ", f_app_over_g_kg)),
      vjust = -0.5)
}

gg1 |> 
  highlight_point(data = df, value = 30)

# Now back to regularly scheduled programming ----

ggsave("plot1.png", plot = gg1, 
       height = 1080, width = 1920, units = "px",
       type = "cairo")

gg_animate1 <- gg1 +
  transition_reveal(delta_cm) +
  view_follow(fixed_y = TRUE,
              fixed_x = FALSE)

# Save animation
animate(
  gg_animate1,
  height = 1080, width = 1920, fps = 30, duration = 10,
  end_pause = 120, rewind = FALSE,
  renderer = gifski_renderer(),
  type = "cairo"
)

anim_save("plot1.gif")

# plot 2 ----
(gg2 <- df |> 
  ggplot(
    aes(x = delta_cm,
        y = delta_height_cm)
  ) +
  geom_line(linewidth = 2, color = "#088BBE") +
  labs(
    x = expression(paste(delta, " (cm)")),
    y = "height (cm)"
  ))

gg3 <- gg1 / gg2
gg3

gg_animate2 <- gg3 +
  transition_reveal(delta_cm) +
  view_follow(fixed_y = TRUE,
              fixed_x = FALSE)

# Save animation
animate(
  gg_animate2,
  height = 1080, width = 1920, fps = 30, duration = 10,
  end_pause = 120, rewind = FALSE,
  renderer = gifski_renderer()
)

anim_save("plot2.gif")

## painful approach:

gg3 <- df |> 
  tidyr::pivot_longer(
    cols = c(f_app_over_g_kg, delta_height_cm),
    names_to = "y_axis",
    values_to = "value"
  ) |> 
  mutate(y_axis = factor(y_axis, 
                         levels = c("f_app_over_g_kg", "delta_height_cm"),
                         labels = c(expression(paste(F["app"]/g, " (kg)")), 
                                    expression(paste("height (cm)")))
                         )) |> 
  ggplot(
    aes(x = delta_cm,
        y = value,
        color = y_axis)
  ) +
  scale_color_manual(values = c("#EA7580", "#088BBE")) +
  guides(color = "none") +
  geom_line(linewidth = 2, lineend = "round") +
  labs(
    x = expression(paste(delta, " (cm)")),
    y = "height (cm)"
  ) +
  facet_grid(
    y_axis ~ .,
    scales = "free_y",
    switch = "y",
    labeller = label_parsed
  ) +
  theme(strip.placement = "outside",
        axis.title.y = element_blank(),
        strip.text = element_text(size = 40))
gg3

ggsave("plot3.png", plot = gg3, 
       height = 1080, width = 1920, units = "px",
       type = "cairo")

gg_animate3 <- gg3 +
  transition_reveal(delta_cm) +
  view_follow(fixed_y = TRUE,
              fixed_x = FALSE)

# Save animation
animate(
  gg_animate3,
  height = 1080, width = 1920, fps = 30, duration = 10,
  end_pause = 120, rewind = FALSE,
  renderer = gifski_renderer(),
  type = "cairo"
)

anim_save("plot3.gif")

