library(tidyverse)
library(showtext)
# library(camcorder)
library(ggtext)
# library(nrBrand)
library(glue)
library(ggstream)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load("2025-04-01")
pokemon_df <- tuesdata$pokemon_df

font_add_google("Lacquer", "Lacquer")
showtext_auto()
showtext_opts(dpi = 100)

bg_col <- "#95c1fe"
text_col <- "#1c2c5e"
highlight_col <- "#ffca08"

body_font <- "Lacquer"
title_font <- "Lacquer"

plot_data <- pokemon_df |>
  count(speed, color_1)

title <- ""
st <- "In Pokémon battles, the Pokémon with the highest speed stat acts first, and those with lower speed stats act later. Unlike real-world speed measurements, Pokémon speed is not measured in any specific unit like meters per second or miles per hour. This chart shows the speed of different Pokémon, and their primary colour.<br><br>"

ggplot(
  data = plot_data,
  mapping = aes(x = speed, y = n, fill = color_1)
) +
  geom_stream() +
  # annotations
  annotate(
    "text", x = 140, y = -20, label = str_wrap("Light purple Pokémon tend to have a higher speed than yellow Pokémon.", 15),
    colour = text_col,
    family = body_font
  ) +
  annotate(
    "curve", x = 135, xend = 125, y = -12, yend = 1,
    colour = text_col,
    arrow = arrow(
      length = unit(2.5, "mm"), type = "closed"
    )
  ) +
  annotate(
    "curve", x = 115, xend = 90, y = -20, yend = -23,
    colour = text_col,
    arrow = arrow(
      length = unit(2.5, "mm"), type = "closed"
    ),
    curvature = -0.5
  ) +
  # axis lines
  annotate(
    "text", x = 130, y = -35, label = "Higher speed",
    colour = text_col,
    family = body_font
  ) +
  annotate(
    "segment", x = 150, xend = 175, y = -35, yend = -35,
    colour = text_col,
    arrow = arrow(
      length = unit(2.5, "mm"), type = "open"
    )
  ) +
  annotate(
    "text", x = 55, y = -35, label = "Lower speed",
    colour = text_col,
    family = body_font
  ) +
  annotate(
    "segment", x = 35, xend = 10, y = -35, yend = -35,
    colour = text_col,
    arrow = arrow(
      length = unit(2.5, "mm"), type = "open"
    )
  ) +
  # # add image
  # geom_image(
  #   data = slice_head(plot_data, n = 1),
  #   aes(
  #     x = 145,
  #     y = 29,
  #     image = "2025/2025-04-01/logo.png"
  #   ),
  #   size = 0.4
  # ) +
  # styling
  scale_fill_identity() +
  labs(title = title) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void(base_size = 5, base_family = body_font) +
  theme(
    plot.margin = margin(10, 0, 15, 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 5, r = 10),
      lineheight = 0.3,
      family = title_font,
      face = "bold",
      size = 0.5
    )
  )
