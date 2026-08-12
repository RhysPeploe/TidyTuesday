library(tidyverse)
library(janitor)
library(scales)

tuesdata <- tidytuesdayR::tt_load(2026, week = 27)

ufc_athletes <- tuesdata$ufc_athletes

ufc_records = ufc_athletes |> 
  filter(wins + draws + losses > 0) |> 
  select(name, wins, draws, losses,
         weight_class, status) |> 
  mutate(record = case_when(
    losses == 0 ~ "Undefeated record",
    (wins > losses) & losses > 0 ~ "Winning record",
    losses > wins ~ "Losing record",
    wins == losses ~ "Equal record",
    TRUE ~ "999"
  ),
  record = factor(record, levels = c("Undefeated record", "Winning record",
                                     "Losing record", "Equal record")),
  weight_class = str_replace_all(weight_class, "\\'", ""),
  weight_class = factor(weight_class, levels = c(
    'Flyweight', 'Bantamweight', 'Featherweight', 'Lightweight',
    'Welterweight', 'Middleweight', 'Light Heavyweight', 'Heavyweight',
    'Womens Strawweight', 'Womens Flyweight', 'Womens Bantamweight',
    'Womens Featherweight', NA
  ))) |> 
  group_by(weight_class, record) |> 
  arrange(weight_class, record, desc(wins), losses, desc(draws)) |> 
  mutate(class_record_n = n(),
         rank = row_number()) |> 
  ungroup() |> 
  group_by(weight_class) |> 
  mutate(weight_n = n()) |> 
  ungroup() |> 
  mutate(class_record_percent = class_record_n/weight_n,
         class_record_percent = if_else(record %in% c("Undefeated record", "Winning record"),
                                        class_record_percent,
                                        -class_record_percent),
         x_pt = (class_record_percent / class_record_n) * (class_record_n - (rank - 1)))

ufc_weights = ufc_records |> 
  select(weight_class, record, class_record_percent, class_record_n) |> 
  unique()

rank_plot = ufc_records |> left_join(ufc_weights |> pivot_wider(id_cols = weight_class, names_from = record, values_from = class_record_percent)) |> 
  mutate(y_pt = case_when(
    record == "Undefeated record" ~ x_pt + `Winning record`,
    record == "Winning record" ~ x_pt,
    record == "Equal record" ~ x_pt,
    record == "Losing record" ~ x_pt - `Equal record`,
    record == "Losing record" & is.na(weight_class) ~ x_pt,
    TRUE ~ NA
  ))

ggplot(ufc_weights, aes(x = weight_class, class_record_percent, fill = record)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal()

ggplot(ufc_weights, aes(x = weight_class, y = class_record_percent, 
                        fill = record)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(
    breaks = c("Losing record", "Equal record", "Winning record", "Undefeated record"),
    palette = "Set1"
  ) +
  geom_text(aes(label = str_c(abs(round(class_record_percent,2)*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            data = ufc_weights |>  filter(abs(class_record_percent) >= 0.02)) +
  # scale_x_discrete() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  labs(x = "Weight Class",
       title = "Most UFC fighters have winning records",
       fill = NULL) +
  geom_point(mapping = aes(y = y_pt),
             data = rank_plot,
             alpha = 0.1,
             show.legend = FALSE)
ggsave("diverging_bar_with_points.tiff", width = 14.7, height = 5.7)

ggplot(ufc_weights, aes(x = weight_class, y = class_record_percent, 
                        fill = record)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(
    breaks = c("Losing record", "Equal record", "Winning record", "Undefeated record"),
    palette = "Set1"
  ) +
  geom_text(aes(label = str_c(abs(round(class_record_percent,2)*100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            data = ufc_weights |>  filter(abs(class_record_percent) >= 0.02)) +
  # scale_x_discrete() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  labs(x = "Weight Class",
       title = "Most UFC fighters have winning records",
       fill = NULL) 
ggsave("diverging_bar.tiff", width = 14.7, height = 5.7)
