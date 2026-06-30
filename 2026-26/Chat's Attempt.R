library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(patchwork)
library(maps)
library(viridis)

tuesdata <- tidytuesdayR::tt_load(2026, week = 26)

wreck_inventory <- tuesdata$wreck_inventory
# -------------------------------------------------------------------
# 1) Classification lookup
# -------------------------------------------------------------------

classification_map <- tibble::tribble(
  ~category, ~classification,
  "Naval / Military", "Aircraft Carrier",
  "Naval / Military", "Admiralty/ Decoy Ship",
  "Naval / Military", "Anti-submarine Drifter",
  "Naval / Military", "Auxiliary Vessel",
  "Naval / Military", "Battleship",
  "Naval / Military", "Coast Guard Cruiser",
  "Naval / Military", "Corvette",
  "Naval / Military", "Cruiser",
  "Naval / Military", "Destroyer",
  "Naval / Military", "Dreadnought",
  "Naval / Military", "Fireship",
  "Naval / Military", "Frigate",
  "Naval / Military", "Ironclad Battleship",
  "Naval / Military", "Light Ship",
  "Naval / Military", "Man O' War",
  "Naval / Military", "Mine-sweeper",
  "Naval / Military", "Privateer",
  "Naval / Military", "Royal Ship",
  "Naval / Military", "Screw Gunboat",
  "Naval / Military", "Submarine",
  "Naval / Military", "Tender",
  "Naval / Military", "Transport",
  
  "Merchant / Cargo", "Cargo Ship",
  "Merchant / Cargo", "Collier",
  "Merchant / Cargo", "Coaster",
  "Merchant / Cargo", "East Indiaman",
  "Merchant / Cargo", "Iron Steamship",
  "Merchant / Cargo", "Liner",
  "Merchant / Cargo", "Mail boat",
  "Merchant / Cargo", "Market boat",
  "Merchant / Cargo", "Merchant Vessel",
  "Merchant / Cargo", "Motor-ship",
  "Merchant / Cargo", "Packet boat",
  "Merchant / Cargo", "Steam Liner",
  "Merchant / Cargo", "Steam Packet",
  "Merchant / Cargo", "Steamship",
  "Merchant / Cargo", "Steel Steamship",
  "Merchant / Cargo", "Tanker",
  "Merchant / Cargo", "Trader",
  "Merchant / Cargo", "West Indiaman",
  
  "Fishing", "Fishing boat",
  "Fishing", "Fishing drifter",
  "Fishing", "Motor Fishing Vessel",
  "Fishing", "Motor Trawler",
  "Fishing", "Nobby",
  "Fishing", "Smack",
  "Fishing", "Steam Drifter",
  "Fishing", "Steam Trawler",
  "Fishing", "Trawler",
  "Fishing", "Whale Boat",
  "Fishing", "Zulu",
  
  "Sailing vessels", "Bark",
  "Sailing vessels", "Barque",
  "Sailing vessels", "Barquentine",
  "Sailing vessels", "Brig",
  "Sailing vessels", "Brigantine",
  "Sailing vessels", "Carvel",
  "Sailing vessels", "Clipper",
  "Sailing vessels", "Clinker",
  "Sailing vessels", "Cot",
  "Sailing vessels", "Cutter",
  "Sailing vessels", "Dandy",
  "Sailing vessels", "Dutch Galliot",
  "Sailing vessels", "Gabbard",
  "Sailing vessels", "Gabarts",
  "Sailing vessels", "Galley",
  "Sailing vessels", "Galleon",
  "Sailing vessels", "Galliot",
  "Sailing vessels", "Glothogue",
  "Sailing vessels", "Hacker",
  "Sailing vessels", "Hooker",
  "Sailing vessels", "Jigger",
  "Sailing vessels", "Ketch",
  "Sailing vessels", "Lugger",
  "Sailing vessels", "Lugsail",
  "Sailing vessels", "Nao",
  "Sailing vessels", "Pink",
  "Sailing vessels", "Polacca",
  "Sailing vessels", "Pookaun",
  "Sailing vessels", "Sailing Boat",
  "Sailing vessels", "Sailing Ship",
  "Sailing vessels", "Schooner",
  "Sailing vessels", "Skiff",
  "Sailing vessels", "Sloop",
  "Sailing vessels", "Snow",
  "Sailing vessels", "Spritsail",
  "Sailing vessels", "Urca",
  "Sailing vessels", "Wherry",
  "Sailing vessels", "Yawl",
  "Sailing vessels", "Zabra",
  
  "Small boats", "Boat",
  "Small boats", "Canoe",
  "Small boats", "Curragh",
  "Small boats", "Ferry",
  "Small boats", "Lifeboat",
  "Small boats", "Longboat",
  "Small boats", "Motor Boat",
  "Small boats", "Pleasure Boat",
  "Small boats", "Punt",
  "Small boats", "Row boat",
  "Small boats", "Water Wag",
  
  "Service / Harbour craft", "Barge",
  "Service / Harbour craft", "Dredger",
  "Service / Harbour craft", "Flat",
  "Service / Harbour craft", "Hobble",
  "Service / Harbour craft", "Hopper Barge",
  "Service / Harbour craft", "Hulk",
  "Service / Harbour craft", "Iron Paddle Steam Tug",
  "Service / Harbour craft", "Iron steam tug",
  "Service / Harbour craft", "Lighter",
  "Service / Harbour craft", "Pilot boat",
  "Service / Harbour craft", "River Steamer",
  "Service / Harbour craft", "Scow",
  "Service / Harbour craft", "Steam dredger",
  "Service / Harbour craft", "Steam launch",
  "Service / Harbour craft", "Steam Tug",
  "Service / Harbour craft", "Tug",
  "Service / Harbour craft", "Wooden Steam Tug",
  "Service / Harbour craft", "Yacht",
  "Service / Harbour craft", "Iron Steam Yacht",
  "Service / Harbour craft", "Smack Yacht",
  "Service / Harbour craft", "Steam Yacht",
  
  "Aircraft", "Airplane",
  
  "Other / Unknown", "Mule",
  "Other / Unknown", "Other",
  "Other / Unknown", "Ship",
  "Other / Unknown", "Unknown",
  "Other / Unknown", "Vessel"
)

# -------------------------------------------------------------------
# 2) Clean and categorise your data
#    Assumes your dataset is called `ships`
#    and has at least:
#      - classification
#      - year
#      - latitude
#      - longitude
# -------------------------------------------------------------------

ships_clean <- wreck_inventory %>%
  mutate(
    classification = str_squish(classification),
    year = as.integer(year)
  ) %>%
  left_join(classification_map, by = "classification") %>%
  mutate(
    category = coalesce(category, "Other / Unknown")
  )

# -------------------------------------------------------------------
# 3) Plot 1: composition over time
#    (stacked area / streamgraph-style)
# -------------------------------------------------------------------

year_cat <- ships_clean %>%
  filter(!is.na(year)) %>%
  count(year, category, name = "n") %>%
  group_by(year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(category = fct_relevel(category,
                                "Sailing vessels",
                                "Merchant / Cargo",
                                "Fishing",
                                "Service / Harbour craft",
                                "Naval / Military",
                                "Small boats",
                                "Aircraft",
                                "Other / Unknown"))

p1 <- ggplot(year_cat, aes(x = year, y = prop, fill = category)) +
  geom_area(alpha = 0.95, colour = NA) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "C", end = 0.95) +
  labs(
    title = "Changing ship categories over time",
    x = NULL,
    y = "Share of records",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# -------------------------------------------------------------------
# 4) Plot 2: world map of wreck locations
# -------------------------------------------------------------------

world_map <- map_data("world")

map_df <- ships_clean %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    longitude >= -180, longitude <= 180,
    latitude >= -90, latitude <= 90
  ) %>%
  mutate(category = fct_lump_n(category, n = 6))

p2 <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    colour = "grey80",
    linewidth = 0.2
  ) +
  geom_point(
    data = map_df,
    aes(x = longitude, y = latitude, colour = category),
    alpha = 0.65,
    size = 0.9
  ) +
  coord_quickmap() +
  scale_colour_viridis_d(option = "C", end = 0.95) +
  labs(
    title = "Where the ship records are located",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )

# -------------------------------------------------------------------
# 5) Plot 3: top classifications lollipop
# -------------------------------------------------------------------

top_class <- ships_clean %>%
  count(classification, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(classification = fct_reorder(classification, n))

p3 <- ggplot(top_class, aes(x = n, y = classification)) +
  geom_segment(aes(x = 0, xend = n, yend = classification),
               linewidth = 1, colour = "grey75") +
  geom_point(size = 3) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Most common ship classifications",
    x = "Count",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

# -------------------------------------------------------------------
# 6) Combine into one TidyTuesday entry
# -------------------------------------------------------------------

final_plot <-
  (p1 / p2) | p3 +
  plot_annotation(
    title = "Ship classifications through time, place, and frequency",
    subtitle = "A three-part TidyTuesday composition"
  ) &
  theme(
    plot.title = element_text(face = "bold")
  )

final_plot
