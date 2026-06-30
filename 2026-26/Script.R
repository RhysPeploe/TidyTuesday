library(tidyverse)
library(ggbeeswarm)
library(janitor)
library(ggiraph)

tuesdata <- tidytuesdayR::tt_load(2026, week = 26)

wreck_inventory <- tuesdata$wreck_inventory

wreck_inventory %>% 
  filter(!is.na(date)) %>% 
  mutate(date = as_date(ymd(date)),
         descriptive_date = as_date(dmy(descriptive_date))) %>% 
  tabyl(classification)

classification_map <- tibble(
  category = c(
    "Naval / Military",
    "Merchant / Cargo",
    "Fishing",
    "Sailing vessels",
    "Small boats",
    "Service / Harbour craft",
    "Aircraft",
    "Other / Unknown"
  ),
  classification = list(
    c(
      "Aircraft Carrier", "Admiralty/ Decoy Ship", "Anti-submarine Drifter",
      "Auxiliary Vessel", "Battleship", "Coast Guard Cruiser", "Corvette",
      "Cruiser", "Destroyer", "Dreadnought", "Fireship", "Frigate",
      "Ironclad Battleship", "Light Ship", "Man O' War", "Mine-sweeper",
      "Privateer", "Royal Ship", "Screw Gunboat", "Submarine", "Tender",
      "Transport", "Tank-landing craft"
    ),
    c(
      "Cargo Ship", "Collier", "Coaster", "East Indiaman", "Iron Steamship",
      "Liner", "Mail boat", "Market boat", "Merchant Vessel", "Motor-ship",
      "Packet boat", "Steam Liner", "Steam Packet", "Steamship", "Steel Steamship",
      "Tanker", "Trader", "West Indiaman", "Flyboat", "Logboat", "Ferry", "Screw Steamer"
    ),
    c(
      "Fishing boat", "Fishing drifter", "Motor Fishing Vessel", "Motor Trawler",
      "Nobby", "Smack", "Steam Drifter", "Steam Trawler", "Trawler",
      "Whale Boat", "Zulu"
    ),
    c(
      "Bark", "Barque", "Barquentine", "Brig", "Brigantine", "Carvel",
      "Clipper", "Clinker", "Cot", "Cutter", "Dandy", "Dutch Galliot",
      "Gabbard", "Gabarts", "Galley", "Galleon", "Galliot", "Glothogue",
      "Hacker", "Hooker", "Jigger", "Ketch", "Lugger", "Lugsail", "Nao",
      "Pink", "Polacca", "Pookaun", "Sailing Boat", "Sailing Ship",
      "Schooner", "Skiff", "Sloop", "Snow", "Spritsail", "Urca", "Wherry",
      "Yawl", "Zabra", "Full-rigged ship", "Motor schooner", "Paddler Steamer"
    ),
    c(
      "Boat", "Canoe", "Curragh", "Lifeboat", "Longboat", "Stone hacker",
      "Motor Boat", "Pleasure Boat", "Punt", "Row boat", "Water Wag", "Pinnace"
    ),
    c(
      "Barge", "Dredger", "Flat", "Hobble", "Hopper Barge", "Hulk",
      "Iron Paddle Steam Tug", "Iron steam tug", "Lighter", "Pilot boat",
      "River Steamer", "Scow", "Steam dredger", "Steam launch", "Steam Tug",
      "Tug", "Wooden Steam Tug", "Yacht", "Iron Steam Yacht", "Smack Yacht",
      "Steam Yacht"
    ),
    c("Airplane"),
    c("Mule", "Other", "Ship", "Unknown", "Vessel", "Crane Vessel")
  )
) %>%
  unnest(classification)

ships_categorised <- wreck_inventory %>% 
  mutate(classification = str_squish(classification)) %>%
  left_join(classification_map, by = "classification") 

plot = ggplot(ships_categorised, aes(x = year, y = category))+  
  geom_rect(mapping = aes(xmin = 1914, xmax = 1918, ymin = 0, ymax = 9),
            colour = "lightblue", fill = alpha("lightblue", 0.7)) +
  geom_rect(mapping = aes(xmin = 1939, xmax = 1945, ymin = 0, ymax = 9),
            colour = "lightblue", fill = alpha("lightblue", 0.7)) +
  geom_point_interactive(
    position = position_quasirandom(),
    mapping = aes(tooltip = wreck_name, data_id = wreck_no),
    alpha = 0.5,
    colour = alpha("#018E42", 0.7),
    fill = "#018E42",
    pch = 21
    ) +
  labs(
    x = "Year of Wreck",
    y = "Category of Wreck",
    title = "Wars resulted in more Military and Merchant Wrecks",
    subtitle = "Though wrecks in other categories did not increase"
  ) +
  theme_minimal() 

girafe(plot)
