# 30 day Map Challenge
# Year : 2023
# Day 2 : French rivers
#
# Subject : trees in Lyon, France
# --------------------------------- #

# Inspired from
# https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/

# Data source
# "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"

# Clear existing objects from workspace and free memory
rm(list=ls(all=TRUE));gc()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load libraries
library(sf)
library(tidyverse)
library(ggplot2)

# Load Rdata
datafilename <- "../Rdata/day2_lines.Rdata"
load(file = datafilename)

# Data source
filename <- "../data/day2/eu_rivers/HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu.shp"

# Load river data 
eu_riv <- sf::st_read(filename)

# Transform LINESTRING to MULTILINESTRING
eu_riv <- eu_riv %>%
  sf::st_cast("MULTILINESTRING") %>%
  st_as_sf()

# Add column with class based on ORD_FLOW
eu_riv_sel <- eu_riv %>%
  dplyr::select(ORD_FLOW, MAIN_RIV, ORD_CLAS, ORD_STRA, HYBAS_L12)

eu_riv_sel <- eu_riv_sel %>%
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 3 ~ 1,
      width == 4 ~ 0.8,
      width == 5 ~ 0.6,
      width == 6 ~ 0.4,
      width == 7 ~ 0.2,
      width == 8 ~ 0.2,
      width == 9 ~ 0.1,
      width == 10 ~ 0.1,
      TRUE ~ 0
    )
  )

# Construct bbox for plotting
french_region_boundary_file <- "../data/day2/regions-20180101-shp"
french_region_boundary <- st_read(french_region_boundary_file)
french_region_boundary <- french_region_boundary %>% 
  select(code_insee) %>%
  mutate(code_insee = as.numeric(code_insee)) %>%
  filter(code_insee > 9)
bbox <- st_bbox(french_region_boundary)

# Get cities
cities_file <- "../data/day2/french_cities.gpkg"
cities <- st_read(cities_file)
cities <- cities %>% 
  select(geom)
cities <- st_intersection(cities, french_region_boundary)

# Map parameters
scale_alpha_manual_values <- c(
  "3" = 1, "4" = 1, "5" = .7, "6" = .6,
  "7" = .4, "8" = .3, "9" = .2, "10" = .1)

scale_color_manual_values <- c(
  "#08306b", "#08519c", "#2171b5",
  "#4292c6", "#6baed6", "#9ecae1",
  "#c6dbef", "#deebf7")

# Map
p <-
  ggplot() +
  geom_sf(
    data = eu_riv_sel,
    aes(color = factor(ORD_FLOW),
        size = width,
        alpha = factor(ORD_FLOW))) +
  geom_sf(data = cities, color = '#5f7c45', fill = '#5f7c45') +
  coord_sf(crs = 4326,
           xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])
           ) +
  labs(title = "Fleuves et rivières en France",
       subtitle = "Rendu du trait basé sur le débit",
       caption = "30daymapchallenge2023 | rivers data from data.hydrosheds.org"
  ) +
  scale_size(range = c(0, .3)) +
  scale_color_manual(values = scale_color_manual_values) +
  scale_alpha_manual(values = scale_alpha_manual_values) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      size = 28, color = "#2171b5", hjust = 0.5, vjust = 0),
    plot.subtitle = element_text(
      size = 14, color = "#4292c6", hjust = 0.5, vjust = 0.5),
    plot.caption = element_text(
      size = 10, color = "grey60", hjust = 0.5, vjust = 0.5),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines")
  )

# Map exporting
ggsave(
  filename = "../maps/day2_lines.png",
  plot = p,
  width = 8, height = 6.5, dpi = 600,
  device = "png", bg = "white"
)

# Save Rdata
rm(list = c('eu_riv'))
save.image(file = datafilename)
