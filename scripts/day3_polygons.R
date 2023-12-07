# 30 day Map Challenge
# Year : 2023
# Day 3 : Polygons
#
# Subject : French Town Extents
# --------------------------------- #

# Data source
# contours de métropoles : 
# https://geoservices.ign.fr/adminexpress#telechargement
# fleuves et rivières sur GeoDataGouv (ancienne plateforme) : 
# https://geo.data.gouv.fr/fr/datasets/c14687b6e5b7294e116ddb764f6d5f6f233ba108

# Clear existing objects from workspace and free memory
rm(list=ls(all=TRUE));gc()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load libraries
library(ggplot2)
library(sf)
library(dplyr)
library(stringr) # for str_detect function
library(osmdata)
library(tmap)
library(lwgeom) # boundingcircle function
library(cowplot)

# Load Rdata
datafilename <- "../Rdata/day3_polygons.Rdata"
load(file = datafilename)

# Define metropoles names
metropoles_names <- c('Bordeaux', 'Brest', 'Clermont', 'Dijon', 'Grenoble', 
                'Lille', 'Metz', 'Montpellier', 'Nancy', 'Nantes', 'Nice', 
                'Orléans', 'Rennes', 'Rouen', 'Saint-Etienne', 'Strasbourg', 
                'Toulon', 'Toulouse', 'Tours', 'Aix-Marseille', 'Paris', 
                'Lyon')

# Get metropoles boundaries
boundaries_file <- "../data/day3/admin/EPCI.shp"
metropoles <- sf::st_read(boundaries_file)
st_crs(metropoles)

metropoles <- metropoles %>%
  filter(NATURE == 'Métropole')

# Get rivers with OSM data
get_rivers <- function(x) {
  print('--------------------------------------')
  print(paste('Function get_rivers with : ', x))
  city <- metropoles %>% 
    filter(str_detect(NOM, x)) %>%
    st_as_sf() %>%
    st_transform(4326)
  
  print(paste('sf object create with ID : ', city$ID))

  print('Requete osm...')
  rivers <-
    opq(st_bbox(city), timeout = 120, memsize = 1000000000) %>%
    add_osm_feature(key = 'waterway', value = 'river') %>%
    osmdata_sf()

  print(paste('OSM data get at : ', rivers$meta$timestamp))

  print('Trim rivers data...')
  rivers_trim <- rivers$osm_lines %>%
    st_intersection(st_minimum_bounding_circle(city))

  return(rivers_trim)
}
  
# Get rivers for all metropoles
rivers_list <- lapply(metropoles_names, get_rivers)
rivers <- dplyr::bind_rows(rivers_list)

# Save rivers in a shapefile
rivers %>%
  sf::st_cast("MULTILINESTRING") %>%
  select('osm_id', 'name', 'NOM', 'NATURE') %>%
  st_as_sf() %>%
  st_write(dsn = "../data/day3/day3_rivers.shp", append = FALSE)

# Get rivers from datagouv
hydro_file <- "../data/day3/TronconHydrograElt_FXX.shp"
hydro <- st_read(hydro_file)
st_crs(hydro)
unique(hydro$Largeur)

hydro <- hydro %>% 
  select(Largeur) %>%
  filter(Largeur %in% c("De 0 à 15 mètres", "Entre 15 et 50 mètres", "Plus de 50 mètres")) %>%
  mutate(width = dplyr::case_when(
    # Largeur == "Entre 15 et 50 mètres" ~ 1,
    Largeur == "Entre 15 et 50 mètres" ~ 2,
    Largeur == "Plus de 50 mètres" ~ 3))

# hydro_trim_circle <- hydro %>%
#   st_intersection(st_minimum_bounding_circle(metropoles))

hydro_trim_poly <- hydro %>%
  st_intersection(metropoles)

# Get ordered names for map
city <- metropoles %>% filter(str_detect(NOM, x))
metropoles$NOM
metropoles_order_names <- c('Strasbourg', 'Toulon')

# Maps with tmap
tmap_mode("plot") # tmap_mode("view") to have interactive map
tm <- 
  tm_shape(metropoles %>% slice_max(CODE_SIREN, n = 2)) +
  # tm_shape(metropoles %>% filter(NOM == "Métropole de Lyon")) +
    tm_polygons(border.col = 'coral4', col = 'coral4', alpha = 0.5) +
    tm_facets('NOM', ncol = 2) +
  # tm_shape(rivers) +
  #   tm_lines(col = '#0072B2', lwd = 2, alpha = 0.6) +
  tm_shape(hydro_trim_poly) +
    tm_lines(col = '#0072B2', 
             # col.scale = tm_scale_categorical(c("#08306b", "#9ecae1")),
             # legend.col.show = FALSE,
             lwd = 3,
             # lwd.scale = tm_scale_categorical(c(1, 3)),
             # legend.lwd.show = FALSE,
             alpha = 0.6) +
  tm_layout(main.title = "Métropoles françaises",
            main.title.size = 2,
            main.title.color = 'brown',
            main.title.position = "center",
            bg.color  = 'coral1',
            panel.show = TRUE,
            panel.labels = metropoles_order_names,
            panel.label.bg.color = 'coral1',
            panel.label.color = 'brown3',
            panel.label.size = 1,
            frame.lwd = NA,
            frame = FALSE,
            attr.outside = FALSE,
            attr.position = c("center", "BOTTOM")) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1);tm
  
tmap_arrange(tm,
  tm_compass(position = c("right", "bottom")),
  tm_credits(text = "30daymapchallenge2023 | data from geoservices.ign.fr & OpenStreetMap.",
                size = 1.5, col = 'brown')
)
  

# Save map
tmap_save(tm, '../maps/day3_polygons.png', 
          width = 8, height = 7, dpi = 600)

# Save Rdata
save.image(file = datafilename)

# ******************************************************************************
# Tentative d'une autre méthode pour avoir une échelle commune et que 
# chaque metropole est sa taille réelle par rapport aux autres

# Get cities max extent
max_extent <- circles %>% 
  mutate(area = st_area(circles)) %>%
  arrange(desc(area)) %>%
  slice_max(area, n = 1)
bbox <- st_bbox(max_extent)
bbox_size <- as.vector(bbox$xmax - bbox$xmin)

# Get same size bbox for each city with its center
get_bbox <- function(city, BBOX_SIZE = bbox_size) {
  center <- st_centroid(city)
  bbox <- st_bbox(c(xmin = unname(st_coordinates(center)[1, 'X']) - BBOX_SIZE/2, 
                    xmax = unname(st_coordinates(center)[1, 'X']) + BBOX_SIZE/2, 
                    ymin = unname(st_coordinates(center)[1, 'Y']) - BBOX_SIZE/2, 
                    ymax = unname(st_coordinates(center)[1, 'Y']) + BBOX_SIZE/2), 
                  crs = st_crs(city))
  return(bbox) 
}

# Get map of one city
map_one_city <- function(city, rivers_of_city, bbox) {
  tm <- tm_shape(city, bbox = bbox) +
    tm_polygons(border.col = 'coral4', col = 'coral4', alpha = 1) +
    tm_shape(rivers_of_city) +
      tm_lines(col = '#0072B2', lwd = 2, alpha = 0.6) +
    tm_layout(bg.color  = 'coral1',
              frame = FALSE,
              attr.position = c("center", "BOTTOM"))
  return(tm)
}

# Get map of all cities
map_all_cities <- function(x) {
  city <- metropoles %>% filter(str_detect(NOM, x))
  rivers_of_city <- st_intersection(hydro, city)
  bbox <- get_bbox(city, BBOX_SIZE = bbox_size)
  tm <- map_one_city(city, rivers_of_city, bbox)
  return(tm)
}

# Map one city
x <- metropoles_names[20]
city <- metropoles %>% filter(str_detect(NOM, x))
rivers_of_city <- st_intersection(hydro, city)
bbox <- get_bbox(city, BBOX_SIZE = bbox_size)
tm <- map_one_city(city, rivers_of_city, bbox)
tm

# Map all cities
maps_list <- lapply(metropoles_names, map_all_cities)
tmap_arrange(maps_list)
