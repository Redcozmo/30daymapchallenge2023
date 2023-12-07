# 30 day Map Challenge
# Year : 2023
# Day 1 : Points
#
# Subject : trees in Lyon, France
# --------------------------------- #

# Data source
# Trees :
# https://data.grandlyon.com/portail/fr/jeux-de-donnees/arbres-alignement-metropole-lyon/telechargements
# Territory boundaries
# https://data.grandlyon.com/portail/fr/jeux-de-donnees/territoire-metropole-lyon/info

# Clear existing objects from workspace and free memory
rm(list=ls(all=TRUE));gc()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load libraries
library(ggplot2)
library(sf)
library(dplyr)
library(osmdata) 
library(geojsonio)
library(MetBrewer)
library(showtext) # fonts
library(forcats) # factor
library(ggpmisc) # table on ggplot
library(ggspatial) # scale bars and north arrows

# Get input data : boundaries
boundary <- st_read("../data/day1/LyonMetropole_boundaries/adr_voie_lieu_adrmetropole.shp")
boundary_4326 <- st_transform(boundary, 4326) # Transform to OSM CRS : EPSG 4326
bbox_4326 <- sf::st_bbox(boundary_4326)

# Get input data : trees
trees <- geojson_read("../data/day1/MetropoleLyon_ArbresAlignement.json",  what = "sp")
trees_df <- as.data.frame(trees)
trees_df <- trees_df %>%
  select(genre, espece, essencefrancais, coords.x1, coords.x2) %>%
  rename(long = coords.x1, lat = coords.x2)

# Filter out trees with bbox_4326
trees_df <- filter(trees_df, long > bbox_4326[1] & long < bbox_4326[3] &
                     lat > bbox_4326[2] & lat < bbox_4326[4])

# Filter out trees with bad values
trees_df <- filter(trees_df, (essencefrancais != "Emplacement libre") &
                     (essencefrancais != "Souche"))

# Summarize by genus and 5 most present species
trees_genus_sum <- trees_df %>%
  group_by(genre) %>%
  summarise(n_trees_by_genus = n()) %>%
  mutate(per_by_genus = 100 * n_trees_by_genus / sum(n_trees_by_genus)) %>%
  mutate(across(.cols = matches('per_by_genus'), .fns = ~ round(as.integer(.x)))) %>%
  arrange(desc(per_by_genus)) %>%
  slice_max(n_trees_by_genus, n = 5)

genus_5 <- merge(trees_df, trees_genus_sum, by = c('genre'))

trees_species_sum <- trees_df %>%
  group_by(essencefrancais) %>%
  summarise(n_trees_by_species = n()) %>%
  mutate(per_by_species = 100 * n_trees_by_species / sum(n_trees_by_species)) %>%
  mutate(across(.cols = matches('per_by_species'), .fns = ~ round(as.integer(.x)))) %>%
  arrange(desc(per_by_species)) %>%
  slice_max(n_trees_by_species, n = 5)

species_5 <- merge(trees_df, trees_species_sum, by = c('essencefrancais'))

# Factor 
species_5 <- species_5 %>%
  mutate(essencefrancais = factor(essencefrancais, levels = unique(.$essencefrancais))) %>%
  mutate(essencefrancais = forcats::fct_reorder(essencefrancais, desc(n_trees_by_species)))
genus_5 <- genus_5 %>%
  mutate(genre = factor(genre, levels = unique(.$genre))) %>%
  mutate(genre = forcats::fct_reorder(genre, desc(n_trees_by_genus)))

# Construct custom labels for plot
first_species <- function(x) { # Get most present species by genus
  result <- genus_5 %>%
    filter(genre == x) %>%
    group_by(essencefrancais) %>%
    summarise(n_trees = n()) %>%
    slice_max(n_trees, n = 1)
  return(result[[1]])
}
first_species_by_genus <- tibble(genre = trees_genus_sum$genre,
                                 first_species = unlist(lapply(trees_genus_sum$genre, first_species)))

custom_labels_table <- trees_genus_sum %>%
  select(c(1,3)) %>%
  rename(per = per_by_genus) %>%
  merge(first_species_by_genus, by = "genre") %>%
  mutate(labels = paste(genre, paste('(', per, '%) - ', first_species, sep = ''))) %>%
  arrange(desc(per))

# Get input data : OSM data
roads <-
  opq(bbox_4326, timeout = 120, memsize = 1000000000) %>%
  add_osm_feature(key = 'highway', value = c('motorway', 'trunk', 'primary', 'secondary')) %>%
  osmdata_sf()

rivers <-
  opq(bbox_4326, timeout = 120, memsize = 1000000000) %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()

# Filter out trees with bbox_4326
roads_subset <- roads$osm_lines %>%
  st_intersection(boundary_4326)

rivers_subset <- rivers$osm_lines %>%
  st_intersection(boundary_4326)

# Plot options
color_list <- c('darkgreen', 'chartreuse2', 'darkgoldenrod1', 
                  'antiquewhite', 'coral4')

# Plot
ggplot() +
  geom_point(data = genus_5, aes(x = long, y = lat, color = genre), 
             size = .02, alpha = .95) +
  geom_sf(data = roads_subset, col = 'grey70', size = .1, alpha = .2) +
  geom_sf(data = rivers_subset, col = 'deepskyblue', size = .1, alpha = .4) +
  theme_void() +
  facet_wrap(~ genre, ncol=3) +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  # scale_color_manual(values = met.brewer('Hokusai1', 10)) +
  scale_color_manual(values = color_list,
                     labels = custom_labels_table$labels) +
  labs(title = "Arbres d'alignement de Lyon",
       subtitle = '5 principaux genres représentés',
       color = 'Genre (% total) - Espèce la plus représentée',
       caption = 'Trees data from data.grandlyon.com | Map data from OpenStreetMap.') +
  theme(plot.background = element_rect(fill = 'grey20', color=NA),
        panel.background = element_rect(fill = 'grey20', color=NA),
        plot.margin = unit(c(t=0,r=.2,b=0,l=.7), 'cm'),
        legend.position = 'right',
        # legend.title = element_blank(),
        legend.title = element_text(color = 'coral', size = 25,
                                    family = 'mono'),
        legend.text = element_text(family = 'mono', color = 'grey80',
                                   size = 25),
        plot.caption = element_text(color = 'grey80', size = 25,
                                    hjust = 0.5, vjust = .6,
                                    family = 'mono'),
        plot.title = element_text(color = 'coral', size = 70,
                                  hjust = 0.14, vjust = .6,
                                  family = 'mono'),
        plot.subtitle = element_text(color = 'coral', size = 35,
                                     hjust = 0.14, vjust = .6,
                                     family = 'mono')) +
  # annotate(geom = "table", x = 9, y = 3, label = list(species_5_table)) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave('../maps/day1_points.png', height = 6.5, width = 8)
