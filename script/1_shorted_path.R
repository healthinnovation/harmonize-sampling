library(qgisprocess)
library(sf)
library(tidyverse)
library(purrr)

# Reading spatial data (input)
network <- st_read("../raw_data/iquitos-nauta.gpkg")
villages <- cp <- read_csv("../processed_data/db_variables.csv")
input <- villages %>% 
  mutate(
    START_POINT = sprintf("%s,%s [EPSG:4326]",lon,lat),
    END_POINT = sprintf("%s,%s [EPSG:4326]",-73.244137,-3.748085)
    ) 

# Exploring qgisprocess
qgis_algorithms() %>% View()
qgis_show_help(algorithm = "native:shortestpathpointtopoint")
shortest_path <- qgis_function(algorithm = "native:shortestpathpointtopoint")

# Preparing function for loop or apply family

distance_by_road <- function(x){
  # Measure river distance 
  longitude_road <- shortest_path(
    INPUT = network, 
    STRATEGY = 0,
    DEFAULT_DIRECTION = 2,
    DEFAULT_SPEED = 50,
    TOLERANCE = 0,
    START_POINT = input[["START_POINT"]][x],
    END_POINT = input[["END_POINT"]][x],
    OUTPUT = qgis_tmp_vector(),
    PROJECT_PATH = "qproject.qgz" # A project of qgis
  ) %>% 
    qgis_output('OUTPUT') %>% 
    st_read()
  # Save shorted path
  write_sf(longitude_road,paste0("../processed_data/",input[["codcp"]][x],".gpkg"))
  # Calculate longitude of river distance 
  longitude_road$distance_m <- st_length(longitude_road) %>%
    as.vector()
  newtable <- st_drop_geometry(longitude_road) %>% 
    mutate(id_computate = input[["codcp"]][x])
}

lista_distancias <- map(
  1:nrow(input),
  possibly(distance_by_road, NA)
)

distance <- lista_distancias %>% map_df(.f = as.data.frame) %>% 
  drop_na(distance_m) %>% 
  select(distance_m,id_computate)

villages <- villages %>% 
  left_join(y = distance,by = c("codcp"="id_computate"))
# Calculate the distance by qgis 
db_distance <- villages %>% 
  drop_na(distance_m)
db_error <- st_read("../processed_data/error.gpkg") %>%
  st_set_geometry(NULL) %>% 
  mutate(distance_m = as.double(distance_m))
db_final <- bind_rows(db_distance,db_error)
# Save shorted path 
write_csv(db_final,"../processed_data/db_variables_v3.csv")