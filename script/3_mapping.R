library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)

data <- read_csv("../processed_data/villages_selected_sample_v2.csv") %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

index <- data %>% 
  ggplot() +  
  annotation_map_tile(type = "osm",) + 
  geom_sf(aes(color = class), size = 6, alpha= 0.7) + 
  scale_color_viridis_d("PCA index",option = "turbo")

ggsave(
  filename = "../graphics/panel_v2.png",
  width = 8,
  height = 10,
  dpi = 300
  )
