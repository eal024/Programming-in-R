
library(tidyverse)
theme_set(hrbrthemes::theme_ipsum())
# Test Spatial data
library(mapdata)
# library( maps)
library(sf)
library(rnaturalearth)
library(lwgeom)

detach(package:maps)
norway <- st_as_sf(maps::map("world", "norway", plot = FALSE, fill = TRUE))


norway <- st_as_sf(maps::map("world", "norway", plot = FALSE, fill = TRUE))

norway %>%
  ggplot() + 
  geom_sf(fill="black", col=NA) 
