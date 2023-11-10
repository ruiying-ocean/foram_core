library(ggplot2)
library(rnaturalearth)

# Create a ggplot object with the land and ocean mask
quick_map <- function(data, lon, lat){
  ## if data not exist, then download
  if (!file.exists("world_map.rds")) {
    world <- ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf")
    saveRDS(world, "world_map.rds")
  } else {
    world <- readRDS("world_map.rds")
  }
  
  p <- ggplot() +
    geom_sf(data = world, color = "black") +
    geom_point(data = data, aes(x = !!sym(lon), y = !!sym(lat)), color = "red", size = 2)
  
  return(p)
}

## example
quick_map(climap_site, 'Long', 'Lat')
read_csv("raw/CLIMAP/formatted_site_info.csv") %>% quick_map('Longitude', 'Latitude')
## read_csv("raw/CLIMAP/CLIMAP_FORMATTED.csv") %>% quick_map('Longitude', 'Latitude')
