library(ggplot2)
library(rnaturalearth)

# Create a ggplot object with the land and ocean mask
quick_map <- function(data, lon, lat){
  ## if data not exist, then download
  if (!file.exists("code/world_map.rds")) {
    world <- ne_download(scale = "small", type = "land", category = "physical", returnclass = "sf")
    saveRDS(world, "world_map.rds")
  } else {
    world <- readRDS("code/world_map.rds")
  }
  
  p <- ggplot() +
    geom_sf(data = world, color = "black") +
    geom_point(data = data, aes(x = !!sym(lon), y = !!sym(lat)), color = "red", size = 2)
  
  return(p)
}

## example
read_csv("fg/lgm_margo_fg_r.csv") %>% quick_map('Longitude', 'Latitude')
## read_csv("raw/CLIMAP/CLIMAP_FORMATTED.csv") %>% quick_map('Longitude', 'Latitude')
