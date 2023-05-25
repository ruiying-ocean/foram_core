#--------------------
# merge all absolute abundance (LGM)
#--------------------

filenames <- list.files("fg", pattern="^lgm.*_a\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Spinose', 'Symbiosis','Absolute Abundance'))
  return(df)
})
ldf %>% rbindlist() %>% fwrite("tidy/lgm_absolute_abundance.csv")

#--------------------
# merge all relative abundance (LGM)
#--------------------

filenames <- list.files("fg", pattern="^lgm.*_r\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Spinose', 'Symbiosis','Relative Abundance'))
  return(df)
})
ldf %>% rbindlist() %>% fwrite("tidy/lgm_relative_abundance.csv")

#--------------------
# Plot example
#--------------------
library(sf)
library(tmap)
land <- read_sf("tidy/ne_50m_land/ne_50m_land.shp")
p_land <- tm_shape(land)+ tm_polygons()

df <- ldf %>% rbindlist() %>% dplyr::filter(Spinose=="Yes" & Symbiosis=="Yes" & `Relative Abundance` <= 1) 
df <- df %>%  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) #WGS84
p_site <- tm_shape(df) + tm_symbols(col="Relative Abundance",size=0.5)

p_land + p_site
