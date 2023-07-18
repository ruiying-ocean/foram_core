#---------------------------------------------------
# merge all relative abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern="^lgm.*_r\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Species', 'Relative Abundance'))
  return(df)
})

wdf <- ldf %>% rbindlist() %>% pivot_wider(id_cols = c("Latitude", "Longitude"),
                           values_from = "Relative Abundance",
                           names_from = "Species", 
                           values_fill = 0,
                           values_fn=mean)

wdf %>% fwrite("tidy/lgm_sp_r_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern="^lgm.*_a\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Species', 'Absolute Abundance'))
  return(df)
})

wdf <- ldf %>%  rbindlist() %>%pivot_wider(id_cols = c("Latitude", "Longitude"),
                           values_from = "Absolute Abundance",
                           names_from = "Species", 
                           values_fill = 0,
                           values_fn=mean)

wdf %>% fwrite("tidy/lgm_sp_a_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Functional Group) 
#---------------------------------------------------

filenames <- list.files("fg", pattern="^lgm.*_a\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Spinose', 'Symbiosis','Absolute Abundance'))
  return(df)
})

ldf <- ldf %>% rbindlist() %>% mutate(`Functional Group` = case_when(
  Symbiosis == "No" & Spinose=="No" ~ "Symbiont-barren Non-Spinose",
  Symbiosis == "No" & Spinose=="Yes" ~ "Symbiont-barren Spinose",
  Symbiosis == "Yes" & Spinose=="No" ~ "Symbiont-facultative Non-Spinose",
  Symbiosis == "Yes" & Spinose=="Yes" ~ "Symbiont-obligate Spinose"))

wdf <- ldf %>% pivot_wider(id_cols = c("Latitude", "Longitude"),
                           values_from = "Absolute Abundance",
                           names_from = "Functional Group", 
                           values_fill = 0,
                           values_fn=mean)

wdf %>% fwrite("tidy/lgm_fg_a_tidy.csv")

#---------------------------------------------------
# merge all relative abundance (LGM Functional Group)
#---------------------------------------------------

filenames <- list.files("fg", pattern="^lgm.*_r\\.csv$", full.names=TRUE)
ldf <- lapply(filenames, function(df){
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", 'Longitude', 'Spinose', 'Symbiosis','Relative Abundance'))
  return(df)
})

ldf <- ldf %>% rbindlist() %>% mutate(`Functional Group` = case_when(
  Symbiosis == "No" & Spinose=="No" ~ "Symbiont-barren Non-Spinose",
  Symbiosis == "No" & Spinose=="Yes" ~ "Symbiont-barren Spinose",
  Symbiosis == "Yes" & Spinose=="No" ~ "Symbiont-facultative Non-Spinose",
  Symbiosis == "Yes" & Spinose=="Yes" ~ "Symbiont-obligate Spinose"))

wdf <- ldf %>% pivot_wider(id_cols = c("Latitude", "Longitude"),
                           values_from = "Relative Abundance",
                           names_from = "Functional Group", 
                           values_fill = 0,
                           values_fn=mean)

wdf %>% fwrite("tidy/lgm_fg_r_tidy.csv")

#--------------------
# Modern fg relative/absolute abundance
#--------------------
pi_a <- read_csv("fg/forcens_fg_a.csv")%>% mutate(`Functional Group` = case_when(
  Symbiosis == "No" & Spinose=="No" ~ "Symbiont-barren Non-Spinose",
  Symbiosis == "No" & Spinose=="Yes" ~ "Symbiont-barren Spinose",
  Symbiosis == "Yes" & Spinose=="No" ~ "Symbiont-facultative Non-Spinose",
  Symbiosis == "Yes" & Spinose=="Yes" ~ "Symbiont-obligate Spinose"))

pi_a %>% pivot_wider(id_cols = c("Latitude", "Longitude"),
                           values_from = "Absolute Abundance",
                           names_from = "Functional Group", 
                           values_fill = 0,
                           values_fn=mean)%>%
  fwrite("tidy/forcens_fg_a_tidy.csv")

pi_r <- read_csv("fg/forcens_fg_r.csv")%>% mutate(`Functional Group` = case_when(
  Symbiosis == "No" & Spinose=="No" ~ "Symbiont-barren Non-Spinose",
  Symbiosis == "No" & Spinose=="Yes" ~ "Symbiont-barren Spinose",
  Symbiosis == "Yes" & Spinose=="No" ~ "Symbiont-facultative Non-Spinose",
  Symbiosis == "Yes" & Spinose=="Yes" ~ "Symbiont-obligate Spinose"))

pi_r %>% pivot_wider(id_cols = c("Latitude", "Longitude"),
                     values_from = "Relative Abundance",
                     names_from = "Functional Group", 
                     values_fill = 0,
                     values_fn=mean)%>%
  fwrite("tidy/forcens_fg_r_tidy.csv")

#--------------------
# Plot example
#--------------------
# library(sf)
# library(tmap)
# land <- read_sf("tidy/ne_50m_land/ne_50m_land.shp")
# p_land <- tm_shape(land)+ tm_polygons()
# 
# df <- wdf %>%  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) #WGS84
# p_site <- tm_shape(df) + tm_symbols(col="Symbiont-facultative Non-Spinose",size=0.5)
# 
# p_land + p_site
