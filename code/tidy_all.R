#---------------------------------------------------
# merge all relative abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern = "^lgm.*_r\\.csv$", full.names = TRUE)
ldf <- lapply(filenames, function(df) {
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", "Longitude", "Species", "Relative Abundance"))
  return(df)
})

wdf <- ldf %>%
  rbindlist() %>%
  pivot_wider(
    id_cols = c("Latitude", "Longitude"),
    values_from = "Relative Abundance",
    names_from = "Species",
    values_fill = 0,
    values_fn = mean
  )

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]

wdf %>% fwrite("tidy/lgm_sp_r_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern = "^lgm.*_a\\.csv$", full.names = TRUE)
ldf <- lapply(filenames, function(df) {
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", "Longitude", "Species", "Absolute Abundance"))
  return(df)
})

wdf <- ldf %>%
  rbindlist() %>%
  pivot_wider(
    id_cols = c("Latitude", "Longitude"),
    values_from = "Absolute Abundance",
    names_from = "Species",
    values_fill = 0,
    values_fn = mean
  )
## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]
wdf %>% fwrite("tidy/lgm_sp_a_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Functional Group)
#---------------------------------------------------

filenames <- list.files("fg", pattern = "^lgm.*_a\\.csv$", full.names = TRUE)
ldf <- lapply(filenames, function(df) {
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", "Longitude", "Spine", "Symbiosis", "Absolute Abundance"))
  return(df)
})

ldf <- ldf %>%
  rbindlist()

ldf <-  ldf %>%
  mutate(`Functional Group` = paste(Symbiosis, Spine, sep = " "))

wdf <- ldf %>% pivot_wider(
  id_cols = c("Latitude", "Longitude"),
  values_from = "Absolute Abundance",
  names_from = "Functional Group",
  values_fill = 0,
  values_fn = mean
)

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]

wdf %>% fwrite("tidy/lgm_fg_a_tidy.csv")

#---------------------------------------------------
# merge all relative abundance (LGM Functional Group)
#---------------------------------------------------

filenames <- list.files("fg", pattern = "^lgm.*_r\\.csv$", full.names = TRUE)
ldf <- lapply(filenames, function(df) {
  df <- read_csv(df)
  df <- df %>% select(c("Latitude", "Longitude", "Spine", "Symbiosis", "Relative Abundance"))
  return(df)
})

ldf <- ldf %>%
  rbindlist() %>%
  mutate(`Functional Group` =  paste(Symbiosis, Spine, sep = " "))

wdf <- ldf %>% pivot_wider(
  id_cols = c("Latitude", "Longitude"),
  values_from = "Relative Abundance",
  names_from = "Functional Group",
  values_fill = 0,
  values_fn = mean
)

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]

wdf %>% fwrite("tidy/lgm_fg_r_tidy.csv")

#--------------------
# Modern fg relative/absolute abundance
#--------------------
pi_a <- read_csv("fg/forcens_fg_a.csv") %>% mutate(`Functional Group` =  paste(Symbiosis, Spine, sep = " "))

pi_aw <- pi_a %>%
  pivot_wider(
    id_cols = c("Latitude", "Longitude"),
    values_from = "Absolute Abundance",
    names_from = "Functional Group",
    values_fill = 0,
    values_fn = mean
  ) %>%
  distinct()

pi_aw %>% fwrite("tidy/forcens_fg_a_tidy.csv")

pi_r <- read_csv("fg/forcens_fg_r.csv") %>% mutate(`Functional Group` =  paste(Symbiosis, Spine, sep = " "))

pi_rw <- pi_r %>%
  pivot_wider(
    id_cols = c("Latitude", "Longitude"),
    values_from = "Relative Abundance",
    names_from = "Functional Group",
    values_fill = 0,
    values_fn = mean
  ) %>%
  distinct()
pi_rw %>% fwrite("tidy/forcens_fg_r_tidy.csv")

#--------------------
# Plot example
#--------------------
library(sf)
library(tmap)
land <- read_sf("tidy/ne_50m_land/ne_50m_land.shp")
p_land <- tm_shape(land)+ tm_polygons()

df_lgm <- fread("tidy/lgm_fg_a_tidy.csv") %>% st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) #WGS84
p_lgm <- tm_shape(df_lgm) + tm_symbols(col="symbiont-facultative spinose",size=0.5)
p_land + p_lgm
