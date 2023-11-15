#---------------------------------------------------
# merge all relative abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern = "^lgm.*_r\\.csv$", full.names = TRUE)

ldf <- lapply(filenames, function(file) {
  df <- read_csv(file)
  df <- df %>% select(c("Latitude", "Longitude", "Species", "Relative Abundance"))
  df <- df %>% mutate(Data_Source = str_extract(basename(file), "(?<=lgm_).*?(?=_sp_r\\.csv)"))  
  return(df)
})

wdf <- ldf %>%
  rbindlist() %>%
  pivot_wider(
    id_cols = c("Data_Source", "Latitude", "Longitude"),
    values_from = "Relative Abundance",
    names_from = "Species",
    values_fill = NA, 
    values_fn = list
  ) %>%
  unnest(cols = c(`O. universa`:`G. crassa`))

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]

wdf %>% fwrite("tidy/lgm_sp_r_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Species)
#---------------------------------------------------

filenames <- list.files("sp", pattern = "^lgm.*_a\\.csv$", full.names = TRUE)

ldf <- lapply(filenames, function(file) {
  df <- read_csv(file)
  df <- df %>% select(c("Latitude", "Longitude", "Species", "Absolute Abundance"))
  df <- df %>% mutate(Data_Source = str_extract(basename(file), "(?<=lgm_).*?(?=_sp_a\\.csv)"))  
  return(df)
})

wdf <- ldf %>%
  rbindlist() %>%
  pivot_wider(
    id_cols = c("Data_Source","Latitude", "Longitude"),
    values_from = "Absolute Abundance",
    names_from = "Species",
    values_fill = NA,
    values_fn = list
  ) %>%
  unnest(cols = c(`O. universa`:`G. crassa`))

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]
wdf %>% fwrite("tidy/lgm_sp_a_tidy.csv")

#---------------------------------------------------
# merge all absolute abundance (LGM Functional Group)
#---------------------------------------------------

filenames <- list.files("fg", pattern = "^lgm.*_a\\.csv$", full.names = TRUE)

ldf <- lapply(filenames, function(file) {
  df <- read_csv(file)
  df <- df %>% select(c("Latitude", "Longitude", "Spine", "Symbiosis", "Absolute Abundance"))
  df <- df %>% mutate(Data_Source = str_extract(basename(file), "(?<=lgm_).*?(?=_fg_a\\.csv)"))  
  return(df)
})

ldf <- ldf %>% rbindlist()

ldf <-  ldf %>% mutate(`Functional Group` = paste(Symbiosis, Spine, sep = " "))

wdf <- ldf %>% pivot_wider(
  id_cols = c("Data_Source", "Latitude", "Longitude"),
  values_from = "Absolute Abundance",
  names_from = "Functional Group",
  values_fill = NA,
  values_fn = list
) %>% unnest(cols=`symbiont-barren non-spinose`:`symbiont-facultative spinose`)

## remove duplicate rows
wdf <- wdf[!duplicated(wdf), ]

wdf %>% fwrite("tidy/lgm_fg_a_tidy.csv")

#---------------------------------------------------
# merge all relative abundance (LGM Functional Group)
#---------------------------------------------------

filenames <- list.files("fg", pattern = "^lgm.*_r\\.csv$", full.names = TRUE)

ldf <- lapply(filenames, function(file) {
  df <- read_csv(file)
  df <- df %>% select(c("Latitude", "Longitude", "Spine", "Symbiosis", "Relative Abundance"))
  df <- df %>% mutate(Data_Source = str_extract(basename(file), "(?<=lgm_).*?(?=_fg_r\\.csv)"))  
  return(df)
})

ldf <- ldf %>%
  rbindlist() %>%
  mutate(`Functional Group` =  paste(Symbiosis, Spine, sep = " "))

wdf <- ldf %>% pivot_wider(
  id_cols = c("Data_Source","Latitude", "Longitude"),
  values_from = "Relative Abundance",
  names_from = "Functional Group",
  values_fill = NA,
  values_fn = list)

## handle with the case of different length of vectors
## delete it first
tmp1 <- wdf[521,]
wdf <- wdf[-521,]

tmp2 <- wdf[536,]
wdf <- wdf[-536,]

wdf <- wdf %>% unnest(`symbiont-barren non-spinose`:`symbiont-facultative spinose`)

## putting back the deleted rows
## the number is based on calculation that all groups sum up to 1
tmp1$`symbiont-barren non-spinose`[[1]] <- c(tmp1$`symbiont-barren non-spinose`[[1]],
                                             1-0.03)
tmp1 <- tmp1 %>% unnest(`symbiont-barren non-spinose`:`symbiont-facultative spinose`)

tmp2$`symbiont-barren non-spinose`[[1]] <- c(tmp2$`symbiont-barren non-spinose`[[1]],
                                             (1-0.013-0.019-0.002),
                                             (1-0.001))
tmp2 <- tmp2 %>% unnest(`symbiont-barren non-spinose`:`symbiont-facultative spinose`)

wdf <- rbind(wdf, tmp1, tmp2)

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
    values_fill = NA,
    values_fn = list
  ) %>%
  distinct() %>%
  unnest(cols=`symbiont-barren spinose`:`symbiont-facultative spinose`)

pi_aw %>% fwrite("tidy/forcens_fg_a_tidy.csv")

pi_r <- read_csv("fg/forcens_fg_r.csv") %>% mutate(`Functional Group` =  paste(Symbiosis, Spine, sep = " "))

pi_rw <- pi_r %>%
  pivot_wider(
    id_cols = c("Latitude", "Longitude"),
    values_from = "Relative Abundance",
    names_from = "Functional Group",
    values_fill = NA,
    values_fn = list
  ) %>%
  distinct() %>%
  unnest(cols=`symbiont-barren spinose`:`symbiont-facultative spinose`)

pi_rw %>% fwrite("tidy/forcens_fg_r_tidy.csv")

## forcens species data already exported in clean_forcens.R
## so ignore here

#--------------------
# Plot example
#--------------------
library(sf)
library(tmap)
land <- read_sf("tidy/ne_50m_land/ne_50m_land.shp")
p_land <- tm_shape(land)+ tm_polygons()

df_lgm <- fread("tidy/lgm_sp_a_tidy.csv") %>% st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) #WGS84
p_lgm <- tm_shape(df_lgm) + tm_symbols(col="N. pachyderma",size=0.3,
                                       palette = "viridis")
p_land + p_lgm
