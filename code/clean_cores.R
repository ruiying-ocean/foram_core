## This script add the cores to the compilation

core1 <- read_tsv("raw/additional/MD95-2042_foram.tab")
## Latitude: 37.799833 * Longitude: -10.166500
core1 <- core1 %>% mutate(Latitude = 37.799833, Longitude = -10.166500, .before='Depth sed [m]')
core1 <- core1 %>% dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21)

core2 <- read_tsv("raw/additional/MD95-2040_foram.tab")
## Latitude: 40.581833 * Longitude: -9.861167
core2 <- core2 %>% mutate(Latitude = 40.581833, Longitude = -9.861167, .before='Depth sed [m]')
core2 <- core2 %>% dplyr::filter(`Depth sed [m]` > 4.885 & `Depth sed [m]` < 5.305)

## -----------------
## clean species name
## -----------------
## Orbulina suturalis is Miocene species as Brummer and Kucera (2022)
core1 <- core1 %>% select(-c("Foraminifera indet [#]", "Foram benth [#]", "Foram plankt fragm [#]",
                             ))
names(core1) <- gsub(" [#]", "", names(core1), fixed=T)
core1 <- core1 %>% replace_column_name("G. menardii", "G. cultrata")
core1 <- core1 %>% replace_column_name("O. bilobata", "O. universa")
core1 <- core1 %>% replace_column_name("T. trilobus", "T. sacculifer")
core1 <- core1 %>% select(-`O. suturalis`)

## combine G. trunc d/s
core1 <- core1 %>% mutate(`G. truncatulinoides` = `G. truncatulinoides d` + `G. truncatulinoides s`) %>%
  select(-c(`G. truncatulinoides d`, `G. truncatulinoides s`))

core2 <- core2 %>% select(-c("Foram benth [#]"))
names(core2) <- gsub(" [#]", "", names(core2), fixed=T)
core2 <- core2 %>% replace_column_name("G. menardii", "G. cultrata")
core2 <- core2 %>% mutate(`G. truncatulinoides` = `G. truncatulinoides d` + `G. truncatulinoides s`) %>%
  select(-c(`G. truncatulinoides d`, `G. truncatulinoides s`))
core2 <- core2 %>% select(-c(`P/D int`,'G. trilobus tril')) 
core2 <- core2 %>% replace_column_name("G. iota", "T. iota")
core2 <- core2 %>% replace_column_name("N. pachyderma d", "N. incompta")
core2 <- core2 %>% replace_column_name("N. pachyderma s", "N. pachyderma")
core2 <- core2 %>% replace_column_name("G. sacculifer", "T. sacculifer")
core2 <- core2 %>% replace_column_name("G. ruber p", "G. ruber ruber")
core2 <- core2 %>% replace_column_name("G. ruber w", "G. ruber albus")
core2 <- core2 %>% replace_column_name("G. aequilateralis", "G. siphonifera")
core2 <- core2 %>% replace_column_name("T. cristata", "T. humilis")
core2 <- core2 %>% mutate(`G. hirsuta` = `G. hirsuta s` + `G. hirsuta d`) %>%
  select(-c(`G. hirsuta s`, `G. hirsuta d`))

## -----------------
## Just absolute abundance
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()

core1_long <- core1 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]", "Age [ka BP]"), names_to = "Species", values_to = "Absolute Abundance")
core1_long <- merge(core1_long %>% select(-'Age [ka BP]'), symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core1_long <- core1_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core1_long <- core1_long %>% group_by(Latitude, Longitude,  Symbiosis, Spine) %>% 
  summarise_all(.funs = mean, na.rm=T)  %>% ungroup()

core2_long <- core2 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core2_long <- merge(core2_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core2_long <- core2_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core2_long <- core2_long %>% group_by(Latitude, Longitude,  Symbiosis, Spine) %>%
  summarise_all(.funs = mean, na.rm=T)  %>% ungroup()

## save to fg
write_csv(core1_long, "fg/lgm_MD952042_fg_a.csv")
write_csv(core2_long, "fg/lgm_MD952040_fg_a.csv")
