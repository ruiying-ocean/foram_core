## This script add the cores to the compilation

core1 <- read_tsv("raw/additional/MD95-2042_foram.tab")
## Latitude: 37.799833 * Longitude: -10.166500
core1 <- core1 %>% mutate(Latitude = 37.799833, Longitude = -10.166500, .before='Depth sed [m]')
core1 <- core1 %>% dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21)

core2 <- read_tsv("raw/additional/MD95-2040_foram.tab")
## Latitude: 40.581833 * Longitude: -9.861167
core2 <- core2 %>% mutate(Latitude = 40.581833, Longitude = -9.861167, .before='Depth sed [m]')
core2 <- core2 %>% dplyr::filter(`Depth sed [m]` > 3.415 & `Depth sed [m]` < 4.885)

core3 <- read_tsv("raw/additional/V26-124_foram.tab")
core3 <- core3 %>% mutate(Latitude = 16.133000, Longitude = -74.450000, .before='Depth sed [m]')
core3 <- core3 %>% dplyr::filter(`Depth sed [m]` == 0.6)

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

core3 <- core3 %>%   select(-c(`G. ruber [#] (sum of G. ruber pink and G. r...)`, 
                               `Foram plankt oth [#]`,
                               `G. menardii [#] (sum of G. menardii, G. tumida...)`,
                               `G. sacculifer wo sac [#]`,
                               `G. sacculifer sac [#]`,
                               `G. pachyderma [#]`
                               ))
names(core3) <- gsub(" [#]", "", names(core3), fixed=T)
core3 <- core3 %>% mutate(`G. truncatulinoides` = `G. truncatulinoides d` + `G. truncatulinoides s`) %>%
  select(-c(`G. truncatulinoides d`, `G. truncatulinoides s`))
core3 <- core3 %>% replace_column_name("G. ruber w", "G. ruber albus")
core3 <- core3 %>% replace_column_name("G. ruber p", "G. ruber ruber")
core3 <- core3 %>% replace_column_name("N. pachyderma d", "N. incompta")
core3 <- core3 %>% replace_column_name("N. pachyderma s", "N. pachyderma")
core3 <- core3 %>% replace_column_name("G. menardii", "G. cultrata")
core3 <- core3 %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
core3 <- core3 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
core3 <- core3 %>% replace_column_name("G. aequilateralis", "G. siphonifera")
core3 <- core3 %>% replace_column_name("G. dutertrei", "N. dutertrei")
core3 <- core3 %>% replace_column_name("G. digitata", "B. digitata")
core3 <- core3 %>% replace_column_name("G. humilis", "T. humilis")
core3 <- core3 %>% replace_column_name("G. anfracta", "D. anfracta")
core3 <- core3 %>% replace_column_name("G. iota", "T. iota")
core3 <- core3 %>% replace_column_name("G. bradyi", "G. uvula")
core3 <- core3 %>% replace_column_name("G. pumilio", "B. pumilio")
core3 <- core3 %>% replace_column_name("G. hexagona", "G. hexagonus")
core3 <- core3 %>% replace_column_name("G. tumida flexuosa", "G. tumida") ## as per Brummer & Kucera, 2022


find_missing_species(symbiosis_tbl$short_name, names(core3))

## -----------------
## Just absolute abundance
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()

core1_long <- core1 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]", "Age [ka BP]"), names_to = "Species", values_to = "Absolute Abundance")
core1_long <- merge(core1_long %>% select(-'Age [ka BP]'), symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core1_long <- core1_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()

core2_long <- core2 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core2_long <- merge(core2_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core2_long <- core2_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()

core3_long <- core3 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core3_long <- merge(core3_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core3_long <- core3_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()

## save to fg
write_csv(core1_long, "fg/lgm_MD952042_fg_a.csv")
write_csv(core2_long, "fg/lgm_MD952040_fg_a.csv")
write_csv(core3_long, "fg/lgm_V26-124_fg_a.csv")
