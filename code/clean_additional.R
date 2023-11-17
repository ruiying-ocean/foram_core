## This script adds more cores with absolute abundance (LGM) to the compilation
## it includes:
## MD95-2040, MD95-2042, V26-124
## Fr194-GC3, SS0206-GC15, JM05-085-GC
## 341-U1421, OCE400-MC44, IODP_U1418
## PS2644-5

core1 <- read_tsv("raw/additional/MD95-2042_foram.tab")
## Latitude: 37.799833 * Longitude: -10.166500
core1 <- core1 %>% mutate(Latitude = 37.799833, Longitude = -10.166500, .before='Depth sed [m]')
core1 <- core1 %>% dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21) %>% select(-"Age [ka BP]")

core2 <- read_tsv("raw/additional/MD95-2040_foram.tab")
## Latitude: 40.581833 * Longitude: -9.861167
core2 <- core2 %>% mutate(Latitude = 40.581833, Longitude = -9.861167, .before='Depth sed [m]')
core2 <- core2 %>% dplyr::filter(`Depth sed [m]` > 3.415 & `Depth sed [m]` < 4.885)

core3 <- read_tsv("raw/additional/V26-124_foram.tab")
core3 <- core3 %>% mutate(Latitude = 16.133000, Longitude = -74.450000, .before='Depth sed [m]')
core3 <- core3 %>% dplyr::filter(`Depth sed [m]` == 0.6)

core4 <- read_tsv("raw/additional/Fr194-GC3_foram.tab")
core4 %>% dplyr::filter(`Age dated [ka]` == 18.30 | `Age dated [ka]` == 24.80) %>%
  select(!contains("%")) -> core4
## Latitude: −44.25, Longitude: 149.98
core4 <- core4 %>% mutate(Latitude = -44.25, Longitude = 149.98, .before='Depth sed [m]')

core5 <- read_tsv("raw/additional/SS0206-GC15_foram.tab")
core5 %>% dplyr::filter(`Age [ka BP]`>19 & `Age [ka BP]` <21) -> core5
## Latitude: -39.312830, Longitude: 142.683980
core5 <- core5 %>% mutate(Latitude = -39.312830, Longitude = 142.683980, .before='Depth sed [m]')
core5 <- core5 %>% select(-c("Depth corr [m]", "Foram plankt indet [%]", "Age [ka BP]",
                             "Foram plankt tropical [%]", "Foram plankt subpolar [%]"))

core6 <- read_tsv("raw/additional/JM05-085-GC_foram.tab")
core6 <- core6 %>% select(-c("Age [ka BP]","Foram plankt [#/g]", "Foram plankt [#]", "Foram plankt flux [#/cm**2/ka]"))
## Latitude: 71.621800, Longitude: 22.926100
core6 <- core6 %>% mutate(Latitude = 71.621800, Longitude = 22.926100, .before='Depth sed [m]')

core7 <- read_tsv("raw/additional/341-U1421_foram.tab") 
core7 <- core7 %>% dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21) %>%
    select(`Depth top [m] (CSF-A, IODP Depth Scale Termi...)`,
           `G. bulloides [#] (Counting >150 µm fraction)`:`O. universa [#] (Counting >150 µm fraction)`) %>%
  rename(`Depth top [m]`=`Depth top [m] (CSF-A, IODP Depth Scale Termi...)`)
core7 <- core7 %>% mutate(Latitude = 59.507200, Longitude =-144.045600, .before=1)


core8 <- read_tsv("https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/keigwin2014/keigwin2014plank.txt", comment="#")
## age model: 
## https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/mellon2019/mellon2019-oce400_mc44.txt
core8 <- core8%>% dplyr::filter(depth_int_cm=='34-35')
core8 %>% select('depth_cm','n.pachy', 'n.incomp','g.bull','t.quinq','g.infla','g.glut') -> core8
core8 <- core8 %>% mutate(Latitude = 43.483033, Longitude = -67.882617)

core9 <- read_tsv("https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/payne2021/payne2021_SNW.txt", comment="#")
core9 <- core9 %>% dplyr::filter(age>19000) %>% select(depth, Num_Npachy) %>% mutate(Latitude =58.8, Longitude=-144.13)
core9 <- core9 %>% replace_column_name("Num_Npachy", "N. pachyderma")


core10 <- read_tsv("raw/additional/PS2644-5_foram.tab")
core10 <- core10 %>% select(c("Depth sed [m]", "N. pachyderma s [#]")) %>%
  rename(`N. pachyderma` = `N. pachyderma s [#]`) %>%
  mutate(Latitude = 67.866660, Longitude =-21.765000, .before=1)
## age model: https://doi.pangaea.de/10.1594/PANGAEA.57797
core10 <- core10 %>% dplyr::filter(`Depth sed [m]` >= 1.1 & `Depth sed [m]` <= 1.23)


## -----------------
## clean species name
## -----------------
## Orbulina suturalis is Miocene species as Brummer and Kucera (2022)
core1 <- core1 %>% select(-c("Foraminifera indet [#]", "Foram benth [#]", "Foram plankt fragm [#]"))
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

core4 <- core4 %>% select(-c(`G. bulloides [#] (Counting)...11`,`B. praeadamsi [#] (Counting)`,
                             `D. conglomerata [#] (Counting)`,`D. pseudofoliata [#] (Counting)`))
names(core4) <- gsub(" [#] (Counting)", "", names(core4), fixed=T)

core4 <- core4 %>% replace_column_name("G. bulloides...10", "G. bulloides")
core4 <- core4 %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
core4 <- core4 %>% replace_column_name("G. aequilateralis", "G. siphonifera")
core4 <- core4 %>% replace_column_name("G. rubescens white", "G. rubescens")
core4 <- core4 %>% replace_column_name("G. ruber w", "G. ruber albus")
core4 <- core4 %>% replace_column_name("N. pachyderma d", "N. incompta")
core4 <- core4 %>% replace_column_name("N. pachyderma s", "N. pachyderma")
core4 <- core4 %>% replace_column_name("G. sacculifer", "T. sacculifer")
core4 <- core4 %>% replace_column_name("G.umbilicata", "G. bulloides")
core4 <- core4 %>% replace_column_name("G. hexagona", "G. hexagonus")
core4 <- core4 %>% replace_column_name("G. clarkei", "T. clarkei")
core4 <- core4 %>% replace_column_name("G. inflata d", "G. inflata")
core4 <- core4 %>% replace_column_name("G. inflata s", "G. inflata")
core4 <- core4 %>% replace_column_name("G. tenella", "G. tenellus")
core4 <- core4 %>% replace_column_name("G. crassula", "G. crassaformis")
core4 <- core4 %>% select(-c("G. obesa", "Foram", "G. praecalida", "G. umbilicata"))

names(core5) <- gsub(" [%]", "", names(core5), fixed=T)
core5 <- core5 %>% replace_column_name("T. quinqueloba d", "T. quinqueloba")
core5 <- core5 %>% replace_column_name("T. quinqueloba s", "T. quinqueloba")
core5 <- core5 %>% replace_column_name("T. crassaformis", "G. crassaformis")
core5 <- core5 %>% replace_column_name("T. truncatulinoides", "G. truncatulinoides")
core5 <- core5 %>% replace_column_name("G. sacculifer", "T. sacculifer")
core5 <- core5 %>% replace_column_name("G. trilobus", "T. sacculifer")
core5 <- core5 %>% replace_column_name("G. digitata", "B. digitata")
core5 <- core5 %>% replace_column_name("N. pachyderma d", "N. incompta")
core5 <- core5 %>% replace_column_name("N. pachyderma s", "N. pachyderma")
core5 <- core5 %>% replace_column_name("G. tenella", "G. tenellus")
core5 <- core5 %>% replace_column_name("G. ruber", "G. ruber albus") ## G. ruber ruber is not appearing in Pacific

names(core6) <- gsub(" [#]", "", names(core6), fixed=T)
core6 <- core6 %>% replace_column_name("N. pachyderma d", "N. incompta")
core6 <- core6 %>% replace_column_name("N. pachyderma s", "N. pachyderma")

names(core7) <- gsub(" [#] (Counting >150 µm fraction)", "", names(core7), fixed=T)
core7 <- core7 %>% replace_column_name("N. pachyderma d", "N. incompta")
core7 <- core7 %>% replace_column_name("N. pachyderma s", "N. pachyderma")
core7 <- core7 %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
core7 <- core7 %>% replace_column_name("G. umbilicata", "G. bulloides")

core8 <- core8 %>% replace_column_name('n.pachy', 'N. pachyderma')
core8 <- core8 %>% replace_column_name('n.incomp', 'N. incompta')
core8 <- core8 %>% replace_column_name('g.bull', 'G. bulloides')
core8 <- core8 %>% replace_column_name('t.quinq', 'T. quinqueloba')
core8 <- core8 %>% replace_column_name('g.infla', 'G. inflata')
core8 <- core8 %>% replace_column_name('g.glut', 'G. glutinata')

find_missing_species(symbiosis_tbl$short_name, names(core7))
## -----------------
## Just absolute abundance
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()

core1_long <- core1 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]",), names_to = "Species", values_to = "Absolute Abundance")
core2_long <- core2 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core3_long <- core3 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core4_long <- core4 %>% select(-c('Sample ID',"Depth top [m] (Foram sample depths)", 'Age dated [ka]', "Depth bot [m] (Foram sample depths)")) %>%
    pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]",), names_to = "Species", values_to = "Absolute Abundance")

## convert relative abundance to absolute abundance (core5)
core5 %>% mutate_at(vars(`G. bulloides`:`T. quinqueloba`), ~ as.integer(. * `Foram plankt [#]`/100)) %>% select(-c(`Foram plankt [#]`)) -> core5
core5_long <- core5 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core6_long <- core6 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core7_long <- core7 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth top [m]"), names_to = "Species", values_to = "Absolute Abundance")
core8_long <- core8 %>% pivot_longer(cols = -c("Latitude", "Longitude", "depth_cm"), names_to = "Species", values_to = "Absolute Abundance")

core9_long <- core9 %>% pivot_longer(cols = -c("Latitude", "Longitude", "depth"), names_to = "Species", values_to = "Absolute Abundance")

core10_long <- core10 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

## save to sp
write_csv(core1_long, "sp/lgm_MD952042_sp_a.csv")
write_csv(core2_long, "sp/lgm_MD952040_sp_a.csv")
write_csv(core3_long, "sp/lgm_V26-124_sp_a.csv")
write_csv(core4_long, "sp/lgm_Fr194-GC3_sp_a.csv")
write_csv(core5_long, "sp/lgm_SS0206-GC15_sp_a.csv")
write_csv(core6_long, "sp/lgm_JM05-085-GC_sp_a.csv")
write_csv(core7_long, "sp/lgm_341-U1421_sp_a.csv")
write_csv(core8_long, "sp/lgm_OCE400-MC44_sp_a.csv")
write_csv(core9_long, "sp/lgm_IODP_U1418_sp_a.csv")
write_csv(core10_long, "sp/lgm_PS2644-5_sp_a.csv")

## save to fg
core1_long <- merge(core1_long,  symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core2_long <- merge(core2_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core3_long <- merge(core3_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core4_long <- merge(core4_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core5_long <- merge(core5_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core6_long <- merge(core6_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core7_long <- merge(core7_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core8_long <- merge(core8_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core9_long <- merge(core9_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
core10_long <- merge(core10_long, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))

core1_long <- core1_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core2_long <- core2_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core3_long <- core3_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core4_long <- core4_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core5_long <- core5_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core6_long <- core6_long %>% group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core7_long <- core7_long %>% group_by(Latitude, Longitude, `Depth top [m]`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core8_long <- core8_long %>% group_by(Latitude, Longitude, `depth_cm`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core9_long <- core9_long %>% group_by(Latitude, Longitude, `depth`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
core10_long <- core9_long %>% group_by(Latitude, Longitude, `depth`, Symbiosis, Spine) %>% 
  summarise_all(.funs = sum, na.rm=T)  %>% ungroup()

write_csv(core1_long, "fg/lgm_MD952042_fg_a.csv")
write_csv(core2_long, "fg/lgm_MD952040_fg_a.csv")
write_csv(core3_long, "fg/lgm_V26-124_fg_a.csv")
write_csv(core4_long, "fg/lgm_Fr194-GC3_fg_a.csv")
write_csv(core5_long, "fg/lgm_SS0206-GC15_fg_a.csv")
write_csv(core6_long, "fg/lgm_JM05-085-GC_fg_a.csv")
write_csv(core7_long, "fg/lgm_341-U1421_fg_a.csv")
write_csv(core8_long, "fg/lgm_OCE400-MC44_fg_a.csv")
write_csv(core9_long, "fg/lgm_IODP_U1418_fg_a.csv")
write_csv(core10_long, "fg/lgm_PS2644-5_fg_a.csv")
