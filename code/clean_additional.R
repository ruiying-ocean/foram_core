## This script adds more cores with absolute abundance (LGM) to the compilation
## mostly in the high latitude North Atlantic

## MD95-2042
core1 <- read_tsv("raw/additional/MD95-2042_foram.tab")
core1 <- core1 %>% mutate(Latitude = 37.799833, Longitude = -10.166500, .before = "Depth sed [m]")
core1 <- core1 %>%
  dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21) %>%
  select(-"Age [ka BP]")

## MD95-2040
core2 <- read_tsv("raw/additional/MD95-2040_foram.tab")
core2 <- core2 %>% mutate(Latitude = 40.581833, Longitude = -9.861167, .before = "Depth sed [m]")
core2 <- core2 %>% dplyr::filter(`Depth sed [m]` > 3.415 & `Depth sed [m]` < 4.885)

## V26-124
core3 <- read_tsv("raw/additional/V26-124_foram.tab")
core3 <- core3 %>% mutate(Latitude = 16.133000, Longitude = -74.450000, .before = "Depth sed [m]")
core3 <- core3 %>% dplyr::filter(`Depth sed [m]` == 0.6)

## Fr194-GC3
core4 <- read_tsv("raw/additional/Fr194-GC3_foram.tab")
core4 %>%
  dplyr::filter(`Age dated [ka]` == 18.30 | `Age dated [ka]` == 24.80) %>%
  select(!contains("%")) -> core4
core4 <- core4 %>% mutate(Latitude = -44.25, Longitude = 149.98, .before = "Depth sed [m]")

## SS0206-GC15
core5 <- read_tsv("raw/additional/SS0206-GC15_foram.tab")
core5 %>% dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21) -> core5
core5 <- core5 %>% mutate(Latitude = -39.312830, Longitude = 142.683980, .before = "Depth sed [m]")
core5 <- core5 %>% select(-c(
  "Depth corr [m]", "Foram plankt indet [%]", "Age [ka BP]",
  "Foram plankt tropical [%]", "Foram plankt subpolar [%]"
))

## JM05-085-GC
core6 <- read_tsv("raw/additional/JM05-085-GC_foram.tab")
core6 <- core6 %>% select(-c("Age [ka BP]", "Foram plankt [#/g]", "Foram plankt [#]", "Foram plankt flux [#/cm**2/ka]"))
core6 <- core6 %>% mutate(Latitude = 71.621800, Longitude = 22.926100, .before = "Depth sed [m]")

## 341-U1421
core7 <- read_tsv("raw/additional/341-U1421_foram.tab")
core7 <- core7 %>%
  dplyr::filter(`Age [ka BP]` > 19 & `Age [ka BP]` < 21) %>%
  select(
    `Depth top [m] (CSF-A, IODP Depth Scale Termi...)`,
    `G. bulloides [#] (Counting >150 µm fraction)`:`O. universa [#] (Counting >150 µm fraction)`
  ) %>%
  rename(`Depth top [m]` = `Depth top [m] (CSF-A, IODP Depth Scale Termi...)`)
core7 <- core7 %>% mutate(Latitude = 59.507200, Longitude = -144.045600, .before = 1)

## OCE400-MC44
core8 <- read_tsv("https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/keigwin2014/keigwin2014plank.txt", comment = "#")
## age model:
## https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/mellon2019/mellon2019-oce400_mc44.txt
core8 <- core8 %>% dplyr::filter(depth_int_cm == "34-35")
core8 %>% select("depth_cm", "n.pachy", "n.incomp", "g.bull", "t.quinq", "g.infla", "g.glut") -> core8
core8 <- core8 %>% mutate(Latitude = 43.483033, Longitude = -67.882617)

## IODP_U1418
core9 <- read_tsv("https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/payne2021/payne2021_SNW.txt", comment = "#")
core9 <- core9 %>%
  dplyr::filter(age > 19000) %>%
  select(depth, Num_Npachy) %>%
  mutate(Latitude = 58.8, Longitude = -144.13)
core9 <- core9 %>% revise_sp_name("Num_Npachy", "N. pachyderma")

## PS2644-5
## this core seems to use different unit
core10 <- read_tsv("raw/additional/PS2644-5_foram.tab")
core10 <- core10 %>%
  select(c("Depth sed [m]", "N. pachyderma s [#/g]")) %>%
  rename(`N. pachyderma` = `N. pachyderma s [#/g]`) %>%
  mutate(Latitude = 67.866660, Longitude = -21.765000, .before = 1)
## age model: https://doi.pangaea.de/10.1594/PANGAEA.57797
core10 <- core10 %>% dplyr::filter(`Depth sed [m]` >= 1.1 & `Depth sed [m]` <= 1.23)

## PS1243-1
core11 <- read_tsv("raw/additional/PS1243-1_foram_125-250.tab")
core11 <- core11 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core11 <- core11 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core11 <- core11 %>% mutate(Latitude = 69.371800, Longitude = -6.553000, .before = "Depth sed [m]")
## PS1244-2
core12 <- read_tsv("raw/additional/PS1244-2_foram_125-250.tab")
core12 <- core12 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core12 <- core12 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core12 <- core12 %>% mutate(Latitude = 69.365200, Longitude = -8.657000, .before = "Depth sed [m]")
## PS1246-2
core13 <- read_tsv("raw/additional/PS1246-2_foram_125-250.tab")
core13 <- core13 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core13 <- core13 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core13 <- core13 %>% mutate(Latitude = 69.385300, Longitude = -12.874800, .before = "Depth sed [m]")
## PS1906-2
core14 <- read_tsv("raw/additional/PS1906-2_foram_125-250.tab")
core14 <- core14 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core14 <- core14 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core14 <- core14 %>% mutate(Latitude = 76.846300, Longitude = -2.150500, .before = "Depth sed [m]")
## GIK17732-1
core15 <- read_tsv("raw/additional/GIK17732-1_foram_125-250.tab")
core15 <- core15 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core15 <- core15 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core15 <- core15 %>% mutate(Latitude = 71.613333, Longitude = 4.213333, .before = "Depth sed [m]")
## GIK23059-3
core16 <- read_tsv("raw/additional/GIK23059-3_foram_125-250.tab")
core16 <- core16 %>% mutate_at(vars(`G. bulloides [#/g]`:`N. pachyderma d [#/g]`), ~ as.integer(. / `Foram plankt indet [#/g]` * `Foram plankt [#]`))
core16 <- core16 %>% select(-c(`Age [ka BP]`, `Samp m [g]`, `Foram plankt indet [#/g]`, `Foram plankt [#]`, `B. megastoma [#/g]`))
core16 <- core16 %>% mutate(Latitude = 70.305000, Longitude = -3.123333, .before = "Depth sed [m]")
## GIK15612-2
core17 <- read_tsv("raw/additional/GIK15612-2_foram.tab")
core17 <- core17 %>% select(-c(
  "SST win [°C] (Counting >150 µm fraction)",
  "PP C [g/m**2/a] (Calculated)",
  "SST sum [°C] (Counting >150 µm fraction)",
  "G. menardii [%] (including G. tumida, Counting...)"
))

## -----------------
## clean species name
## -----------------
## Orbulina suturalis is Miocene species as Brummer and Kucera (2022)
core1 <- core1 %>% select(-c("Foraminifera indet [#]", "Foram benth [#]", "Foram plankt fragm [#]"))
names(core1) <- gsub(" [#]", "", names(core1), fixed = T)
core1 <- core1 %>% clean_species()
core1 <- merge_morphotypes(core1, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
core1 <- core1 %>% select(-`O. suturalis`)

## -----------------
core2 <- core2 %>% select(-c("Foram benth [#]"))
names(core2) <- gsub(" [#]", "", names(core2), fixed = T)
core2 <- core2 %>% revise_sp_name("G. menardii", "G. cultrata")
core2 <- merge_morphotypes(core2, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
core2 <- clean_species(core2)
core2 <- core2 %>% revise_sp_name("T. cristata", "T. humilis")
core2 <- merge_morphotypes(core2, c("G. hirsuta d", "G. hirsuta s"), "G. hirsuta")
core2 <- core2 %>% revise_sp_name("P/D int", "N. incompta")
core2 <- core2 %>% revise_sp_name("G. trilobus tril", "T. sacculifer")

## -----------------
core3 <- core3 %>% select(-c(
  `G. ruber [#] (sum of G. ruber pink and G. r...)`,
  `Foram plankt oth [#]`,
  `G. menardii [#] (sum of G. menardii, G. tumida...)`,
  `G. sacculifer wo sac [#]`,
  `G. sacculifer sac [#]`,
))
names(core3) <- gsub(" [#]", "", names(core3), fixed = T)
core3 <- merge_morphotypes(core3, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
core3 <- core3 %>% clean_species()
core3 <- core3 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
## -----------------
## G. bulloides [#] (Counting)...11 is 0
core4 <- core4 %>% select(-c(
  `G. bulloides [#] (Counting)...11`, `B. praeadamsi [#] (Counting)`,
  `D. conglomerata [#] (Counting)`, `D. pseudofoliata [#] (Counting)`
))
names(core4) <- gsub(" [#] (Counting)", "", names(core4), fixed = T)

core4 <- core4 %>% revise_sp_name("G. bulloides...10", "G. bulloides")
core4 <- core4 %>% revise_sp_name("G. rubescens white", "G. rubescens")
core4 <- core4 %>% revise_sp_name("G.umbilicata", "G. bulloides")
core4 <- merge_morphotypes(core4, c("G. inflata d", "G. inflata s"), "G. inflata")
core4 <- clean_species(core4)
core4 <- core4 %>% select(-c("G. obesa", "Foram", "G. praecalida", "G. umbilicata"))

## -----------------
names(core5) <- gsub(" [%]", "", names(core5), fixed = T)
core5 <- core5 %>% select(-c("T. quinqueloba d", "T. quinqueloba s"))
core5 <- core5 %>% revise_sp_name("T. crassaformis", "G. crassaformis")
core5 <- core5 %>% revise_sp_name("T. truncatulinoides", "G. truncatulinoides")
core5 <- core5 %>% revise_sp_name("G. sacculifer", "T. sacculifer")
core5 <- core5 %>% revise_sp_name("G. trilobus", "T. sacculifer")
core5 <- core5 %>% revise_sp_name("G. digitata", "B. digitata")
core5 <- core5 %>% revise_sp_name("N. pachyderma d", "N. incompta")
core5 <- core5 %>% revise_sp_name("N. pachyderma s", "N. pachyderma")
core5 <- core5 %>% revise_sp_name("G. tenella", "G. tenellus")
core5 <- core5 %>% revise_sp_name("G. ruber", "G. ruber albus") ## G. ruber ruber is not appearing in Pacific
## -----------------
names(core6) <- gsub(" [#]", "", names(core6), fixed = T)
core6 <- core6 %>% revise_sp_name("N. pachyderma d", "N. incompta")
core6 <- core6 %>% revise_sp_name("N. pachyderma s", "N. pachyderma")
## -----------------
names(core7) <- gsub(" [#] (Counting >150 µm fraction)", "", names(core7), fixed = T)
core7 <- core7 %>% revise_sp_name("N. pachyderma d", "N. incompta")
core7 <- core7 %>% revise_sp_name("N. pachyderma s", "N. pachyderma")
core7 <- core7 %>% revise_sp_name("G. quinqueloba", "T. quinqueloba")
core7 <- core7 %>% revise_sp_name("G. umbilicata", "G. bulloides")
## -----------------
core8 <- core8 %>% revise_sp_name("n.pachy", "N. pachyderma")
core8 <- core8 %>% revise_sp_name("n.incomp", "N. incompta")
core8 <- core8 %>% revise_sp_name("g.bull", "G. bulloides")
core8 <- core8 %>% revise_sp_name("t.quinq", "T. quinqueloba")
core8 <- core8 %>% revise_sp_name("g.infla", "G. inflata")
core8 <- core8 %>% revise_sp_name("g.glut", "G. glutinata")
## -----------------
names(core11) <- gsub(" [#/g]", "", names(core11), fixed = T)
core11 <- clean_species(core11) %>% replace_na_with_zero()
## -----------------
names(core12) <- gsub(" [#/g]", "", names(core12), fixed = T)
core12 <- clean_species(core12) %>% replace_na_with_zero()
## -----------------
names(core13) <- gsub(" [#/g]", "", names(core13), fixed = T)
core13 <- clean_species(core13) %>% replace_na_with_zero()
## -----------------
names(core14) <- gsub(" [#/g]", "", names(core14), fixed = T)
core14 <- clean_species(core14) %>% replace_na_with_zero()
## -----------------
names(core15) <- gsub(" [#/g]", "", names(core15), fixed = T)
core15 <- clean_species(core15) %>% replace_na_with_zero()
## -----------------
names(core16) <- gsub(" [#/g]", "", names(core16), fixed = T)
core16 <- clean_species(core16) %>% replace_na_with_zero()
## -----------------
names(core17) <- gsub(" [%] (Counting >150 µm fraction)", "", names(core17), fixed = T)
core17 <- merge_morphotypes(core17, c("P/D int...43", "P/D int...25", "N. pachyderma d"), "N. incompta")
## the total abundance is provided already
core17 <- core17 %>% select(-c(
  "G. quinqueloba s", "G. quinqueloba d",
  "G. cavernula",
  "G. truncatulinoides d", "G. truncatulinoides s"
))
core17 <- merge_morphotypes(core17, c("G. trilobus sac", "G. trilobus tril"), "T. sacculifer")
core17 <- clean_species(core17)
## convert to absoltue abundance
core17 <- core17 %>% mutate_at(vars(`G. eastropacia`:`T. sacculifer`), ~ as.integer(. * `Foram plankt [#] (Counting >150 µm fraction)` / 100))
core17 <- core17 %>% select(-c("Foram plankt [#] (Counting >150 µm fraction)", "Age [ka BP]"))
core17 <- core17 %>% mutate(Latitude = 44.360000, Longitude = -26.543333, .before = "Depth sed [m]")

find_missing_species(symbiosis_tbl$short_name, names(core17))

## -----------------
## Just absolute abundance
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>%
  select(!c(Species)) %>%
  distinct()

core1_long <- core1 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]", ), names_to = "Species", values_to = "Absolute Abundance")
core2_long <- core2 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core3_long <- core3 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core4_long <- core4 %>%
  select(-c("Sample ID", "Depth top [m] (Foram sample depths)", "Age dated [ka]", "Depth bot [m] (Foram sample depths)")) %>%
  pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]", ), names_to = "Species", values_to = "Absolute Abundance")

## convert relative abundance to absolute abundance (core5)
core5 %>%
  mutate_at(vars(`G. bulloides`:`T. quinqueloba`), ~ as.integer(. * `Foram plankt [#]` / 100)) %>%
  select(-c(`Foram plankt [#]`)) -> core5
core5_long <- core5 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core6_long <- core6 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

core7_long <- core7 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth top [m]"), names_to = "Species", values_to = "Absolute Abundance")
core8_long <- core8 %>% pivot_longer(cols = -c("Latitude", "Longitude", "depth_cm"), names_to = "Species", values_to = "Absolute Abundance")

core9_long <- core9 %>% pivot_longer(cols = -c("Latitude", "Longitude", "depth"), names_to = "Species", values_to = "Absolute Abundance")

core10_long <- core10 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core11_long <- core11 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core12_long <- core12 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core13_long <- core13 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core14_long <- core14 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core15_long <- core15 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core16_long <- core16 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")
core17_long <- core17 %>% pivot_longer(cols = -c("Latitude", "Longitude", "Depth sed [m]"), names_to = "Species", values_to = "Absolute Abundance")

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
write_csv(core11_long, "sp/lgm_PS1243-1_sp_a.csv")
write_csv(core12_long, "sp/lgm_PS1244-2_sp_a.csv")
write_csv(core13_long, "sp/lgm_PS1246-2_sp_a.csv")
write_csv(core14_long, "sp/lgm_PS1906-2_sp_a.csv")
write_csv(core15_long, "sp/lgm_GIK17732-1_sp_a.csv")
write_csv(core16_long, "sp/lgm_GIK23059-3_sp_a.csv")
write_csv(core17_long, "sp/lgm_GIK15612-2_sp_a.csv")

## save to fg
core1_long <- merge(core1_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core2_long <- merge(core2_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core3_long <- merge(core3_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core4_long <- merge(core4_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core5_long <- merge(core5_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core6_long <- merge(core6_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core7_long <- merge(core7_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core8_long <- merge(core8_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core9_long <- merge(core9_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core10_long <- merge(core10_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core11_long <- merge(core11_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core12_long <- merge(core12_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core13_long <- merge(core13_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core14_long <- merge(core14_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core15_long <- merge(core15_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core16_long <- merge(core16_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))
core17_long <- merge(core17_long, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))

core1_long <- core1_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core2_long <- core2_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core3_long <- core3_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core4_long <- core4_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core5_long <- core5_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core6_long <- core6_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core7_long <- core7_long %>%
  group_by(Latitude, Longitude, `Depth top [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core8_long <- core8_long %>%
  group_by(Latitude, Longitude, `depth_cm`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core9_long <- core9_long %>%
  group_by(Latitude, Longitude, `depth`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core10_long <- core10_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core11_long <- core11_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core12_long <- core12_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core13_long <- core13_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core14_long <- core14_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core15_long <- core15_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core16_long <- core16_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()
core17_long <- core17_long %>%
  group_by(Latitude, Longitude, `Depth sed [m]`, Symbiosis, Spine) %>%
  summarise_all(.funs = sum, na.rm = T) %>%
  ungroup()


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
write_csv(core11_long, "fg/lgm_PS1243-1_fg_a.csv")
write_csv(core12_long, "fg/lgm_PS1244-2_fg_a.csv")
write_csv(core13_long, "fg/lgm_PS1246-2_fg_a.csv")
write_csv(core14_long, "fg/lgm_PS1906-2_fg_a.csv")
write_csv(core15_long, "fg/lgm_GIK17732-1_fg_a.csv")
write_csv(core16_long, "fg/lgm_GIK23059-3_fg_a.csv")
write_csv(core17_long, "fg/lgm_GIK1561202_fg_a.csv")
