## climap cores
## included_cores <- c(unique(climap_lgm_a$Event),unique(foram_dat_lgm$Event))
## this file seems to be identical to the source1 (CLIMAP_LGM_foram_data.tab)
## mix1999 <- read_tsv("raw/CLIMAP/Mix_et_al_1999.tab")
## find out those not included here
## mix1999%>% dplyr::filter(!Event %in% included_cores) %>% pull(Event) %>% unique()
## lgm_co3%>% dplyr::filter(!Event %in% included_cores) %>% pull(Event) %>% unique()


## read data
## these are already age filtered manually, with age references from lgm_co3/climap_lgm data.frames
## ------------------
CHN82.24 <- read_tsv("raw/CLIMAP/manually_downloaded/CHN82-24.tab") %>%
  mutate(Event = "CHN82-24", Latitude = 43.465000, Longitude = -32.850000, .before = 1)
names(CHN82.24) <- gsub(" [#]", "", names(CHN82.24), fixed = T)
CHN82.24 <- merge_morphotypes(CHN82.24, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
CHN82.24 %>% select(-c("G. ruber", "G. sacculifer sac", "G. sacculifer wo sac")) -> CHN82.24
CHN82.24 <- CHN82.24 %>%
  clean_species() %>%
  replace_na_with_zero()

## ------------------
ELT48.022 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT48.022-PC.tab") %>%
  mutate(Event = "ELT48.022-PC", Latitude = 39.895000, Longitude = -85.410000, .before = 1)
names(ELT48.022) <- gsub(" [#]", "", names(ELT48.022), fixed = T)
ELT48.022 <- merge_morphotypes(ELT48.022, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
ELT48.022 <- clean_species(ELT48.022) %>% replace_na_with_zero()
ELT48.022 <- ELT48.022 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT48.022 <- ELT48.022 %>% select(-c(
  "G. ruber (sum of G. ruber pink and G. r...)",
  "G. menardii (sum of G. menardii, G. tumida...)"
))

## ------------------

ELT48.027 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT48.027-PC.tab") %>%
  mutate(Event = "ELT48.027-PC", Latitude = -38.542000, Longitude = 79.898000, .before = 1)

names(ELT48.027) <- gsub(" [#]", "", names(ELT48.027), fixed = T)

ELT48.027 <- merge_morphotypes(ELT48.027, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
ELT48.027 <- ELT48.027 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT48.027 <- clean_species(ELT48.027) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)"
  ))

## ------------------

ELT45.029 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT45.029-PC.tab") %>%
  mutate(Event = "ELT48.027-PC", Latitude = -44.877000, Longitude = 106.518000, .before = 1)

ELT45.029 <- ELT45.029 %>% select(1:4, 7, 15:33)
names(ELT45.029) <- gsub(" [%]", "", names(ELT45.029), fixed = T)


ELT45.029 <- merge_morphotypes(ELT45.029, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
ELT45.029 <- ELT45.029 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT45.029 <- clean_species(ELT45.029) %>% replace_na_with_zero()
## convert to absolute abundance
ELT45.029 <- ELT45.029 %>% mutate_at(vars(`G. eastropacia`:`G. truncatulinoides`), ~ as.integer(.x * `Foram plankt [#]` / 100))
ELT45.029 <- ELT45.029 %>% select(-`Foram plankt [#]`)

## ------------------

RC13.152 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-152.tab") %>%
  mutate(Event = "RC13-152", Latitude = 16.708000, Longitude = -75.440000, .before = 1)
names(RC13.152) <- gsub(" [#]", "", names(RC13.152), fixed = T)
RC13.152 <- clean_species(RC13.152) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)",
    "G. sacculifer wo sac",
    "G. sacculifer sac",
    "Foram plankt oth"
  ))

RC13.152 <- RC13.152 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.152 <- merge_morphotypes(RC13.152, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")

## ------------------

RC13.153 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-153.tab") %>%
  mutate(Event = "RC13-152", Latitude = 15.065000, Longitude = -75.950000, .before = 1)
names(RC13.153) <- gsub(" [#]", "", names(RC13.153), fixed = T)
RC13.153 <- clean_species(RC13.153) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)",
    "G. sacculifer wo sac",
    "G. sacculifer sac",
    "Foram plankt oth"
  ))
RC13.153 <- RC13.153 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.153 <- merge_morphotypes(RC13.153, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")

## ------------------
RC13.158 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-158.tab") %>%
  mutate(Event = "RC13-152", Latitude = 13.178000, Longitude = -79.830000, .before = 1)
names(RC13.158) <- gsub(" [#]", "", names(RC13.158), fixed = T)
RC13.158 <- clean_species(RC13.158) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)",
    "Sample comment (replicate)",
    "G. sacculifer wo sac",
    "G. sacculifer sac",
    "Foram plankt oth",
  ))
RC13.158 <- RC13.158 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.158 <- merge_morphotypes(RC13.158, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")

## ------------------

RE5.034 <- read_tsv("raw/CLIMAP/manually_downloaded/RE5-034.tab") %>%
  mutate(Event = "RE5-034", Latitude = 42.383300, Longitude = -21.966700, .before = 1)
names(RE5.034) <- gsub(" [#]", "", names(RE5.034), fixed = T)
RE5.034 <- clean_species(RE5.034) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber", "G. sacculifer wo sac",
    "G. sacculifer sac"
  ))
RE5.034 <- merge_morphotypes(RE5.034, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")

## ------------------

V04.8 <- read_tsv("raw/CLIMAP/manually_downloaded/V04-8.tab") %>%
  mutate(Event = "V04-8", Latitude = 37.233000, Longitude = 33.130000, .before = 1)
names(V04.8) <- gsub(" [#]", "", names(V04.8), fixed = T)
V04.8 <- clean_species((V04.8)) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)",
    "G. sacculifer wo sac", "G. sacculifer sac",
    "Foram plankt oth"
  ))
V04.8 <- V04.8 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
V04.8 <- merge_morphotypes(V04.8, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")


## ------------------
V04.32 <- read_tsv("raw/CLIMAP/manually_downloaded/V04-32.tab") %>%
  mutate(Event = "V04-32", Latitude = 35.050000, Longitude = -11.620000, .before = 1)
names(V04.32) <- gsub(" [#]", "", names(V04.32), fixed = T)
V04.32 <- clean_species((V04.32)) %>%
  replace_na_with_zero() %>%
  select(-c(
    "G. ruber (sum of G. ruber pink and G. r...)",
    "G. menardii (sum of G. menardii, G. tumida...)",
    "G. sacculifer wo sac",
    "G. sacculifer sac",
    "Foram plankt oth",
    "Sample comment (replicate)"
  ))
V04.32 <- V04.32 %>% revise_sp_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
V04.32 <- merge_morphotypes(V04.32, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")


### merge all the dataframes
## convert to long format first
CHN82.24 <- pivot_longer(CHN82.24,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
ELT48.022 <- pivot_longer(ELT48.022,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
ELT48.027 <- pivot_longer(ELT48.027,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
ELT45.029 <- pivot_longer(ELT45.029,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
RC13.152 <- pivot_longer(RC13.152,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
RC13.153 <- pivot_longer(RC13.153,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
RC13.158 <- pivot_longer(RC13.158,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
RE5.034 <- pivot_longer(RE5.034,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
V04.8 <- pivot_longer(V04.8,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)
V04.32 <- pivot_longer(V04.32,
  cols = -c(Event, Longitude, Latitude, `Depth sed [m]`),
  names_to = "Species", values_to = "Absolute Abundance"
)

## merge all
climap_source3 <- rbind(CHN82.24, ELT48.022, ELT48.027, ELT45.029, RC13.152, RC13.153, RC13.158, RE5.034, V04.8, V04.32)

## export to csv
write_csv(climap_source3, "sp/lgm_climap3_sp_a.csv")

## export to functional group
climap_source3 %>%
  global_group_and_aggregate(Depth = "Depth sed [m]") %>%
  write_csv("fg/lgm_climap3_fg_a.csv")
