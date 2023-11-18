## climap cores
## included_cores <- c(unique(climap_lgm_a$Event),unique(foram_dat_lgm$Event))
## this file seems to be identical to the source1 (CLIMAP_LGM_foram_data.tab)
## mix1999 <- read_tsv("raw/CLIMAP/Mix_et_al_1999.tab")
## find out those not included here
## mix1999%>% dplyr::filter(!Event %in% included_cores) %>% pull(Event) %>% unique()
## lgm_co3%>% dplyr::filter(!Event %in% included_cores) %>% pull(Event) %>% unique()


## clean species
clean_species <- function(data) {
  data <- data %>%
    replace_column_name("G. ruber p", "G. ruber ruber") %>%
    replace_column_name("G. ruber w", "G. ruber albus") %>%
    replace_column_name("G. sacculifer", "T. sacculifer") %>%
    replace_column_name("N. pachyderma s", "N. pachyderma") %>%
    replace_column_name("N. pachyderma d", "N. incompta") %>%
    replace_column_name("G. menardii", "G. cultrata") %>%
    replace_column_name("G. aequilateralis", "G. eastropacia") %>%
    replace_column_name("G. digitata", "B. digitata") %>%
    replace_column_name("G. anfracta", "D. anfracta") %>%
    replace_column_name("G. quinqueloba", "T. quinqueloba") %>%
    replace_column_name("G. humilis", "T. humilis") %>%
    replace_column_name("G. hexagona", "G. hexagonus") %>%
    replace_column_name("G. dutertrei","N. dutertrei")%>%
    replace_column_name("G. bradyi", "G. uvula") %>%
    replace_column_name("G. pumilio", "B. pumilio") %>%
    replace_column_name("G. iota", "T. iota") %>%    
    mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
  
  return(data)
}

## read data
## these are already age filtered manually, with age references from lgm_co3/climap_lgm data.frames
## ------------------
CHN82.24 <- read_tsv("raw/CLIMAP/manually_downloaded/CHN82-24.tab") %>% 
  mutate(Event="CHN82-24", Latitude=43.465000,Longitude=-32.850000, .before=1)

names(CHN82.24) <- gsub(" [#]", "", names(CHN82.24), fixed=T)
## remove G. ruber and G. pachyderma
CHN82.24 <- CHN82.24 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`)) 
CHN82.24 %>% select(-c("G. ruber", "G. pachyderma","G. sacculifer sac","G. sacculifer wo sac")) -> CHN82.24
## combine morphtypes of G. truncatulinoides
CHN82.24 <- CHN82.24 %>% clean_species()

## ------------------

ELT48.022 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT48.022-PC.tab") %>%
  mutate(Event="ELT48.022-PC", Latitude=39.895000,Longitude=-85.410000, .before=1)
names(ELT48.022) <- gsub(" [#]", "", names(ELT48.022), fixed=T)

ELT48.022 <- ELT48.022 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`)) 
ELT48.022 <- ELT48.022 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT48.022 <- clean_species(ELT48.022) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                    "G. menardii (sum of G. menardii, G. tumida...)",
                                                    "G. pachyderma",
                                                    "G. menardii (sum of G. menardii, G. tumida...)",
                                                    "G. tumida flexuosa"))
## ------------------


ELT48.027 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT48.027-PC.tab") %>%
  mutate(Event="ELT48.027-PC", Latitude=-38.542000,Longitude=79.898000, .before=1)

names(ELT48.027) <- gsub(" [#]", "", names(ELT48.027), fixed=T)

ELT48.027 <- ELT48.027 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
    select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))
ELT48.027 <- ELT48.027 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT48.027 <- clean_species(ELT48.027) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                    "G. menardii (sum of G. menardii, G. tumida...)",
                                                    "G. pachyderma",
                                                    "G. menardii (sum of G. menardii, G. tumida...)",
                                                    "G. tumida flexuosa"))    

## ------------------

ELT45.029 <- read_tsv("raw/CLIMAP/manually_downloaded/ELT45.029-PC.tab") %>%
    mutate(Event="ELT48.027-PC", Latitude=-44.877000,Longitude=106.518000, .before=1)

ELT45.029 <- ELT45.029 %>% select(1:4,7,15:33)
names(ELT45.029) <- gsub(" [%]", "", names(ELT45.029), fixed=T)

ELT45.029 <- ELT45.029 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
    select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))
ELT45.029 <- ELT45.029 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
ELT45.029 <- clean_species(ELT45.029)
## convert to absolute abundance
ELT45.029 <- ELT45.029 %>% mutate_at(vars(`G. eastropacia`:`G. eastropacia`), ~ as.integer(.x*`Foram plankt [#]`/100))
ELT45.029 <- ELT45.029 %>% select(-`Foram plankt [#]`)

## ------------------

RC13.152 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-152.tab") %>%
  mutate(Event="RC13-152", Latitude=16.708000,Longitude= -75.440000, .before=1)
names(RC13.152) <- gsub(" [#]", "", names(RC13.152), fixed=T)
RC13.152 <- clean_species(RC13.152) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "G. pachyderma",
                                                  "G. sacculifer wo sac",
                                                  "G. sacculifer sac",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "Foram plankt oth",
                                                  "G. tumida flexuosa"))    


RC13.152 <- RC13.152 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.152 <- RC13.152 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))

## ------------------

RC13.153 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-153.tab") %>%
  mutate(Event="RC13-152", Latitude=15.065000,Longitude= -75.950000, .before=1)
names(RC13.153) <- gsub(" [#]", "", names(RC13.153), fixed=T)
RC13.153 <- clean_species(RC13.153) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "G. pachyderma",
                                                  "G. sacculifer wo sac",
                                                  "G. sacculifer sac",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "Foram plankt oth",
                                                  "G. tumida flexuosa"))  
RC13.153 <- RC13.153 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.153 <- RC13.153 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))


## ------------------
RC13.158 <- read_tsv("raw/CLIMAP/manually_downloaded/RC13-158.tab") %>%
  mutate(Event="RC13-152", Latitude=13.178000 ,Longitude= -79.830000, .before=1)
names(RC13.158) <- gsub(" [#]", "", names(RC13.158), fixed=T)
RC13.158 <- clean_species(RC13.158) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "G. pachyderma",
                                                  "Sample comment (replicate)",
                                                  "G. sacculifer wo sac",
                                                  "G. sacculifer sac",
                                                  "G. menardii (sum of G. menardii, G. tumida...)",
                                                  "Foram plankt oth",
                                                  "G. tumida flexuosa"))  
RC13.158 <- RC13.158 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
RC13.158 <- RC13.158 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))
## ------------------

RE5.034 <- read_tsv("raw/CLIMAP/manually_downloaded/RE5-034.tab") %>%
  mutate(Event="RE5-034", Latitude= 42.383300,Longitude= -21.966700, .before=1)
names(RE5.034) <- gsub(" [#]", "", names(RE5.034), fixed=T)
RE5.034 <-  clean_species(RE5.034) %>% select(-c("G. ruber",
                                                              "G. pachyderma",
                                                              "G. sacculifer wo sac",
                                                              "G. sacculifer sac"))
RE5.034 <- RE5.034 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))
## ------------------

V04.8 <- read_tsv("raw/CLIMAP/manually_downloaded/V04-8.tab") %>%
  mutate(Event="V04-8", Latitude=37.233000,Longitude= 33.130000, .before=1)
names(V04.8) <- gsub(" [#]", "", names(V04.8), fixed=T)
V04.8 <-  clean_species((V04.8)) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                                 "G. pachyderma",
                                               "G. menardii (sum of G. menardii, G. tumida...)",
                                                 "G. sacculifer wo sac",
                                               "G. tumida flexuosa",
                                               "Foram plankt oth"  ,
                                                 "G. sacculifer sac"))
V04.8 <- V04.8 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
V04.8 <- V04.8 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))


## ------------------
V04.32 <- read_tsv("raw/CLIMAP/manually_downloaded/V04-32.tab") %>%
  mutate(Event="V04-32", Latitude=35.050000,Longitude= -11.620000, .before=1)
names(V04.32) <- gsub(" [#]", "", names(V04.32), fixed=T)
V04.32 <-  clean_species((V04.32)) %>% select(-c("G. ruber (sum of G. ruber pink and G. r...)",
                                               "G. pachyderma",
                                               "G. menardii (sum of G. menardii, G. tumida...)",
                                               "G. sacculifer wo sac",
                                               "G. tumida flexuosa",
                                               "Foram plankt oth"  ,
                                               "Sample comment (replicate)",
                                               "G. sacculifer sac"))
V04.32 <- V04.32 %>% replace_column_name("G. sacculifer (sum of G. sacculifer no sac a...)", "T. sacculifer")
V04.32 <- V04.32 %>% mutate(`G. truncatulinoides`=`G. truncatulinoides d`+`G. truncatulinoides s`) %>% 
  select(-c(`G. truncatulinoides d`,`G. truncatulinoides s`))

### merge all the dataframes
## convert to long format first
CHN82.24 <- pivot_longer(CHN82.24, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`), 
                         names_to="Species", values_to="Absolute Abundance")
ELT48.022 <- pivot_longer(ELT48.022, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`), 
                         names_to="Species", values_to="Absolute Abundance")
ELT48.027 <- pivot_longer(ELT48.027, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
ELT45.029 <- pivot_longer(ELT45.029, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
RC13.152  <- pivot_longer(RC13.152, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
RC13.153  <- pivot_longer(RC13.153, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
RC13.158  <- pivot_longer(RC13.158, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
RE5.034   <- pivot_longer(RE5.034, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
V04.8 <- pivot_longer(V04.8, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")
V04.32 <- pivot_longer(V04.32, cols=-c(Event, Longitude,Latitude, `Depth sed [m]`),
                          names_to="Species", values_to="Absolute Abundance")

## merge all
climap_source3 <- rbind(CHN82.24,ELT48.022,ELT48.027,ELT45.029,RC13.152,RC13.153,RC13.158,RE5.034,V04.8,V04.32)

## export to csv
write_csv(climap_source3, "sp/lgm_climap3_sp_a.csv")

## export to functional group
climap_source3 %>% global_group_and_aggregate(Depth='Depth sed [m]') %>%
  write_csv("fg/lgm_climap3_fg_a.csv")
