## This script is to clean the CLIMAP data in the LGM section

## ----------------------------------
## Data source 1: 
## LGM relative abundance compilation
## ----------------------------------

## data is pre-downloaded from pangaea
climap_lgm <- read_tsv("raw/CLIMAP/CLIMAP_LGM_foram_data.tab")

## clean unnecessary string in the species name
names(climap_lgm) <- gsub(" [%]", "", names(climap_lgm), fixed=T)
names(climap_lgm) <- gsub(" [m]", "", names(climap_lgm), fixed=T)

## replace species names
climap_lgm <- climap_lgm %>% replace_column_name("G. sacculifer", "T. sacculifer")
climap_lgm <- climap_lgm %>% replace_column_name("N. pachyderma s", "N. pachyderma")
climap_lgm <- climap_lgm %>% replace_column_name("N. pachyderma d", "N. incompta")
climap_lgm <- climap_lgm %>% replace_column_name("G. digitata", "B. digitata")
climap_lgm <- climap_lgm %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
climap_lgm <- climap_lgm %>% replace_column_name("G. ruber", "G. ruber albus")
climap_lgm <- climap_lgm %>% replace_column_name("G. hexagona", "G. hexagonus")
climap_lgm <- climap_lgm %>% replace_column_name("G. aequilateralis", "G. siphonifera")

## combine G. truncatulinoides s and d
climap_lgm <- climap_lgm %>% 
  mutate(`G. truncatulinoides` = `G. truncatulinoides s` + `G. truncatulinoides d`) %>%
  select(-`G. truncatulinoides s`, -`G. truncatulinoides d`)

find_missing_species(symbiosis_tbl$short_name, names(climap_lgm))

## -----------------
##  export relative abundance
## -----------------

## convert to decimal (0-1) not percentage (0-100) format
climap_lgm_r <- climap_lgm %>% mutate(total = rowSums(across(`O. universa`:`G. truncatulinoides`))) %>%
  mutate(across(`O. universa`:`G. truncatulinoides`, ~ ./ 100)) %>%
  pivot_longer(cols=c(`O. universa`:`G. truncatulinoides`), 
               names_to = "Species", values_to="Relative Abundance")

fwrite(climap_lgm_r, "sp/lgm_climap_sp_r.csv")

## ----------------------------------
## Data source 2: 
## All absolute abundance collection
## ----------------------------------

## this is raw data in CLIMAP, not just LGM part
foram_dat <- read_csv("https://www.ncei.noaa.gov/pub/data/paleo/paleocean/climap/climap18/forams.txt")

## replace -999 to 0
foram_dat <- foram_dat %>% mutate(across(`Foram1`:`Foram45`, ~ replace(., .==-999, 0)))

## get the total foram count for each sample
## and convert depth from cm to m
foram_dat <- foram_dat %>% 
  mutate(total_count = rowSums(across(`Foram1`:`Foram45`)),
         Depth = Depth/100)

## change species name
names(foram_dat)[3:47] <- read_csv("raw/CLIMAP/foram_sp_list.txt")$Species

## clean species name
foram_dat <- foram_dat %>% select(-c('G. ruber total', 'T. sacculifer wo sac', 'T. sacculifer w sac',
                                     'N. pachyderma--N. dutertrei','G. tumida+flexuosa',
                                     'G. cultrata+tumida+flexuosa','Others'))

foram_dat <- foram_dat %>% replace_column_name('N. pachyderma left', 'N. pachyderma')
foram_dat <- foram_dat %>% replace_column_name('N. pachyderma right', 'N. incompta')
foram_dat <- foram_dat %>% replace_column_name('T. sacculifer total', 'T. sacculifer')
foram_dat <- foram_dat %>% replace_column_name('G. hexagona', 'G. hexagonus')

## combine G. trunc sin/dex
foram_dat <- foram_dat %>% 
  mutate(`G. truncatulinoides` = `G. truncatulinodes left` + `G. truncatulinodes right`) %>%
  select(-`G. truncatulinodes left`, -`G. truncatulinodes right`)

## format the CoreID column  
core_id_formatter <- function(core_id) {
  # Use regular expression to extract the three groups
  match_result <- regexec('([A-Za-z]+)(\\d{3})(\\d{3})', core_id)
  
  if (length(match_result[[1]]) > 1) {
    # Extract matched groups
    letter_group <- regmatches(core_id, match_result)[[1]][2]
    first_three_numbers <- regmatches(core_id, match_result)[[1]][3]
    last_three_numbers <- regmatches(core_id, match_result)[[1]][4]
    
    # re-format the data
    ## remove the leading 0 in number groups
    first_three_numbers <- as.numeric(first_three_numbers)
    last_three_numbers <- as.numeric(last_three_numbers)
    new_id <- paste0(letter_group, first_three_numbers,'-', last_three_numbers)
    return(new_id)
  } else {
    # Return NA if it doesn't match the expected format
    return(rep(NA, 3))
  }
}

## map this function to the CoreID column
foram_dat <- foram_dat %>% 
  mutate(New_CoreID = map_chr(CoreID, core_id_formatter), .before=CoreID) %>%
  select(-CoreID)

## calcium carbonate concentrations data which contains the core ID and LGM depth
lgm_co3 <- read_tsv("raw/CLIMAP/CaCO3_LGM.tab") %>% 
  select(c("Event","Latitude","Longitude","Depth sed [m]",))

## merge both
foram_dat_lgm <- left_join(lgm_co3, foram_dat, by=c("Event"="New_CoreID", "Depth sed [m]"="Depth"))

## most cores don't have foram data, or just surface sediment data
## we need to drop these, and filter useful LGM samples out
foram_dat_lgm <- foram_dat_lgm %>% drop_na(total_count)

foram_dat_lgm <- foram_dat_lgm %>% select(-total_count) %>%
  pivot_longer(cols=c(`O. universa`:`G. truncatulinoides`), 
               names_to = "Species", values_to="Absolute Abundance")

foram_dat_lgm %>% fwrite("sp/lgm_climap2_sp_a.csv")

foram_dat_lgm %>% global_group_and_aggregate(Depth='Depth sed [m]') %>% write_csv(., "fg/lgm_climap2_fg_a.csv")

## -----------------
## convert data source 1 to absolute abundance
## -----------------

## convert relative abundance to absolute abundance
foram_dat_total_count <- foram_dat %>% select(New_CoreID, Depth, total_count)
climap_lgm_a <- left_join(climap_lgm, foram_dat_total_count, by=c("Event"="New_CoreID", "Depth sed"="Depth"))

subset_wo_count <- climap_lgm_a %>% dplyr::filter(is.na(total_count))

climap_lgm_a <- climap_lgm_a %>% 
  mutate(across(`O. universa`:`G. truncatulinoides`, ~ as.integer(.x * total_count/100))) %>%
  select(-total_count)

## wide to long data format
climap_lgm_a <- climap_lgm_a %>% pivot_longer(cols=c(`O. universa`:`G. truncatulinoides`), 
                                    names_to = "Species", values_to="Absolute Abundance") %>%
  drop_na(`Absolute Abundance`)

## save to csv
fwrite(climap_lgm_a, "sp/lgm_climap1_sp_a.csv")
## -----------------
## group spcies
## -----------------

climap_lgm_r %>% global_group_and_aggregate(Depth='Depth sed') %>% write_csv(., "fg/lgm_climap1_fg_r.csv")
climap_lgm_a %>% global_group_and_aggregate(Depth='Depth sed') %>% write_csv(., "fg/lgm_climap1_fg_a.csv")

