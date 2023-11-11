## -----------------
## clean species name
## -----------------
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
##  relative abundance
## -----------------

## convert to decimal (0-1) not percentage (0-100) format
climap_lgm_r <- climap_lgm %>% mutate(total = rowSums(across(`O. universa`:`G. truncatulinoides`))) %>%
  mutate(across(`O. universa`:`G. truncatulinoides`, ~ ./ 100)) %>%
  pivot_longer(cols=c(`O. universa`:`G. truncatulinoides`), 
               names_to = "Species", values_to="Relative Abundance")

fwrite(climap_lgm_r, "sp/lgm_climap_sp_r.csv")

## -----------------
## absolute abundance source
## -----------------

foram_dat <- read_csv("https://www.ncei.noaa.gov/pub/data/paleo/paleocean/climap/climap18/forams.txt")

## replace -999 to 0
foram_dat <- foram_dat %>% mutate(across(`Foram1`:`Foram45`, ~ replace(., .==-999, 0)))

## get the total foram count for each sample
## and convert depth from cm to m
foram_dat <- foram_dat %>% 
  mutate(total_count = rowSums(across(`Foram1`:`Foram45`)),
         Depth = Depth/100) %>%
  select(CoreID, Depth, total_count)

## average if CoreID and Depth are the same
foram_dat <- foram_dat %>% group_by(CoreID, Depth) %>% 
  summarise(total_count = mean(total_count)) %>% ungroup()

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
  mutate(New_CoreID = map_chr(CoreID, core_id_formatter)) %>%
  select(-CoreID)


## -----------------
## merge data and convert to absolute abundance
## -----------------

## convert relative abundance to absolute abundance
climap_lgm_a <- left_join(climap_lgm, foram_dat, by=c("Event"="New_CoreID", "Depth sed"="Depth"))

climap_lgm_a <- climap_lgm_a %>% 
  mutate(across(`O. universa`:`G. truncatulinoides`, ~ as.integer(.x * total_count/100))) %>%
  select(-total_count)

## wide to long data format
climap_lgm_a <- climap_lgm_a %>% pivot_longer(cols=c(`O. universa`:`G. truncatulinoides`), 
                                    names_to = "Species", values_to="Absolute Abundance")

## save to csv
fwrite(climap_lgm_a, "sp/lgm_climap_sp_a.csv")

## -----------------
## group spcies
## -----------------

climap_lgm_r %>% global_group_and_aggregate(Depth='Depth sed') %>% write_csv(., "fg/lgm_climap_fg_r.csv")
climap_lgm_a %>% global_group_and_aggregate(Depth='Depth sed') %>% write_csv(., "fg/lgm_climap_fg_a.csv")
