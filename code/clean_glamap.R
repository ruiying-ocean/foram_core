## glamap is in relative abundance only
glamap <- read_csv("raw/GLAMAP.csv")%>% replace_na_with_zero()

## -----------------
## clean species name
## -----------------

names(glamap) <- gsub(" [%]", "", names(glamap), fixed=T)

glamap <- merge_morphotypes(glamap, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")

glamap <- glamap %>% select(!c("G. mentum", "D. grahami", "G. quinqueloba s", "G. quinqueloba d"))
glamap <- glamap %>% revise_sp_name("G. quinqueloba", "T. quinqueloba")

glamap <- glamap %>% revise_sp_name("G. trilobus sac", "T. sacculifer")
glamap <- glamap %>% revise_sp_name("G. trilobus tril", "T. sacculifer")
glamap <- glamap %>% revise_sp_name("G. sacculifer", "T. sacculifer")

glamap <- merge_morphotypes(glamap, c("P/D int...44","P/D int...26","N. pachyderma d"), "N. incompta")

glamap <- glamap %>% clean_species()

glamap <- glamap %>% mutate_at(vars(`G. eastropacia`:`N. incompta`), ~ . /100)
glamap <- glamap %>% pivot_longer(cols=c(`G. eastropacia`:`N. incompta`), names_to = "Species", values_to="Relative Abundance")
glamap <- glamap %>% distinct()
write_csv(glamap, "sp/lgm_glamap_sp_r.csv")

## -----------------
## Species to groups
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
##join two tables                                    
glamap_merged <- merge(glamap, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))

## Each unique sample and sediment depth as a group
## i.e., allow different depths in one sample to exist
glamap_merged <- glamap_merged %>% group_by(Campaign, Event, Latitude, Longitude,  `Depth [m]`, `Date/Time`, `Elevation [m]`,  Symbiosis, Spine) %>% 
    summarise_all(.funs = sum, na.rm=T) %>% ungroup()

## export
write_csv(glamap_merged, "fg/lgm_glamap_fg_r.csv")
