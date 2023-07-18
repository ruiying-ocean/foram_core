## glamap is in relative abundance only
glamap <- read_csv("raw/GLAMAP.csv")%>% replace_na_with_zero()

## -----------------
## clean species name
## -----------------

names(glamap) <- gsub(" [%]", "", names(glamap), fixed=T)

glamap <- glamap %>% select(!c("P/D int...44","P/D int...26","G. mentum", "D. grahami"))
glamap <- glamap %>% replace_column_name("N. pachyderma d", "N. incompta")
glamap <- glamap %>% replace_column_name("G. ruber p", "G. ruber ruber")
glamap <- glamap %>% replace_column_name("G. quinqueloba d", "T. quinqueloba")
glamap <- glamap %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
glamap <- glamap %>% replace_column_name("G. trilobus sac", "G. sacculifer")
glamap <- glamap %>% replace_column_name("G. trilobus tril", "G. clavaticamerata")
glamap <- glamap %>% replace_column_name("G. truncatulinoides d", "G. truncatulinoides")
glamap <- glamap %>% replace_column_name("G. aequilateralis", "G. siphonifera")
glamap <- glamap %>% replace_column_name("G. digitata", "B. digitata")
glamap <- glamap %>% replace_column_name("G. quinqueloba s", "T. quinqueloba")
glamap <- glamap %>% replace_column_name("G. truncatulinoides s", "G. truncatulinoides")
glamap <- glamap %>% replace_column_name("N. pachyderma s", "N. pachyderma")
glamap <- glamap %>% replace_column_name("G. ruber w", "G. ruber albus")
glamap <- glamap %>% replace_column_name("G. menardii", "G. cultrata")

glamap <- glamap %>% mutate_at(vars(`G. siphonifera`:`N. pachyderma`), ~ . /100)
glamap <- glamap %>% pivot_longer(cols=c(`G. siphonifera`:`N. pachyderma`), names_to = "Species", values_to="Relative Abundance")
glamap <- glamap %>% distinct()
write_csv(glamap, "sp/lgm_glamap_sp_r.csv")

## -----------------
## Species to groups
## -----------------
symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
##join two tables                                    
glamap_merged <- merge(glamap, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
glamap_merged <- glamap_merged %>% dplyr::filter(Symbiosis != "Undetermined" & Spinose != 'Undetermined')
# aggregate functional group abundance and divided by 100

## 1 Each unique sample to be a group
glamap_merged <- glamap_merged %>% group_by(Campaign, Event, Latitude, Longitude,  `Depth [m]`, `Date/Time`, `Elevation [m]`,  Symbiosis, Spinose) %>% 
    summarise_all(.funs = sum, na.rm=T) %>% ungroup()

## 2 average different `Depth [m]`
glamap_merged <- glamap_merged %>% group_by(Campaign, Event, Latitude, Longitude, `Date/Time`, `Elevation [m]`,  Symbiosis, Spinose) %>% 
    summarise_all(.funs = mean, na.rm=T) %>% ungroup()

## export
write_csv(glamap_merged, "fg/lgm_glamap_fg_r.csv")
