## mix1999 provides relative abundance only
mix1999 <- read_csv("raw/LGM_MIX1999.csv")%>% replace_na_with_zero()

## -----------------
## clean species name
## -----------------
names(mix1999) <- gsub(" [%]", "", names(mix1999), fixed=T)
mix1999 <- mix1999 %>% replace_column_name("N. pachyderma d",  "N. pachyderma")
mix1999 <- mix1999 %>% replace_column_name("N. pachyderma s",  "N. incompta")
mix1999 <- mix1999 %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
mix1999 <- mix1999 %>% replace_column_name("G. hexagona", "G. hexagonus")
mix1999 <- mix1999 %>% replace_column_name( "G. truncatulinoides s","G. truncatulinoides")
mix1999 <- mix1999 %>% replace_column_name( "G. truncatulinoides d","G. truncatulinoides")
mix1999 <- mix1999 %>% replace_column_name( "G. sacculifer","T. sacculifer")

mix1999 <- mix1999 %>% mutate_at(vars(`O. universa`:`G. glutinata`), ~ . /100)
write_csv(mix1999, "sp/lgm_mix1999_sp_r.csv")

## -----------------
## species to group
## -----------------
mix1999 <- mix1999 %>% pivot_longer(cols=c(`O. universa`:`G. glutinata`), names_to = "Species", values_to="Relative Abundance")

# aggregate functional group's abundance
mix1999_merged <- merge(mix1999, symbiosis_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species, Species.y))
mix1999_merged <- mix1999_merged %>% dplyr::filter(Symbiosis != "Undetermined" & Spinose != 'Undetermined')

## 1 Each unique sample to be a group
mix1999_merged <- mix1999_merged %>% group_by(Event, Latitude, Longitude, `Elevation [m]`, `Depth [m]`,`SST (1-12) [°C]`, Symbiosis, Spinose) %>% 
    summarise_all(.funs = sum, na.rm=T) %>% ungroup()

## 2 average different `Depth [m]`
mix1999_merged <- mix1999_merged %>% group_by(Event, Latitude, Longitude, `Elevation [m]`, `SST (1-12) [°C]`, Symbiosis, Spinose) %>% 
  summarise_all(.funs = mean, na.rm=T) %>% ungroup()

write_csv(mix1999_merged, "fg/lgm_mix1999_fg_r.csv")
