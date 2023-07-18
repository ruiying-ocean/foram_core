## EPILOG is provided in [relative abundance] x [total counts] format
epilog <- read_csv("raw/EPILOG.csv")
names(epilog) <- gsub(" [%]", "", names(epilog), fixed=T)

## ---------------------------------
## Rename species
## ---------------------------------

epilog <- epilog %>% select(!c("P/D int", "G. ruber hsp"))
epilog <- epilog %>% replace_column_name( "N. pachyderma d" ,  "N. incompta")
epilog <- epilog %>% replace_column_name( "N. pachyderma s" , "N. pachyderma")
epilog <- epilog %>% replace_column_name( "G. ruber p" , "G. ruber ruber")
epilog <- epilog %>% replace_column_name( "G. ruber w" , "G. ruber albus")
epilog <- epilog %>% replace_column_name( "G. rubescens white" , "G. rubescens")
epilog <- epilog %>% replace_column_name( "G. rubescens pink", "G. rubescens")
epilog <- epilog %>% replace_column_name( "G. truncatulinoides s" , "G. truncatulinoides")
epilog <- epilog %>% replace_column_name( "G. truncatulinoides d" , "G. truncatulinoides")
epilog <- epilog %>% replace_column_name( "G. sacculifer wo sac" , "T. sacculifer")
epilog <- epilog %>% replace_column_name( "G. sacculifer sac" , "T. sacculifer")
epilog <- epilog %>% replace_column_name( "G. menardii flexuosa" , "G. tumida")
epilog <- epilog %>% replace_column_name( "G. bradyi" , "G. uvula")
epilog <- epilog %>% replace_column_name( "G. quinqueloba", "T. quinqueloba")

## ---------------------------------
## Convert Relative Abundance to Absolute Abundance
## ---------------------------------

epilog_a <- epilog %>% mutate_at(vars(`G. bulloides`:`T. iota`), ~ as.integer(. * `Foram plankt [#]`/100)) %>%
  drop_na(`Foram plankt [#]`) %>% select(!`Foram plankt [#]`)

## wide to long format
epilog_a <- epilog_a %>% pivot_longer(`G. bulloides`:`T. iota`, names_to = "Species", values_to = "Absolute Abundance")

epilog_r <- epilog %>% mutate_at(vars(`G. bulloides`:`T. iota`), ~ . /100) %>%
 select(!`Foram plankt [#]`)

## wide to long format
epilog_r <- epilog_r %>% pivot_longer(`G. bulloides`:`T. iota`, names_to = "Species", values_to = "Relative Abundance")

fwrite(epilog_a, "sp/lgm_epilog_sp_a.csv")
fwrite(epilog_r, "sp/lgm_epilog_sp_r.csv")

## ---------------------------------
## Species into functional group
## ---------------------------------
group_and_aggregate <- function(data, value_name="Relative Abundance"){
    symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
    data <- merge(data, symbiosis_tbl, by.x="Species", by.y = "short_name") %>%
        select(-c("Species","Species.y"))
    data <- data %>% dplyr::filter(Symbiosis != "Undetermined" & Spinose != 'Undetermined')

    ## aggregate functional group abundance and divided by 100
    data <- data %>% group_by(Latitude, Longitude, Event, `Depth [m]`, `Elevation [m]`,  Symbiosis, Spinose) %>% 
        summarise_all(.funs = sum, na.rm=T) %>% ungroup()

    ## average through different depths (i.e., ages)
    data <- data %>% group_by(Latitude, Longitude, Event, `Elevation [m]`,  Symbiosis, Spinose) %>% 
        summarise_all(.funs = mean, na.rm=T) %>% ungroup()
    return(data)
}

## export
epilog_r %>% group_and_aggregate() %>% write_csv("fg/lgm_epilog_fg_r.csv")
epilog_a %>% group_and_aggregate() %>% write_csv("fg/lgm_epilog_fg_a.csv")
