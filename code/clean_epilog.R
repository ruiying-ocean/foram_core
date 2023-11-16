## EPILOG is provided in [relative abundance] x [total counts] format
epilog <- read_csv("raw/EPILOG.csv") %>% select(!`Elevation [m]`)
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
epilog <- epilog %>% replace_column_name( "G. menardii" , "G. cultrata")
epilog <- epilog %>% replace_column_name( "G. bradyi" , "G. uvula")
epilog <- epilog %>% replace_column_name( "G. quinqueloba", "T. quinqueloba")
epilog <- epilog %>% replace_column_name( "G. digitata", "B. digitata")
epilog <- epilog %>% replace_column_name( "G. theyeri", "G. eastropacia")
epilog <- epilog %>% replace_column_name( "G. aequilateralis", "G. siphonifera")

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
## export
epilog_r %>% global_group_and_aggregate(Depth = 'Depth [m]') %>% write_csv("fg/lgm_epilog_fg_r.csv")
epilog_a %>%global_group_and_aggregate(Depth = 'Depth [m]') %>% write_csv("fg/lgm_epilog_fg_a.csv")
