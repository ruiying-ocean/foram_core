## EPILOG is provided in [relative abundance] x [total counts] format
epilog <- read_csv("raw/EPILOG.csv") %>% select(!`Elevation [m]`)
names(epilog) <- gsub(" [%]", "", names(epilog), fixed = T)

## ---------------------------------
## Rename species
## ---------------------------------

epilog <- epilog %>% select(!c("G. ruber hsp"))
epilog <- epilog %>% revise_sp_name("P/D int", "N. incompta")
epilog <- epilog %>% revise_sp_name("G. rubescens white", "G. rubescens")
epilog <- epilog %>% revise_sp_name("G. rubescens pink", "G. rubescens")
epilog <- merge_morphotypes(epilog, c("G. truncatulinoides d", "G. truncatulinoides s"), "G. truncatulinoides")
epilog <- merge_morphotypes(epilog, c("G. sacculifer wo sac", "G. sacculifer sac"), "T. sacculifer")
epilog <- epilog %>% revise_sp_name("G. menardii flexuosa", "G. cultrata")
epilog <- epilog %>% clean_species()

## put `Foram plankt [#]` at the end
epilog <- epilog %>% relocate("Foram plankt [#]", .after = "T. sacculifer")
find_missing_species(symbiosis_tbl$short_name, names(epilog))
## ---------------------------------
## Convert Relative Abundance to Absolute Abundance
## ---------------------------------

epilog_a <- epilog %>%
  mutate_at(vars(`G. bulloides`:`T. sacculifer`), ~ as.integer(. * `Foram plankt [#]` / 100)) %>%
  drop_na(`Foram plankt [#]`) %>%
  select(!`Foram plankt [#]`)

## wide to long format
epilog_a <- epilog_a %>% pivot_longer(`G. bulloides`:`T. sacculifer`, names_to = "Species", values_to = "Absolute Abundance")

epilog_r <- epilog %>%
  mutate_at(vars(`G. bulloides`:`T. sacculifer`), ~ . / 100) %>%
  select(!`Foram plankt [#]`)

## wide to long format
epilog_r <- epilog_r %>% pivot_longer(`G. bulloides`:`T. sacculifer`, names_to = "Species", values_to = "Relative Abundance")

fwrite(epilog_a, "sp/lgm_epilog_sp_a.csv")
fwrite(epilog_r, "sp/lgm_epilog_sp_r.csv")

## ---------------------------------
## Species into functional group
## ---------------------------------
## export
epilog_r %>%
  global_group_and_aggregate(Depth = "Depth [m]") %>%
  write_csv("fg/lgm_epilog_fg_r.csv")
epilog_a %>%
  global_group_and_aggregate(Depth = "Depth [m]") %>%
  write_csv("fg/lgm_epilog_fg_a.csv")
