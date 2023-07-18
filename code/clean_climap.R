## CLIMAP is in the format of absolute abundance
climap <- read_csv("raw/CLIMAP.csv")
climap <- climap %>% dplyr::filter("Depth [m]" > 0.3)
## -----------------
## clean species name
## -----------------
## clean unnecessary string in the species name
names(climap) <- gsub(" [#]", "", names(climap), fixed=T)
names(climap) <- gsub(" [m]", "", names(climap), fixed=T)

## remove unncessary subspecies/morphtypes
climap <- climap %>% select(!c(`G. sacculifer sac`, `G. sacculifer wo sac`, `G. ruber total`,
                               `G. truncatulinoides d`, `G. truncatulinoides s`, `G. menardii and G. tumida`,
                               `G. truncatulinoides d`, `G. truncatulinoides s`))
climap <- climap %>% replace_column_name("G. sacculifer total", "T. sacculifer")
climap <- climap %>% replace_column_name("N. pachyderma s", "N. pachyderma")
climap <- climap %>% replace_column_name("N. pachyderma d", "N. incompta")
climap <- climap %>% replace_column_name("G. pachyderma", "N. pachyderma")
climap <- climap %>% replace_column_name("G. truncatulinoides total", "G. truncatulinoides")
climap <- climap %>% replace_column_name("G. ruber p", "G. ruber ruber")
climap <- climap %>% replace_column_name("G. ruber w", "G. ruber albus")
climap <- climap %>% replace_column_name("G. quinqueloba", "T. quinqueloba")
climap <- climap %>% replace_column_name("G. tumida flexuosa", "G. tumida")
climap <- climap %>% replace_column_name("G. hexagona", "G. hexagonus")
climap <- climap %>% replace_column_name("G. truncatulinoides", "T. truncatulinoides")
climap <- climap %>% replace_column_name("G. bradyi", "G. uvula")
climap <- climap %>% replace_column_name("G. anfracta", "D. anfracta")
climap <- climap %>% replace_column_name("G. menardii", "G. cultrata")
climap <- climap %>% replace_column_name("G. humilis", "T. humilis")
climap <- climap %>% replace_column_name("G. iota", "T. iota")
climap <- climap %>% replace_column_name("G. pumilio", "B. pumilio")

climap <- climap %>% select(!`Foram plankt oth`)

## -----------------
## calculate relative abundance
## -----------------

## Because all depths are 0, I manually set identifier to each 
climap <- climap %>% mutate(Depth=row_number())

## wide to long data format
climap_a <- climap %>% pivot_longer(cols=c(`O. universa`:`H. digitata`), 
                                    names_to = "Species", values_to="Absolute Abundance")

## already in decimal (0-1) not percentage (0-100) format
climap_r <- climap %>% mutate(total = rowSums(across(`O. universa`:`H. digitata`))) %>%
    mutate(across(`O. universa`:`H. digitata`, ~ ./ total)) %>%
    select(-total)%>%
    pivot_longer(cols=c(`O. universa`:`H. digitata`), 
                 names_to = "Species", values_to="Relative Abundance")
climap_r <- climap_r %>% dplyr::filter(`Relative Abundance` <= 1)

## export both
fwrite(climap_a, "sp/lgm_climap_sp_a.csv")
fwrite(climap_r, "sp/lgm_climap_sp_r.csv")

## -----------------
## group spcies
## -----------------

group_and_aggregate <- function(data){
    symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
    data <- merge(data, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
    data <- data %>% dplyr::filter(Symbiosis != "Undetermined" & Spinose != 'Undetermined')

    ## aggregate functional group abundance and divided by 100
    data <- data %>% group_by(Latitude, Longitude, Event, `Depth`, `Elevation`, Symbiosis, Spinose) %>% 
        summarise_all(.funs = sum, na.rm=T)  %>% ungroup()

   data <- data %>% group_by(Latitude, Longitude, Event, `Elevation`, Symbiosis, Spinose) %>%
      summarise_all(.funs = mean, na.rm=T)  %>% ungroup()

    return(data)
}

climap_r %>% group_and_aggregate() %>% write_csv(., "fg/lgm_climap_fg_r.csv")
climap_a %>% group_and_aggregate() %>% write_csv(., "fg/lgm_climap_fg_a.csv")
