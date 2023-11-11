library(stringr)

## read symbiosis table
symbiosis_tbl <- fread('fg/foram_taxonomy.csv')

setnames(symbiosis_tbl, "Species name","Species")

## remove column
symbiosis_tbl <- symbiosis_tbl[, -c('Author')]

## replace white space by underscore (_)
## symbiosis_tbl[, "Species" := lapply(.SD, function(x) gsub(" ", "_", x)),
##              .SDcol="Species"]

symbiosis_tbl$Species <- gsub("_", " ",  symbiosis_tbl$Species)

## Add a abbreviation column
species_abbrev <- function(full_name, sep_string = ". ") {
  name_parts <- str_split(full_name, " ")[[1]]
  genus_name <- name_parts[1]
  species_name <- name_parts[2]
  
  if (length(name_parts) > 2) {
    subspecies_name <- name_parts[3]
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
    abbrev <- paste(abbrev, subspecies_name, sep=" ")
  } else {
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
  }
  
  return(abbrev)
}


symbiosis_tbl[, short_name := mapply(species_abbrev, Species)][]

find_missing_species <- function(all_species, species_to_check) {
  not_found_species <- species_to_check[!(species_to_check %in% all_species)]
  if (length(not_found_species) > 0){
    return(not_found_species)
  } else{
    print("All species included!")
  }
}

global_group_and_aggregate <- function(data, Depth){
  
  symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
  data <- merge(data, symbiosis_short_tbl, by.x="Species", by.y = "short_name") %>% select(!c(Species))
  
  data <- data %>% group_by(Event, Latitude, Longitude, !!sym(Depth), Symbiosis, Spine) %>% 
    summarise_all(.funs = sum, na.rm=T)  %>% ungroup()
  
  data <- data %>% group_by(Event, Latitude, Longitude, Symbiosis, Spine) %>%
    summarise_all(.funs = mean, na.rm=T)  %>% ungroup()
  
  return(data)
}

