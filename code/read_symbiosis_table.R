library(stringr)

## read symbiosis table
symbiosis_tbl <- fread('fg/foram_sp_db.csv')
symbiosis_tbl[, "Name" := lapply(.SD, function(x) gsub(" ", "_", x)),
              .SDcol="Name"] #replace white space by underscore(_)
symbiosis_tbl <- symbiosis_tbl[, -("Remark")] #delete comment column
symbiosis_tbl <- rbindlist(list(symbiosis_tbl, data.table(Name="Others",
                                                          Spinose="Undetermined",
                                                          Symbiosis="Undetermined")),
                           use.names = TRUE)
setnames(symbiosis_tbl, "Name", "Species")
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

