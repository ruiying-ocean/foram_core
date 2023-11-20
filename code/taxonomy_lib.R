library(stringr)
library(dplyr)

## read symbiosis table
symbiosis_tbl <- fread("fg/foram_taxonomy.csv")

setnames(symbiosis_tbl, "Species name", "Species")

## remove column
symbiosis_tbl <- symbiosis_tbl[, -c("Author")]

## replace white space by underscore (_)
## symbiosis_tbl[, "Species" := lapply(.SD, function(x) gsub(" ", "_", x)),
##              .SDcol="Species"]

symbiosis_tbl$Species <- gsub("_", " ", symbiosis_tbl$Species)

## Add a abbreviation column
species_abbrev <- function(full_name, sep_string = ". ") {
  name_parts <- str_split(full_name, " ")[[1]]
  genus_name <- name_parts[1]
  species_name <- name_parts[2]

  if (length(name_parts) > 2) {
    subspecies_name <- name_parts[3]
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
    abbrev <- paste(abbrev, subspecies_name, sep = " ")
  } else {
    genus_abbrev <- str_sub(genus_name, 1, 1)
    abbrev <- paste(genus_abbrev, species_name, sep = sep_string)
  }

  return(abbrev)
}


symbiosis_tbl[, short_name := mapply(species_abbrev, Species)][]

find_missing_species <- function(all_species, species_to_check) {
  not_found_species <- species_to_check[!(species_to_check %in% all_species)]
  if (length(not_found_species) > 0) {
    return(not_found_species)
  } else {
    print("All species included!")
  }
}

global_group_and_aggregate <- function(data, Depth) {
  symbiosis_short_tbl <- symbiosis_tbl %>%
    select(!c(Species)) %>%
    distinct()
  data <- merge(data, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!c(Species))

  # allow different `Depth [m]` in one core sample to exist
  data <- data %>%
    group_by(Event, Latitude, Longitude, !!sym(Depth), Symbiosis, Spine) %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    ungroup()

  return(data)
}


## this function is used to revise species name
## but not for merging morphotypes
## e.g., do not use like revise_sp_name("T. sacculifer sac", "T. sacculifer")
## this actually add the morphotype to the total count and overestimate their abundance
revise_sp_name <- function(df, old_name, new_name) {
  # Check if the old column name exists in the data frame
  if (!old_name %in% colnames(df)) {
    cat("Column", old_name, "does not exist in the data frame\n")
    return(df)
  }

  # Check if the new column name already exists in the data frame
  if (new_name %in% colnames(df)) {
    ## if the new column name already exists, sum the two columns
    cat("Column", new_name, "already exists. Summing columns\n")
    df <- df %>%
      rowwise() %>%
      mutate(!!sym(new_name) := sum(!!sym(new_name), !!sym(old_name), na.rm = T)) %>%
      select(-!!sym(old_name))
  } else {
    # Replace the old column name with the new one
    df <- df %>% rename(!!new_name := !!sym(old_name))
  }

  return(df)
}


merge_morphotypes <- function(df, morpho_list, new_name) {
  # Check if the columns in morpho_list exist in the dataframe
  missing_columns <- setdiff(morpho_list, names(df))

  if (length(missing_columns) > 0) {
    cat("The following columns are missing in the dataframe:", paste(missing_columns, collapse = ", "), "\n")
    break
  } else {
    if (new_name %in% colnames(df)) {
      cat("Column", new_name, "already exists, temporarily resetting it to zero!\n")
      df <- df %>% mutate(!!sym(new_name) := 0)
    }
    df <- df %>%
      rowwise() %>%
      mutate(!!sym(new_name) := sum(c_across(all_of(morpho_list)), na.rm = T)) %>%
      select(-all_of(morpho_list))
    # Return the modified dataframe
    return(df)
  }
}

## test (PASS)
# test <- data.frame("A" = c(1, 2, 3), "A2" = c(4, 5, 6))
# test <- revise_sp_name(test_df, "A2", "A")

## test merge_morphotypes (PASS)
# test <- data.frame("A1" = c(1, 2, 3), "A2" = c(4, 5, 6))
# test <- merge_morphotypes(test, c("A1", "A2"), "A")


clean_species <- function(data) {
  data <- data %>%
    revise_sp_name("G. ruber p", "G. ruber ruber") %>%
    revise_sp_name("G. ruber w", "G. ruber albus") %>%
    revise_sp_name("G. sacculifer", "T. sacculifer") %>%
    revise_sp_name("T. trilobus", "T. sacculifer") %>%
    revise_sp_name("N. pachyderma s", "N. pachyderma") %>%
    revise_sp_name("N. pachyderma left", "N. pachyderma") %>%
    revise_sp_name("N. pachyderma right", "N. incompta") %>%
    revise_sp_name("N. pachyderma d", "N. incompta") %>%
    revise_sp_name("G. pachyderma", "N. incompta") %>%
    revise_sp_name("G. menardii", "G. cultrata") %>%
    revise_sp_name("G. aequilateralis", "G. eastropacia") %>%
    revise_sp_name("G. digitata", "B. digitata") %>%
    revise_sp_name("G. anfracta", "D. anfracta") %>%
    revise_sp_name("G. quinqueloba", "T. quinqueloba") %>%
    revise_sp_name("G. humilis", "T. humilis") %>%
    revise_sp_name("G. hexagona", "G. hexagonus") %>%
    revise_sp_name("G. dutertrei", "N. dutertrei") %>%
    revise_sp_name("G. bradyi", "G. uvula") %>%
    revise_sp_name("G. pumilio", "B. pumilio") %>%
    revise_sp_name("G. tumida flexuosa", "G. cultrata") %>%
    revise_sp_name("G. iota", "T. iota") %>%
    revise_sp_name("G. aequilateralis", "G. siphonifera") %>%
    revise_sp_name("G. theyeri", "G. eastropacia") %>%
    revise_sp_name("G. tenella", "G. tenellus") %>%
    revise_sp_name("G. crassula", "G. crassaformis") %>%
    revise_sp_name("G. clarkei", "T. clarkei") %>%
    revise_sp_name("O. bilobata", "O. universa")

  return(data)
}
