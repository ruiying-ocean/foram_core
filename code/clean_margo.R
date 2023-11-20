## MARGO contains both relative and absolute abundance
## North Atlantic has only relative abundance

## manually labelled datatype
## >>> Datatype
## 1: species in relative, total in relative
## 2: species in absolute, total in absolute
## 3: species in relative, total in absolute
## 4: species in absolute, total in relative (currently not exists)

n_atlantic <- read_csv("raw/MARGO/clean format/LGM_MARGO_NAtlantic.csv")
s_atlantic <- read_csv("raw/MARGO/clean format/LGM_MARGO_SAtlantic.csv")
indopac <- read_csv("raw/MARGO/clean format/LGM_MARGO_IndoPacific.csv")
pacific <- readxl::read_xlsx("raw/MARGO/clean format/LGM_MARGO_Pacific.xlsx")
med <- read_csv("raw/MARGO/clean format/LGM_MARGO_Med.csv")
## fill the column of sample depth
med <- med %>% mutate(`Sample depth - upper (m)` = `Sample depth - lower (m)`)
## only for convenience when select species
pacific <- pacific %>% mutate(`Globigerinita uvula` = 0, .after = `Globigerinita glutinata`)

## -----------------------------------
## Clean Species Name
## -----------------------------------

## two local functions
margo_remove_columns <- function(data, column_names) {
  existing_columns <- intersect(column_names, names(data))
  if (length(existing_columns) > 0) {
    data <- data %>% select(-any_of(existing_columns))
  }
  return(data)
}

margo_clean_taxa <- function(data) {
  data <- data %>% dplyr::filter(!is.na(Longitude) & !is.na(Latitude))
  data <- data %>% margo_remove_columns(c(    
    "Berggrenia pumilio + T. humilis",
    "Globorotalia menardii + tumida",
    "Globigerinoides sacculifer w/o sac",
    "Globigerinoides sacculifer with sac",
    "Globorotalia truncatulinoides L",
    "Globorotalia truncatulinoides R",
    "Globigerinoides ruber total",
    "Neogloboquadrina pachyderma R",
    "Other UnID", "Other ID"
  ))

  data <- data %>% revise_sp_name("Beela digitata", "Beella digitata")
  data <- data %>% revise_sp_name("Globigerinoides sacc total", "Trilobatus sacculifer")
  data <- data %>% revise_sp_name("Dentagloborotalia anfracta", "Dentigloborotalia anfracta")
  data <- data %>% revise_sp_name("Neogloboquadrina pachyderma L", "Neogloboquadrina pachyderma")
  data <- data %>% revise_sp_name("P/D integrade + N. pachyderma R", "Neogloboquadrina incompta")
  data <- data %>% revise_sp_name("Globorotalia crassula", "Globorotalia crassaformis")
  data <- data %>% revise_sp_name("Globigerinoides ruber (white)", "Globigerinoides ruber albus")
  data <- data %>% revise_sp_name("Globigerinoides ruber (pink)", "Globigerinoides ruber ruber")
  data <- data %>% revise_sp_name("Globigerinella siphonifera (=aequilateralis)", "Globigerinella siphonifera")
  data <- data %>% revise_sp_name("Globorotalia truncatulinoides total", "Globorotalia truncatulinoides")
  data <- data %>% revise_sp_name("Tenuitella iota", "Tenuitellita iota")
  data <- data %>% revise_sp_name("Globoturborotalita tenella", "Globigerinoides tenellus")
  data <- data %>% revise_sp_name("Globigerinella digitata", "Beella digitata")
  data <- data %>% revise_sp_name("Globorotalia theyeri", "Globorotalia eastropacia")
  data <- data %>% revise_sp_name("Globorotalia menardii flexuosa", "Globorotalia cultrata")
  data <- data %>% revise_sp_name("Globorotalia menardii", "Globorotalia cultrata")
  
  return(data)
}

s_atlantic <- margo_clean_taxa(s_atlantic)
n_atlantic <- margo_clean_taxa(n_atlantic)
pacific <- margo_clean_taxa(pacific)
indopac <- margo_clean_taxa(indopac)
med <- margo_clean_taxa(med)

## find_missing_species(symbiosis_tbl$Species, names(s_atlantic))

## -----------------------------------
## Sort relative and absolute abundance
## -----------------------------------

## convert all data into relative abundance
margo_relative_abundance <- function(data) {
  
    perc_data <- data %>% dplyr::filter(`Datatype` == 1)
    raw_data <- data %>% dplyr::filter(`Datatype` == 2)
    fake_raw_data <- data %>% dplyr::filter(`Datatype` == 3)
    
  ## 2 convert absolute abundance to relative abundance
  raw_data <- raw_data %>% mutate_at(
    vars(`Orbulina universa`:`Globigerinita uvula`),
    ~ . / `Total Planktics` * 100
    )
    
  ## 3 merge together
    new_data <- rbind(perc_data, raw_data, fake_raw_data)
    
  ## 4 remove the last column, it'll be unuseful
  new_data <- new_data %>% margo_remove_columns(c("Datatype", "Total Planktics"))
  ## 5 convert relative abundance in decimal format
  new_data <- new_data %>% mutate_at(
    vars(`Orbulina universa`:`Globigerinita uvula`),
    ~ . / 100
  )

  return(new_data)
}

## convert all data into abs abundance
margo_abs_abundance <- function(data) {
  
  raw_data <- data %>% dplyr::filter(`Datatype` == 2)
    perc_data <- data %>% dplyr::filter(`Datatype` == 1)
    fake_raw_data <- data %>% dplyr::filter(`Datatype` == 3)

    ## convert fake raw data to real raw data
    fake_raw_data <- fake_raw_data %>% mutate_at(
      vars(`Orbulina universa`:`Globigerinita uvula`),
      ~ ceiling(. * `Total Planktics` / 100)
    )
 
  
  perc_data <- perc_data %>%
      drop_na(`Total Planktics`)
    
  new_data <- rbind(raw_data, fake_raw_data)

  new_data <- new_data %>% margo_remove_columns(c("Datatype", "Total Planktics"))
  return(new_data)
}

combine_margo_dfs <- function(..., wide_format = FALSE) {
  # Combine all input data frames into a single list
  data_frames <- list(...)

  # Pivot longer on each data frame
  long_data_frames <- lapply(data_frames, function(df) {
    df <- df %>% margo_remove_columns(c(
      "sedimentation rate (cm/ky)",
      "added by (name)", "Species",
      "calendar age estimate (cal ky BP)", "Instrument",
      "Chronostratigraphic quality (chronozone level)",
      "Laboratory", "Sample depth - lower (m)",
      "date of addition", "Publication"
    ))

    pivot_longer(df,
      cols = `Orbulina universa`:`Globigerinita uvula`,
      names_to = "Species", values_to = "Abundance"
    )
  })

  # Combine the long data frames into a single data frame
  combined_data <- do.call(rbind, long_data_frames)

  # Pivot wider to reshape the combined data frame
  # if identifier is not unique, then take average
  if (wide_format) {
    wide_data <- combined_data %>%
      distinct() %>%
      pivot_wider(
        names_from = "Species", values_from = "Abundance",
        values_fn = mean
      )
    return(wide_data)
  } else {
    return(combined_data)
  }
}


## r for relative, a for absolute
med_r <- margo_relative_abundance(med)
s_atlantic_r <- margo_relative_abundance(s_atlantic)
n_atlantic_r <- margo_relative_abundance(n_atlantic)
pacific_r <- margo_relative_abundance(pacific)
indopac_r <- margo_relative_abundance(indopac)

med_a <- margo_abs_abundance(med)
pacific_a <- margo_abs_abundance(pacific)
s_atlantic_a <- margo_abs_abundance(s_atlantic)
n_atlantic_a <- margo_abs_abundance(n_atlantic)
indopac_a <- margo_abs_abundance(indopac)

margo_a <- combine_margo_dfs(s_atlantic_a, n_atlantic_a, pacific_a, indopac_a, med_a) %>%
  rename("Absolute Abundance" = "Abundance") %>%
  rowwise() %>%
  mutate_at(.vars = "Species", .funs = species_abbrev)

margo_r <- combine_margo_dfs(s_atlantic_r, n_atlantic_r, pacific_r, indopac_r, med_r) %>%
  rename("Relative Abundance" = "Abundance") %>%
  rowwise() %>%
  mutate_at(.vars = "Species", .funs = species_abbrev)

fwrite(margo_r, "sp/lgm_margo_sp_r.csv")
fwrite(margo_a, "sp/lgm_margo_sp_a.csv")
## -----------------------------------
## aggregate into functional groups
## -----------------------------------

margo_group_and_aggregate <- function(data) {
  symbiosis_short_tbl <- symbiosis_tbl %>%
    select(!c(Species)) %>%
    distinct()
  data_merged <- merge(data, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!"Species")

  data_merged <- data_merged %>% margo_remove_columns(c(
    "sedimentation rate (cm/ky)",
    "added by (name)", "Species", "short_name",
    "calendar age estimate (cal ky BP)",
    "Chronostratigraphic quality (chronozone level)",
    "Laboratory", "Sample depth - lower (m)",
    "date of addition", "Publication"
  ))

  # allow different `Depth [m]` in one core sample to exist
  data_merged <- data_merged %>%
    group_by(
      Core, `Coring device`, Latitude, Longitude,
      `Water depth (m)`, Ocean, `Sample depth - upper (m)`,
      Symbiosis, Spine
    ) %>%
    summarise_all(.funs = sum, na.rm = T) %>%
    ungroup()

  return(data_merged)
}

## covert them to long format, assign symbiosis and spine trait
margo_r %>%
  margo_group_and_aggregate() %>%
  dplyr::filter(`Relative Abundance` < 1.1) %>%
  fwrite("fg/lgm_margo_fg_r.csv")
margo_a %>%
  margo_group_and_aggregate() %>%
  fwrite("fg/lgm_margo_fg_a.csv")
