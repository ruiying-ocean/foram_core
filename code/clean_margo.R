## MARGO contains both relative and absolute abundance
## North Atlantic has only relative abundance

## manually labelled datatype
## Datatype
## 1: species in relative, total in relative
## 2: species in absolute, total in absolute
## 3: species in relative, total in absolute
## 4: species in absolute, total in relative (not exists)

n_atlantic <- read_csv("raw/MARGO/clean format/LGM_MARGO_NAtlantic.csv")
s_atlantic <- read_csv("raw/MARGO/clean format/LGM_MARGO_SAtlantic.csv")
indopac <- read_csv("raw/MARGO/clean format/LGM_MARGO_IndoPacific.csv")
pacific <- readxl::read_xlsx("raw/MARGO/clean format/LGM_MARGO_Pacific.xlsx")
med <- read_csv("raw/MARGO/clean format/LGM_MARGO_Med.csv")
## fill the column of sample depth
med <- med %>% mutate(`Sample depth - upper (m)` = `Sample depth - lower (m)`)
## only for convenience when select species
pacific <- pacific %>% mutate(`Globigerinita uvula` = 0, .after = `Globigerinita glutinata`)

## combine North Atlantic with manually picked cores (with absolute abundance)
n_atlantic_cores <- read_csv("raw/MARGO/clean format/LGM_MARGO_NAtlantic_manual_picked.csv")
n_atlantic_cores <- rename(n_atlantic_cores, `Sample depth - upper (m)`=`Sediment depth [m]`)
n_atlantic_cores <- n_atlantic_cores %>% select(-c("References", "Age model [ka]"))
n_atlantic <- left_join(n_atlantic, n_atlantic_cores)
n_atlantic <- n_atlantic %>% select(-`Total Planktics`) %>%
  rename(`Total Planktics` = `Foram plankt [#/g]`) %>%
  mutate(Datatype = ifelse(is.na(`Total Planktics`), 1, 3))

## -----------------------------------
## Clean Species Name
## -----------------------------------

remove_columns <- function(data, column_names) {
  existing_columns <- intersect(column_names, names(data))
  if (length(existing_columns) > 0) {
    data <- data %>% select(-any_of(existing_columns))
  }
  return(data)
}

clean_sp_name <- function(data) {
  data <- data %>% dplyr::filter(!is.na(Longitude) & !is.na(Latitude))
  data <- data %>% remove_columns(c(
    "Globigerinoides ruber total",
    "Berggrenia pumilio + T. humilis",
    "Globorotalia menardii + tumida",
    "P/D integrade + N. pachyderma R",
    "Globigerinoides sacculifer w/o sac",
    "Globigerinoides sacculifer with sac",
    "Globorotalia truncatulinoides L",
    "Globorotalia truncatulinoides R",
    "Other UnID", "Other ID"
  ))

  data <- data %>% replace_column_name("Beela digitata", "Globigerinella digitata")
  data <- data %>% replace_column_name("Globigerinoides sacc", "Trilobatus sacculifer")
  data <- data %>% replace_column_name("Globigerinoides sacc total", "Trilobatus sacculifer")
  data <- data %>% replace_column_name("Dentagloborotalia anfracta", "Dentigloborotalia anfracta")
  data <- data %>% replace_column_name("Neogloboquadrina pachyderma L", "Neogloboquadrina pachyderma")
  data <- data %>% replace_column_name("Neogloboquadrina pachyderma R", "Neogloboquadrina incompta")
  data <- data %>% replace_column_name("Globorotalia crassula", "Globorotalia crassa")
  data <- data %>% replace_column_name("Globorotalia menardii flexuosa", "Globorotalia tumida")
  data <- data %>% replace_column_name("Globigerinoides ruber (white)", "Globigerinoides ruber albus")
  data <- data %>% replace_column_name("Globigerinoides ruber (pink)", "Globigerinoides ruber ruber")
  data <- data %>% replace_column_name("Globigerinella siphonifera (=aequilateralis)", "Globigerinella siphonifera")
  data <- data %>% replace_column_name("Globorotalia truncatulinoides total", "Globorotalia truncatulinoides")
  data <- data %>% replace_column_name("Tenuitella iota", "Tenuitellita iota")

  return(data)
}

n_atlantic <- clean_sp_name(n_atlantic)
s_atlantic <- clean_sp_name(s_atlantic)
pacific <- clean_sp_name(pacific)
indopac <- clean_sp_name(indopac)
med <- clean_sp_name(med)

## -----------------------------------
## Sort relative and absolute abundance
## -----------------------------------

## convert all data into relative abundance
only_relative_abundance <- function(data) {
  
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
  new_data <- new_data %>% remove_columns(c("Datatype", "Total Planktics"))
  ## 5 convert relative abundance in decimal format
  new_data <- new_data %>% mutate_at(
    vars(`Orbulina universa`:`Globigerinita uvula`),
    ~ . / 100
  )

  return(new_data)
}

## convert all data into abs abundance
only_abs_abundance <- function(data) {
  
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

  new_data <- new_data %>% remove_columns(c("Datatype", "Total Planktics"))
  return(new_data)
}

combine_data_frames <- function(..., wide_format = FALSE) {
  # Combine all input data frames into a single list
  data_frames <- list(...)

  # Pivot longer on each data frame
  long_data_frames <- lapply(data_frames, function(df) {
    df <- df %>% remove_columns(c(
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
med_r <- only_relative_abundance(med)
s_atlantic_r <- only_relative_abundance(s_atlantic)
n_atlantic_r <- only_relative_abundance(n_atlantic)
pacific_r <- only_relative_abundance(pacific)
indopac_r <- only_relative_abundance(indopac)

med_a <- only_abs_abundance(med)
pacific_a <- only_abs_abundance(pacific)
s_atlantic_a <- only_abs_abundance(s_atlantic)
n_atlantic_a <- only_abs_abundance(n_atlantic)
indopac_a <- only_abs_abundance(indopac)

margo_a <- combine_data_frames(s_atlantic_a, n_atlantic_a, pacific_a, indopac_a, med_a) %>%
  rename("Absolute Abundance" = "Abundance") %>%
  rowwise() %>%
  mutate_at(.vars = "Species", .funs = species_abbrev)

margo_r <- combine_data_frames(s_atlantic_r, n_atlantic_r, pacific_r, indopac_r, med_r) %>%
  rename("Relative Abundance" = "Abundance") %>%
  rowwise() %>%
  mutate_at(.vars = "Species", .funs = species_abbrev)

fwrite(margo_r, "sp/lgm_margo_sp_r.csv")
fwrite(margo_a, "sp/lgm_margo_sp_a.csv")
## -----------------------------------
## aggregate into functional groups
## -----------------------------------

local_group_and_aggregate <- function(data) {
  symbiosis_short_tbl <- symbiosis_tbl %>%
    select(!c(Species)) %>%
    distinct()
  data_merged <- merge(data, symbiosis_short_tbl, by.x = "Species", by.y = "short_name") %>% select(!"Species")

  data_merged <- data_merged %>% remove_columns(c(
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
  local_group_and_aggregate() %>%
  dplyr::filter(`Relative Abundance` < 1.1) %>%
  fwrite("fg/lgm_margo_fg_r.csv")
margo_a %>%
  local_group_and_aggregate() %>%
  fwrite("fg/lgm_margo_fg_a.csv")
