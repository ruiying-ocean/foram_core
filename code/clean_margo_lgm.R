## MARGO contains both relative and absolute abundance
## North Atlantic has only relative abundance
n_atlantic <- read_csv("raw/LGM_MARGO/clean format/LGM_MARGO_NAtlantic.csv") %>% replace_na_with_zero()
s_atlantic <- read_csv("raw/LGM_MARGO/clean format/LGM_MARGO_SAtlantic.csv") %>% replace_na_with_zero()
indopac <- read_csv("raw/LGM_MARGO/clean format/LGM_MARGO_IndoPacific.csv")%>% replace_na_with_zero()
pacific <- read_csv("raw/LGM_MARGO/clean format/LGM_MARGO_Pacific.csv")%>% replace_na_with_zero()
## only for convenience when select species
pacific <- pacific %>% mutate(`Globigerinita uvula` = 0, .after=`Globigerinita glutinata`)

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

clean_sp_name <- function(data){
  
    data <- data %>% dplyr::filter(!is.na(Longitude) & !is.na(Latitude))
    data <- data %>% remove_columns(c("Globigerinoides ruber total", 
                               "Berggrenia pumilio + T. humilis",
                               "Globorotalia menardii + tumida",
                               "P/D integrade + N. pachyderma R",
                               "Globigerinoides sacculifer w/o sac",
                               "Globigerinoides sacculifer with sac",
                               "Globorotalia truncatulinoides L",
                               "Globorotalia truncatulinoides R", 
                               "Other UnID","Other ID"))

    data <- data %>% replace_column_name("Beela digitata", "Globigerinella digitata")
    data <- data %>% replace_column_name("Globigerinoides sacc", "Trilobatus sacculifer")
    data <- data %>% replace_column_name("Globigerinoides sacc total", "Trilobatus sacculifer")
    data <- data %>% replace_column_name("Dentagloborotalia anfracta", "Dentigloborotalia anfracta")
    data <- data %>% replace_column_name("Neogloboquadrina pachyderma L", "Neogloboquadrina pachyderma")
    data <- data %>% replace_column_name("Neogloboquadrina pachyderma R", "Neogloboquadrina incompta")
    data <- data %>% replace_column_name("Neogloboquadrina pachyderma R", "Neogloboquadrina incompta")
    data <- data %>% replace_column_name("Globorotalia crassula","Globorotalia crassa")
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

## -----------------------------------
## Sort relative and absolute abundance
## -----------------------------------

only_relative_abundance <- function(data){
    ## 1. filter relative and absolute abundance
    raw_data <- data %>% dplyr::filter(`%(1) or Raw(2)` == 2)
    perc_data <- data %>% dplyr::filter(`%(1) or Raw(2)` == 1)
    ## 2 convert absolute abundance to relative abundance
    raw_data <- raw_data %>% mutate_at(vars(`Orbulina universa`:`Globigerinita uvula`),
                                       ~ . / `Total Planktics` * 100)
    ## 3 merge together
    new_data <- rbind(raw_data, perc_data)
    ## 4 remove the last column, it'll be unuseful
    new_data <- new_data %>% remove_columns(c("%(1) or Raw(2)", "Total Planktics" ))
    ## 5 convert relative abundance in decimal format
    new_data <- new_data %>% mutate_at(vars(`Orbulina universa`:`Globigerinita uvula`),
                                     ~ . / 100)

    return(new_data)    
}

only_abs_abundance <- function(data){
    
    threshold <- 5 ## Define the threshold to determine if the value is "around" 100
    data <- data %>%  mutate_at(vars(`Orbulina universa`:`Globigerinita uvula`),
                        ~ if_else(abs(`Total Planktics` - 100) > threshold & !is.na(`Total Planktics`),
                                  as.integer(. * `Total Planktics`/100),
                                  NA))
    data <- data %>% drop_na(`Orbulina universa`)
    data <- data %>% remove_columns(c("%(1) or Raw(2)", "Total Planktics" ))
    return(data)
}

combine_data_frames <- function(...) {
  # Combine all input data frames into a single list
  data_frames <- list(...)
  
  # Pivot longer on each data frame
  long_data_frames <- lapply(data_frames, function(df) {
    df <- df %>% remove_columns(c("sedimentation rate (cm/ky)",
                         "added by (name)", "Species",
                         "calendar age estimate (cal ky BP)",
                         "Chronostratigraphic quality (chronozone level)",
                         "Laboratory", "Sample depth - lower (m)",
                         "date of addition","Publication"))
    
  pivot_longer(df, cols =  `Orbulina universa`:`Globigerinita uvula`,
                 names_to = "Species", values_to = "Abundance")
  })
  
  # Combine the long data frames into a single data frame
  combined_data <- do.call(rbind, long_data_frames)
  
  # Pivot wider to reshape the combined data frame
  # if identifier is not unique, then take average
  wide_data <- combined_data %>% distinct() %>%
    pivot_wider(names_from = "Species", values_from = "Abundance",
                values_fn = mean)
  return(wide_data)
}


## r for relative, a for absolute
s_atlantic_r <- only_relative_abundance(s_atlantic)
n_atlantic_r <- only_relative_abundance(n_atlantic)
pacific_r <- only_relative_abundance(pacific)
indopac_r <- only_relative_abundance(indopac)

s_atlantic_a <- only_abs_abundance(s_atlantic)
n_atlantic_a <- only_abs_abundance(n_atlantic)
indopac_a <- only_abs_abundance(indopac)
pacific_a <- only_abs_abundance(pacific)
combine_data_frames(s_atlantic_a, n_atlantic_a, pacific_a,indopac_a) %>%
  fwrite("sp/lgm_margo_sp_a.csv")

combine_data_frames(s_atlantic_r, n_atlantic_r, pacific_r, indopac_r) %>%
  fwrite("sp/lgm_margo_sp_r.csv")

## -----------------------------------
## aggregate into functional groups
## -----------------------------------

group_and_aggregate <- function(data, value_name="Relative Abundance"){
    data <- data %>% pivot_longer(cols=c(`Orbulina universa`:`Globigerinita uvula`), names_to = "Species", values_to=value_name)
    data_merged <- merge(data, symbiosis_tbl, by="Species")
    data_merged <- data_merged %>% dplyr::filter(Symbiosis != "Undetermined" & Spinose != 'Undetermined')
    data_merged <- data_merged %>% remove_columns(c("sedimentation rate (cm/ky)",
                                      "added by (name)", "Species","short_name",
                                      "calendar age estimate (cal ky BP)",
                                      "Chronostratigraphic quality (chronozone level)",
                                      "Laboratory", "Sample depth - lower (m)",
                                      "date of addition","Publication"))
    
    data_merged <- data_merged %>% group_by(Core, `Coring device`, Latitude, Longitude,
                              `Water depth (m)`, Ocean, `Sample depth - upper (m)`,
                              Symbiosis, Spinose) %>% 
      summarise_all(.funs = sum, na.rm=T) %>% ungroup()
    
    # 2 average different `Depth [m]`
    data_merged <- data_merged %>% group_by(Core, `Coring device`, Latitude, Longitude, `Water depth (m)`,
                              Ocean, Symbiosis, Spinose) %>%
      summarise_all(.funs = mean, na.rm=T) %>% ungroup()

    return(data_merged)
}


## covert them to long format, assign symbiosis and spine trait 
pacific_r_melt <- pacific_r %>% group_and_aggregate()
n_atlantic_r_melt <- n_atlantic_r %>% group_and_aggregate()
s_atlantic_r_melt <- s_atlantic_r %>% group_and_aggregate()
indopac_r_melt <- indopac_r %>% group_and_aggregate()

## merge oceans
list(
    s_atlantic_r_melt,
    n_atlantic_r_melt,
    pacific_r_melt,
    indopac_r_melt
) %>%
    rbindlist() %>%
    fwrite( "fg/lgm_margo_fg_r.csv")


## covert them to long format, assign symbiosis and spine trait 
pacific_a_melt <- pacific_a %>% group_and_aggregate("Absolute Abundance")%>%
  mutate(`Absolute Abundance` = as.integer(`Absolute Abundance`))
n_atlantic_a_melt <- n_atlantic_a %>%  group_and_aggregate("Absolute Abundance") %>%
  mutate(`Absolute Abundance` = as.integer(`Absolute Abundance`))
s_atlantic_a_melt <- s_atlantic_a %>%  group_and_aggregate("Absolute Abundance") %>%
  mutate(`Absolute Abundance` = as.integer(`Absolute Abundance`))
indopac_a_melt <- indopac_a %>%  group_and_aggregate("Absolute Abundance") %>%
  mutate(`Absolute Abundance` = as.integer(`Absolute Abundance`))

## merge oceans
list(
    s_atlantic_a_melt,
    n_atlantic_a_melt,
    pacific_a_melt,
    indopac_a_melt
) %>%
    rbindlist() %>%
    fwrite( "fg/lgm_margo_fg_a.csv")
