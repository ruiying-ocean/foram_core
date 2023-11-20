replace_na_with_zero <- function(df){
  df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  return(df)
}

drop_all_na_column <- function(df){
  ## remove columns with all NA
  df <- df  %>% discard(~all(is.na(.)))
  return(df)
}


