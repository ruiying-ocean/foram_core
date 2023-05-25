library(dplyr)

replace_column_name <- function(df, old_name, new_name) {
  # Check if the old column name exists in the data frame
  if (!old_name %in% colnames(df)) {
    cat("Column", old_name, "does not exist in the data frame.")
    return(df)
  }
  
  # Check if the new column name already exists in the data frame
  if (new_name %in% colnames(df)) {
    cat("Column", new_name, "already exists. Merging columns.")
    df <- df %>%
      mutate(!!new_name := if_else(is.na(!!sym(new_name)), !!sym(old_name), !!sym(new_name) + !!sym(old_name))) %>%
      select(-!!sym(old_name))
  } else {
    # Replace the old column name with the new one
    df <- df %>% rename(!!new_name := !!sym(old_name))
  }
  
  return(df)
}

# Example usage
# df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
# df <- replace_column_name(df, "C", "B")

