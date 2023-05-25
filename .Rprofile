message("Welcome to core-top project!")
message("This project is to generate clean data based on planktic foraminifera fossil in sediment core")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))

message(">>> read symbiosis table")
source("code/read_symbiosis_table.R")
message(">>> read replace_column_name function")
source("code/replace_column_name.R")
message(">>> read replace_na_with_zero function")
source("code/replace_na_with_zero.R")
message("<<< DONE")


# Define the function for interactive prompt using menu
customPrompt <- function() {
  # Define the menu choices
  choices <- c("Yes", "No", "Exit")
  
  # Display the menu and prompt for user selection
  selection <- utils::menu(choices, title = "Do you want to run all cleaning codes?")
  
  # Convert the selection to lowercase
  selection <- tolower(choices[selection])
  
  # Check the selection and execute code accordingly
  if (selection == "yes") {
    # Code to execute if user selects "Yes"
      cat("Executing the action...\n")
            source("code/clean_forcens.R")
      source("code/clean_climap_lgm.R")
      source("code/clean_margo_lgm.R")
      source("code/clean_epilog_lgm.R")
      source("code/clean_glamap_lgm.R")
      source("code/clean_mix1999_lgm.R")
    # Add your specific code here
  } else if (selection == "no") {
    # Code to execute if user selects "No"
    cat("No action taken.\n")
  } else {
    # Code to execute if user selects "Exit" or closes the menu
    cat("Exiting...\n")    
  }
}

# Call the custom prompt function
customPrompt()
