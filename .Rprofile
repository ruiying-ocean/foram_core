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

USE_FORCENS = TRUE
USE_MARGO = TRUE
USE_EPILOG = FALSE
USE_GLAMAP = FALSE
USE_CLIMAP = FALSE ## This is 18 ka, that differs from MARGO's 19-23 ka
USE_MIX1999 = FALSE ## this should be Holocene not LGM


delete_files <- function(folder_path,except_file="foram_sp_db.csv"){
    library(fs)
    ## Get a list of all files in the folder
    file_list <- dir_ls(folder_path)
    ## Filter out the "a.csv" file
    files_to_delete <- file_list[!basename(file_list) == except_file]

    ## Delete the files
    for (file in files_to_delete) {
        file_delete(file)
    }
}

add_sst <- function(){
    cat("Running Python script...\n")
    
    ## Set the path to the Python script    
    project_path <- file.path(getwd())
    
    python_script_path <- paste(project_path, "code/add_sst.py", sep = "/")
    
    ## Run the Python script
    system(paste("python3", python_script_path, project_path))
}


                                        # Define the function for interactive prompt using menu
customPrompt <- function() {
    ## Define the menu choices
    choices <- c("Yes", "No", "Exit")
    
    ## Display the menu and prompt for user selection
    selection <- utils::menu(choices, title = "Do you want to re-generate abundance data?")
    
    ## Convert the selection to lowercase
    selection <- tolower(choices[selection])
    
    ## Check the selection and execute code accordingly
    if (selection == "yes") {        
        
        ## Code to execute if user selects "Yes"
        cat("Executing the action...\n")
        cat("Removing existing csv files...\n")
        delete_files("sp")
        delete_files("fg")
        if (USE_FORCENS) source("code/clean_forcens.R")
        if (USE_CLIMAP)source("code/clean_climap.R")
        if (USE_MARGO) source("code/clean_margo.R")
        if (USE_EPILOG) source("code/clean_epilog.R")
        if (USE_GLAMAP) source("code/clean_glamap.R")
        if (USE_MIX1999) source("code/clean_mix1999.R")
        source("code/tidy_all.R")
        ## Add your specific code here
    } else if (selection == "no") {
        ## Code to execute if user selects "No"
        cat("No action taken.\n")
    } else {
        ## Code to execute if user selects "Exit" or closes the menu
        cat("Exiting...\n")    
    }

    selection <- utils::menu(choices, title = "Do you want to re-match SST data?")
    selection <- tolower(choices[selection])
    if (selection == "yes") {
        add_sst()
    } else if (selection == "no") {
        cat("No action taken.\n")
    } else {
        cat("Exiting...\n")    
    }
}

                                        # Call the custom prompt function
customPrompt()
