message("Welcome to core-top project!")
message("This project is to generate clean data based on planktic foraminifera fossil in sediment core")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
library(sf)
library(tmap)

message(">>> read taxonomy related functions")
source("code/taxonomy_lib.R")
message("<<< DONE")
message(">>> read replace_na_with_zero function")
source("code/replace_na_with_zero.R")
message("<<< DONE")

USE_FORCENS = TRUE
USE_MARGO = TRUE
USE_EPILOG = FALSE
USE_GLAMAP = FALSE
USE_CLIMAP = TRUE
USE_ADDITIONAL=TRUE


plot_map <- function(data, lon, lat, var, ...){
    
    land <- read_sf("tidy/ne_50m_land/ne_50m_land.shp")
    p_land <- tm_shape(land)+ tm_polygons()

    data <- data %>% st_as_sf(coords = c(lon, lat), crs=4326) #WGS84
    
    p_data <- tm_shape(data) + tm_dots(col=var, palette = "viridis", ...)

    p <- p_land + p_data
    
    return(p)
}



delete_files <- function(folder_path, exclude_file="foram_taxonomy.csv"){
    library(fs)
    ## Get a list of all files in the folder
    file_list <- dir_ls(folder_path)
    ## Filter out the "a.csv" file
    files_to_delete <- file_list[!basename(file_list) == exclude_file]

    ## Delete the files
    for (file in files_to_delete) {
        file_delete(file)
    }
}

add_sst <- function(){
    cat("Running Python script...\n")
    
    ## Set the path to the Python script    
    project_path <- file.path(getwd())
    
    add_sst_script_path <- paste(project_path, "code/add_sst.py", sep = "/")
    
    ## sample efforts
    samp_eff_script_path <- paste(project_path, "code/sample_efforts.py", sep = "/")
    
    ## Run the Python script to add SST data and plot sample efforts
    ## Please ensure packages is installed in user's Python environment
    system(paste("python3", add_sst_script_path, project_path))
    system(paste("python3", samp_eff_script_path, project_path))
}


## Define the function for interactive prompt using menu
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
        if (USE_ADDITIONAL) source("code/clean_additional.R")
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
