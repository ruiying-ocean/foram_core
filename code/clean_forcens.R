## read ForCenS data
## ForCenS is an existing dataset, so is provided in both relative abundance (decimal) and absolute abundance
forcens <- fread('raw/ForCenS_reduced.txt')
forcens <- forcens[-1,] #delete the first describing row
forcens <- forcens[, lapply(.SD, function(x) replace(x, which(x=="N/A"), NA))] #make N/A string to NA

#remove CLIMAP and GLAMAP
#forcens <- forcens[like(Sample_ID, "MARGO")]
#forcens <- forcens[!like(Sample_ID, "ATL974")]

## -----------------
## clean species name
## -----------------
## remove (1) multi-species plexus and (2) morphotype and (3) unidentified species
forcens <- forcens[,c(63:72) := NULL]

species_idx <- names(forcens)[22:62] #species columns
forcens[, (species_idx) := lapply(.SD, as.numeric), .SDcols=species_idx] #convert into numeric
forcens[, (species_idx) := lapply(.SD, nafill, fill=0), .SDcols = species_idx] #NA -> zero
## The sum is indeed < 1
## forcens[, Total_Foram := rowSums(.SD), .SDcols = species_idx]

## rename species
## Both G. menardii fimbriata and G. menardii flexuosa are subspecies of G. menardii,
## that is replaced with G. cultrata

colnames(forcens) <- gsub("_", " ", colnames(forcens), fixed = TRUE)
forcens <- forcens %>% replace_column_name("Globigerinoides white", "Globigerinoides ruber albus")
forcens <- forcens %>% replace_column_name("Globigerinoides ruber", "Globigerinoides ruber ruber")
forcens <- forcens %>% replace_column_name("Globorotalia menardii", "Globorotalia cultrata")

fwrite(forcens, "sp/forcens_sp_r.csv")

## -----------------------------------------
## convert relative abundance to absolute abundance
## -----------------------------------------

# Get the column indices of B, C, and D
sp_indices <- c(22:62)

# Multiply columns B, C, and D by column A using data.table and column indices
forcens[, Count := as.numeric(Count)]
forcens_a <- copy(forcens)
forcens_a[, (sp_indices) := .SD[, sp_indices, with = FALSE] * Count]
forcens_a  <- forcens_a %>% mutate_if(is.numeric, ~round(., 0))
fwrite(forcens_a, "sp/forcens_sp_a.csv")

## -----------------------------------------
## group species into functional types
## -----------------------------------------

## Check if all species are included
find_missing_species(symbiosis_tbl$Species, (names(forcens)[22:62]))

group_and_aggregate <- function(data, value_name){

    ## wide to long table
    data <- melt(data, id.vars = names(data)[1:21], 
                         measure.vars = names(data)[22:61],
                         variable.name = "Species", value.name = "Abundance")

    ##join two tables                                    
    data <- merge(data, symbiosis_tbl, by="Species", all = TRUE)

    
    data <- data[,-c("Publication doi","short_name","Resource doi",
                     "Database", "Type","Journal", "Year","Author",
                     "Count", "Count min")]

    ## transform to numeric type
    data[, c("Abundance") := lapply(.SD, as.numeric), 
                   .SDcols=c("Abundance")]

    ## group each sample and sum up functional type's abundance
    data_agg <- data[, .(Prop = sum(Abundance, na.rm=T)),
                                  by = .(`Sample`, `Sample ID`, `Flag`,
                                         `Device`, `Latitude`, `Longitude`,
                                         `Water depth`, `Ocean`,
                                         `Sample depth top`, `Sample depth bottom`, `Sample depth average`, 
                                          `Symbiosis`, `Spinose`)]

    # average different depth
    data_agg <-data_agg[, .(Prop = mean(Prop, na.rm=T)),
                               by = .(`Sample`, `Sample ID`, `Flag`,
                                      `Device`, `Latitude`, `Longitude`,
                                      `Water depth`, `Ocean`,
                                       `Symbiosis`, `Spinose`)]

    setnames(data_agg, "Prop", value_name)
    return(data_agg)    
}

forcens %>% group_and_aggregate("Relative Abundance") %>% fwrite("fg/forcens_fg_r.csv")
forcens_a %>% group_and_aggregate("Absolute Abundance") %>% fwrite("fg/forcens_fg_a.csv")
