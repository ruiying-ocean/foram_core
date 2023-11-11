## read ForCenS data
## ForCenS is an existing dataset, so is provided in both relative abundance (decimal) and absolute abundance
forcens <- fread('raw/ForCenS_reduced.txt')
forcens <- forcens[-1,] #delete the first describing row
forcens <- forcens[, lapply(.SD, function(x) replace(x, which(x=="N/A"), NA))] #make N/A string to NA

## strictly remove all the sample with unclear ages
## although this does not influence result at all
## forcens <- forcens[!is.na(Sample_depth_average)]

## -----------------
## clean species name
## -----------------
## remove (1) multi-species plexus and (2) morphotype and (3) unidentified species
forcens <- forcens[,c(63:72) := NULL]

## species columns
species_idx <- names(forcens)[22:62]
## convert into numeric
forcens[, (species_idx) := lapply(.SD, as.numeric), .SDcols=species_idx]
## NA -> zero
forcens[, (species_idx) := lapply(.SD, nafill, fill=0), .SDcols = species_idx] 

colnames(forcens) <- gsub("_", " ", colnames(forcens), fixed = TRUE)

## rename species
forcens <- forcens %>% replace_column_name("Globigerinoides white", "Globigerinoides ruber albus")
forcens <- forcens %>% replace_column_name("Globigerinoides ruber", "Globigerinoides ruber ruber")
forcens <- forcens %>% replace_column_name("Globorotalia menardii", "Globorotalia cultrata")

## using abbreviation
names(forcens)[22:62] <- map_vec(names(forcens)[22:62],species_abbrev)

## -----------------------------------------
## convert relative abundance to absolute abundance
## -----------------------------------------
## export in wide format
fwrite(forcens, "tidy/forcens_sp_r_tidy.csv")

## Relative abundance, wide to long table
forcens_r <- melt(forcens, id.vars = names(forcens)[1:21], 
                  measure.vars = names(forcens)[22:61],
                  variable.name = "Species", value.name = "Abundance")
## export in long format
fwrite(forcens_r, "sp/forcens_sp_r.csv")

## Absolute abundance, wide to long table
sp_indices <- c(22:62)
forcens[, Count := as.numeric(Count)]
forcens_a <- copy(forcens)
forcens_a[, (sp_indices) := .SD[, sp_indices, with = FALSE] * Count]
forcens_a  <- forcens_a %>% mutate_if(is.numeric, ~round(., 0))

## export in wide format
fwrite(forcens_a, "tidy/forcens_sp_a_tidy.csv")

forcens_a <- melt(forcens_a, id.vars = names(forcens_a)[1:21], 
                  measure.vars = names(forcens_a)[22:61],
                  variable.name = "Species", value.name = "Abundance")
## export in long format
fwrite(forcens_a, "sp/forcens_sp_a.csv")

## -----------------------------------------
## group species into functional types
## -----------------------------------------

## Check if all species are included
find_missing_species(symbiosis_tbl$Species, (names(forcens)[22:62]))

local_group_and_aggregate <- function(data, value_name){

    symbiosis_short_tbl <- symbiosis_tbl %>% select(!c(Species)) %>% distinct()
    ##join two tables                                    
    data <- merge(data, symbiosis_short_tbl, by.x="Species", by.y="short_name") %>% 
        select(!any_of(c('Species', 'short_name')))

    data <- data[,-c("Publication doi","Resource doi",
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
                                          `Symbiosis`, `Spine`)]

    # average different depth
    data_agg <-data_agg[, .(Prop = mean(Prop, na.rm=T)),
                               by = .(`Sample`, `Sample ID`, `Flag`,
                                      `Device`, `Latitude`, `Longitude`,
                                      `Water depth`, `Ocean`,
                                       `Symbiosis`, `Spine`)]

    setnames(data_agg, "Prop", value_name)
    return(data_agg)    
}

forcens_r %>% local_group_and_aggregate("Relative Abundance") %>%
    dplyr::filter(`Relative Abundance`<1.1) %>%
    fwrite("fg/forcens_fg_r.csv")

forcens_a %>% local_group_and_aggregate("Absolute Abundance") %>% fwrite("fg/forcens_fg_a.csv")
