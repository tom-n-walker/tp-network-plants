rm(list = ls())
library(tidyverse)
library(drake)
library(Hmisc)


### FUNCTION TO IMPORT SPECIES -------------------------------------------------

get_species = function(){
  # load drake environment
  loadd()
  # collate all lists of site level data
  alldat <- list(CH_Lavey, CH_Calanda, CH_Calanda2, CN_Gongga, CN_Damxung, 
                 NO_Gudmedalen, NO_Lavisdalen, NO_Skjellingahaugen, 
                 NO_Ulvhaugen,US_Colorado, US_Montana, IN_Kashmir, DE_Grainau, 
                 FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia)
  # map all taxa to one vector
  taxa <- alldat %>% 
    map(~.$taxa, bind_rows())
  # return
  return(taxa)
}


### FUNCTION TO CLEAN SPECIES NAMES WITH TNRS + GNR ----------------------------

resolve_species <- function(taxa){
  # make taxa character
  taxa <- as.character(taxa)
  # call TNRS
  taxa_tnrs <-  tnrs(query = taxa, 
                     code = "ICBN", 
                     messages = F)
  # call GNR and format output
  taxa_gnr <- gnr_resolve(names = taxa, 
                          canonical = T, 
                          best_match_only = T, 
                          with_canonical_ranks = T)
  # format TNRS
  taxa_tnr <- taxa_tnrs %>%
    select(submittedname, matchedname)
  # format GNR
  taxa_gnr <- taxa_gnr %>%
    select(user_supplied_name, matched_name2)
  # generate collated output
  taxa_df <- data.frame(given_name = taxa) %>%
    left_join(., taxa_tnrs, by = c("given_name" = "submittedname")) %>%
    left_join(., taxa_gnr, by = c("given_name" = "user_supplied_name")) %>%
    rename(tnrs_name = matchedname,
           gnr_name = matched_name2) %>%
    mutate(tnrs_mismatch = given_name != tnrs_name,
           gnr_mismatch = given_name != tnrs_name)
  # return output
  return(taxa_df)
}


### CONVENIENCE FUNCTION -------------------------------------------------------

clean_species <- function(){
  # load and clean species
  taxa <- get_species()
  cleaned <- resolve_species(taxa)
  # write output to csv file
  write_csv(file = "species_names.csv",
            x = cleaned)
}

