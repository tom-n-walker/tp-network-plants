####################
### FR_Lautaret  ###
####################

#### Import Community ####
ImportCommunity_FR_Lautaret <- function(){
  community_FR_Lautaret_raw<-read.table(file = "data/FR_Lautaret/FR_Lautaret_commdata/TransPlant_Lautaret.txt")
  return(community_FR_Lautaret_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_FR_Lautaret <- function(community_FR_Lautaret_raw){
  dat <- community_FR_Lautaret_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot)
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_FR_Lautaret <- function(community_FR_Lautaret_raw){
  dat <- community_FR_Lautaret_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot)
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_FR_Lautaret <- function(community_FR_Lautaret_raw){
  dat <- community_FR_Lautaret_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot) %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()  %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'G' = '2450', 'L'= '1950')),
           Gradient = 'FR_Lautaret',
           Country = 'FR',
           YearEstablished = 2017,
           PlotSize_m2 = 1)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_FR_Lautaret <- function(){
  
  ### IMPORT DATA
  community_FR_Lautaret_raw = ImportCommunity_FR_Lautaret()
  
  
  ### CLEAN DATA SETS
  ## FR_Lautaret
  ### CLEAN DATA SETS
  meta_FR_Lautaret = CleanMeta_FR_Lautaret(community_FR_Lautaret_raw) 
  community_FR_Lautaret = CleanCommunity_FR_Lautaret(community_FR_Lautaret_raw)
  taxa_FR_Lautaret = CleanTaxa_FR_Lautaret(community_FR_Lautaret_raw)
  
  
  # Make list
  FR_Lautaret = list(meta = meta_FR_Lautaret,
                     community = community_FR_Lautaret,
                     taxa = taxa_FR_Lautaret,
                     trait = NA)
  
  return(FR_Lautaret)
}

