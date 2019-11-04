####################
### US_Montana  ###
####################

source("R/community_US_Montana/loadcomm_Mon.r")

#### Import Community ####
ImportCommunity_US_Montana <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  community_US_Montana_raw <- load_cover_US_Montana()
  
  return(community_US_Montana_raw)
}


#### Cleaning Code ####
# Cleaning Montana community data
CleanCommunity_US_Montana <- function(community_US_Montana_raw){
  dat <- community_US_Montana_raw %>% 
    rename(destPlotID = turfID, Gradient=Region) %>% 
    select(Gradient, Year, destSiteID, destPlotID, Treatment, SpeciesName, Cover)
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_US_Montana <- function(community_US_Montana_raw){
  dat <- community_US_Montana_raw %>% 
    rename(destPlotID = turfID, Gradient=Region) %>% 
    select(Gradient, Year, destSiteID, destPlotID, Treatment, SpeciesName, Cover)
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_US_Montana <- function(community_US_Montana_raw){
  dat <- community_US_Montana_raw %>% 
    rename(destPlotID = turfID, Gradient=Region) %>% 
    select(Gradient, Year, destSiteID, destPlotID, Treatment, SpeciesName, Cover) %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'Low' = '1985', 'Middle'= '2185', 'High'='2620')), #need to figure this out
           Country = 'USA',
           YearEstablished = 2013,
           PlotSize_m2 = NA) #need to find this
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Montana <- function(){
  
  ### IMPORT DATA
  community_US_Montana_raw = ImportCommunity_US_Montana()
  
  
  ### CLEAN DATA SETS
  ## US_Montana
  ### CLEAN DATA SETS
  meta_US_Montana = CleanMeta_US_Montana(community_US_Montana_raw) 
  community_US_Montana = CleanCommunity_US_Montana(community_US_Montana_raw)
  taxa_US_Montana = CleanTaxa_US_Montana(community_US_Montana_raw)
  
  
  # Make list
  US_Montana = list(meta = meta_US_Montana,
                     community = community_US_Montana,
                     taxa = taxa_US_Montana,
                     trait = NA)
  
  return(US_Montana)
}

