####################
### US_Colorado  ###
####################

#### Import Community ####
ImportCommunity_US_Colorado <- function(){
  community_US_Colorado_raw<-read_csv(file = "data/US_Colorado/RMBLtransplant_speciesCover2018.csv")
  return(community_US_Colorado_raw)
} 


#### Cleaning Code ####
# Cleaning Colorado community data
CleanCommunity_US_Colorado <- function(community_US_Colorado_raw){
  dat <- community_US_Colorado_raw %>% 
    select(-c(pos1.1:pos5.5)) %>% 
    rename(SpeciesName = species, Cover = percentCover) %>% 
    mutate(Year = year(ymd(date_yyyymmdd)), 
           destSiteID = substr(turfID, 1, 2),
           destBlockID = substr(turfID, 3, 3),
           Treatment = substr(turfID, 7, 8),
           originSiteID = substr(turfID, nchar(turfID)-4, nchar(turfID)-3),
           originBlockID = substr(turfID, nchar(turfID)-2, nchar(turfID)-2)) %>% 
      mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm", "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl")) %>% 
    rename(destPlotID = turfID) %>% 
    select(-date_yyyymmdd, -comments)
    
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_US_Colorado <- function(community_US_Colorado_raw){
  dat <- community_US_Colorado_raw %>% 
    select(-c(pos1.1:pos5.5)) %>% 
    rename(SpeciesName = species, Cover = percentCover) %>% 
    mutate(Year = year(ymd(date_yyyymmdd)), 
           destSiteID = substr(turfID, 1, 2),
           destBlockID = substr(turfID, 3, 3),
           Treatment = substr(turfID, 7, 8),
           originSiteID = substr(turfID, nchar(turfID)-4, nchar(turfID)-3),
           originBlockID = substr(turfID, nchar(turfID)-2, nchar(turfID)-2)) %>% 
    mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm", "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl")) %>% 
    rename(destPlotID = turfID) %>% 
    select(-date_yyyymmdd, -comments)
taxa <- unique(dat$SpeciesName)
return(taxa)
}

# Clean metadata
CleanMeta_US_Colorado <- function(community_US_Colorado_raw){
  dat <- community_US_Colorado_raw %>% 
    select(-c(pos1.1:pos5.5)) %>% 
    rename(SpeciesName = species, Cover = percentCover) %>% 
    mutate(Year = year(ymd(date_yyyymmdd)), 
           destSiteID = substr(turfID, 1, 2),
           destBlockID = substr(turfID, 3, 3),
           Treatment = substr(turfID, 7, 8),
           originSiteID = substr(turfID, nchar(turfID)-4, nchar(turfID)-3),
           originBlockID = substr(turfID, nchar(turfID)-2, nchar(turfID)-2)) %>% 
    mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "Cold", "w1" = "Warm", "w2" = "Warm", "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl")) %>% 
    rename(destPlotID = turfID) %>% 
    select(-date_yyyymmdd, comments) %>%
  select(-c('SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'um' = '2900', 'pf'= '3200', 'mo' = '3300')),
           Gradient = 'US_Colorado',
           Country = 'USA',
           YearEstablished = 2017,
           PlotSize_m2 = 0.25)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Colorado <- function(){
  
  ### IMPORT DATA
  community_US_Colorado_raw = ImportCommunity_US_Colorado()
 
  
  ### CLEAN DATA SETS
  ## US_Colorado
  ### CLEAN DATA SETS
  meta_US_Colorado = CleanMeta_US_Colorado(community_US_Colorado_raw) 
  community_US_Colorado = CleanCommunity_US_Colorado(community_US_Colorado_raw)
  taxa_US_Colorado = CleanTaxa_US_Colorado(community_US_Colorado_raw)
  
  
  # Make list
  US_Colorado = list(meta = meta_US_Colorado,
                   community = community_US_Colorado,
                   taxa = taxa_US_Colorado,
                   trait = NA)
  
  return(US_Colorado)
}

