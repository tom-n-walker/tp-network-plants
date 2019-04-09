####################
### US_Colorado  ###
####################

#### Import Community ####
ImportCommunity_US_Colorado <- function(){
  community_US_Colorado_raw<-read_csv(file = "data/US_Colorado/RMBLtransplant_speciesCover2018.csv")
  return(community_US_Colorado_raw)
} 


community_US_Colorado_raw<-read_csv(file = "data/US_Colorado/RMBLtransplant_speciesCover2018.csv")
community_US_Colorado_raw



#### Cleaning Code ####
# Cleaning Colorado community data
CleanCommunity_US_Colorado <- function(community_US_Colorado_raw){
  dat2 <- community_US_Colorado_raw %>% 
    select(-c(pos1.1:pos5.5)) %>% 
    rename(SpeciesName = species, Cover = percentCover) %>% 
    mutate(Year = year(ymd(date_yyyymmdd)), 
           destSiteID = substr(turfID, 1, 2),
           destBlockID = substr(turfID, 3, 3),
           Treatment = substr(turfID, 7, 8),
           originSiteID = substr(turfID, nchar(turfID)-4, nchar(turfID)-3),
           originBlockID = substr(turfID, nchar(turfID)-2, nchar(turfID)-2)) %>% 
      mutate(Treatment = recode(Treatment, "c1" = "Cold", "c2" = "ColdLong", "w1" = "Warm", "w2" = "WarmLong", "nu" = "NettedControl", "u_" = "Control", "ws" = "LocalControl")) %>% 
    rename(destPlotID = turfID) %>% 
    select(-date_yyyymmdd)
    
  return(dat2)
}

# Clean metadata


# Clean species list



#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Colorado <- function(){
  
  ### IMPORT DATA
  community_US_Colorado_raw = ImportCommunity_US_Colorado()
 
  
  ### CLEAN DATA SETS
  ## US_Colorado

  community_US_Colorado = CleanCommunity_US_Colorado(community_US_Colorado_raw)
  
  # Make list
  US_Colorado = list(meta = NA,
                   metaCommunity = NA,
                   community = community_US_Colorado,
                   taxa = NA,
                   trait = NA)
  
  return(US_Colorado)
}

