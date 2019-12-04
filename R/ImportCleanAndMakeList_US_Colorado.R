####################
### US_Colorado  ###
####################

#### Import Community ####
ImportCommunity_US_Colorado <- function(){
  community_US_Colorado_raw<-read_csv(file = "./data/US_Colorado/US_Colorado_commdata/RMBLtransplant_speciesCover2018.csv")
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
    select(-date_yyyymmdd, -comments) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destBlockID, destPlotID, sep='_'), Collector='Laura', SpeciesName = recode(SpeciesName, 'Rock' = "rock", 'Moss' = "moss")) 
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover >= 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  #dat2 %>% filter(Total_Cover<100) #There are plots with <100, so we need to create an Other cover class
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Bare Soil', 'Litter', 'rock', 'Rock', 'moss'))
  cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Bare Soil', 'Litter', 'rock', 'Rock', 'moss')) %>%
    mutate(SpeciesName=recode(SpeciesName, "Bare Soil"='Bareground', 'moss'= 'Moss', 'rock'='Rock')) %>%
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
    
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_US_Colorado <- function(community_US_Colorado){
taxa <- unique(community_US_Colorado$dat$SpeciesName)
return(taxa)
}

# Clean metadata
CleanMeta_US_Colorado <- function(community_US_Colorado){
  dat <- community_US_Colorado %>% 
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
  cleaned_US_Colorado = CleanCommunity_US_Colorado(community_US_Colorado_raw)
  community_US_Colorado = cleaned_US_Colorado$comm
  cover_US_Colorado = cleaned_US_Colorado$cover
  meta_US_Colorado = CleanMeta_US_Colorado(community_US_Colorado) 
  taxa_US_Colorado = CleanTaxa_US_Colorado(community_US_Colorado)
  
  
  # Make list
  US_Colorado = list(meta = meta_US_Colorado,
                   community = community_US_Colorado,
                   cover = cover_US_Colorado,
                   taxa = taxa_US_Colorado)
  
  return(US_Colorado)
}

