####################
### US_Montana  ###
####################

source("R/community_US_Montana/loadcomm_Mon.r")

#### Import Community ####
ImportCommunity_US_Montana <- function(){
  #load cover data and metadata
  community_US_Montana_raw <- load_cover_US_Montana()
  
  return(community_US_Montana_raw)
}


#### Cleaning Code ####
# Cleaning Montana community data *** NOTE MOST CODE FOR CLEANING IS IN THE COMM DATAFRAME, IT REQUIRED BINDING DATAFRAMES WHICH DIDN'T MATCH
CleanCommunity_US_Montana <- function(community_US_Montana_raw){
  dat <- community_US_Montana_raw %>% 
    rename(destPlotID = turfID, Gradient=Region) %>% 
    select(Gradient, Year, originSiteID, destSiteID, destPlotID, Treatment, SpeciesName, Cover) %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'), Collector='Tim?', Cover = as.numeric(Cover)) 
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover >= 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  #Check relative cover sums to >=100
  #dat2 %>% group_by(UniqueID) %>% filter(Total_Cover <100)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other', 'Bare', 'Litter', 'bareground', 'rock', 'litter', 'moss', 'Moss', 'Litter', 'Rock'))
  cover <- dat2 %>% filter(SpeciesName %in% c('Other', 'Bare', 'Litter', 'bareground', 'rock', 'litter', 'moss', 'Moss', 'Litter', 'Rock')) %>% 
    mutate(SpeciesName=recode(SpeciesName, litter="Litter", "bareground"='Bareground', "bare"='Bareground', 'moss'= 'Moss', 'rock'='Rock')) %>%
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_US_Montana <- function(community_US_Montana){
  taxa <- data.frame(taxa=unique(community_US_Montana$SpeciesName))
  return(taxa)
}

# Clean metadata
CleanMeta_US_Montana <- function(community_US_Montana){
  dat <- community_US_Montana %>% 
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'Low' = '1985', 'Middle'= '2185', 'High'='2620')), #need to figure this out
           Country = 'USA',
           YearEstablished = 2013,
           destBlockID = NA,
           PlotSize_m2 = NA) #need to find this
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Montana <- function(){
  ### IMPORT DATA
  community_US_Montana_raw = ImportCommunity_US_Montana()
  
  ### CLEAN DATA SETS
  cleaned_US_Montana = CleanCommunity_US_Montana(community_US_Montana_raw)
  community_US_Montana = cleaned_US_Montana$comm
  cover_US_Montana = cleaned_US_Montana$cover
  meta_US_Montana = CleanMeta_US_Montana(community_US_Montana) 
  taxa_US_Montana = CleanTaxa_US_Montana(community_US_Montana)
  
  
  # Make list
  US_Montana = list(meta = meta_US_Montana,
                     community = community_US_Montana,
                     cover = cover_US_Montana,
                     taxa = taxa_US_Montana)
  
  return(US_Montana)
}

