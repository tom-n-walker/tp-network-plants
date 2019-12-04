#####################
#### CH_CALANDA2  ###
#####################

source("R/community_CH_Calanda2/loadcomm_JL.r")

#### Import Community ####
ImportCommunity_CH_Calanda <- function(){

  #load cover data and metadata
  community_CH_Calanda_raw <- load_cover_CH_Calanda()
  
  return(community_CH_Calanda_raw)
}

#### Cleaning Code ####
# Cleaning Calanda2 community data
CleanCommunity_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(Collector='Jacob', originSiteID = 'Cal',
           Treatment = case_when(Site == "Cal" ~ "LocalControl", 
                                 Site != "Cal" ~ "Warm")) %>%
    rename(destSiteID = Site, Cover = cover, Year = year, destPlotID = turfID) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%
    filter(!grepl('Focal', SpeciesName)) 
  
  dat2 <- dat %>%
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other", Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover > 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover, na.rm=T), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other'))
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
  
  return(dat)
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda <- function(community_CH_Calanda) {
  taxa <- data.frame(taxa=unique(community_CH_Calanda$SpeciesName))
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda <- function(community_CH_Calanda) {
  dat <- community_CH_Calanda %>%
    select(-SpeciesName, -Cover) %>%
    distinct() %>% 
    # only select control, local control, warm/down transplant
    mutate(Elevation = as.numeric(recode(destSiteID, 'Are'= '1000', 'Nes'='1400', 'Bar'='1600','Neu'='1800', 'Cal'='2000')),
           Gradient = "CH_Calanda2",
           Country = as.character("Switzerland"),
           YearEstablished = 2016, #double check this with Jacob
           PlotSize_m2 = 0.071) 
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CH_Calanda2 <- function(){
  
  ### IMPORT DATA
  community_CH_Calanda_raw = ImportCommunity_CH_Calanda()
  
  ### CLEAN DATA SETS
  cleaned_CH_Calanda = CleanCommunity_CH_Calanda(community_CH_Calanda_raw)
  community_CH_Calanda = cleaned_CH_Calanda$comm
  cover_CH_Calanda = cleaned_CH_Calanda$cover
  meta_CH_Calanda = CleanMeta_CH_Calanda(community_CH_Calanda) 
  taxa_CH_Calanda = CleanTaxa_CH_Calanda(community_CH_Calanda)
  
  
  # Make list
  CH_Calanda2 = list(meta = meta_CH_Calanda,
                    community = community_CH_Calanda,
                    cover = cover_CH_Calanda,
                    taxa = taxa_CH_Calanda)
  
  
  return(CH_Calanda2)
}

