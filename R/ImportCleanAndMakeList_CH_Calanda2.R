#####################
#### CH_Calanda2  ###
#####################

source("R/community_CH_Calanda2/loadcomm_JL.r")

#### Import Community ####
ImportCommunity_CH_Calanda2 <- function(){

  #load cover data and metadata
  community_CH_Calanda2_raw <- load_cover_CH_Calanda2()
  
  return(community_CH_Calanda2_raw)
}

#### Cleaning Code ####
# Cleaning Calanda2 community data
CleanCommunity_CH_Calanda2 <- function(community_CH_Calanda2_raw) {
  dat <- community_CH_Calanda2_raw %>% 
    mutate(Collector='Jacob', originSiteID = 'Cal',
           Treatment = case_when(Site == "Cal" ~ "LocalControl", 
                                 Site != "Cal" ~ "Warm")) %>%
    rename(destSiteID = Site, Cover = cover, Year = year, destPlotID = turfID) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%
    filter(!grepl('Focal', SpeciesName)) %>% 
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
  
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
CleanTaxa_CH_Calanda2 <- function(community_CH_Calanda2) {
  taxa <- unique(community_CH_Calanda2$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda2 <- function(community_CH_Calanda2) {
  dat <- community_CH_Calanda2 %>%
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
  community_CH_Calanda2_raw = ImportCommunity_CH_Calanda2()
  
  ### CLEAN DATA SETS
  cleaned_CH_Calanda2 = CleanCommunity_CH_Calanda2(community_CH_Calanda2_raw)
  community_CH_Calanda2 = cleaned_CH_Calanda2$comm
  cover_CH_Calanda2 = cleaned_CH_Calanda2$cover
  meta_CH_Calanda2 = CleanMeta_CH_Calanda2(community_CH_Calanda2) 
  taxa_CH_Calanda2 = CleanTaxa_CH_Calanda2(community_CH_Calanda2)
  
  
  # Make list
  CH_Calanda2 = list(meta = meta_CH_Calanda2,
                    community = community_CH_Calanda2,
                    cover = cover_CH_Calanda2,
                    taxa = taxa_CH_Calanda2)
  
  
  return(CH_Calanda2)
}

