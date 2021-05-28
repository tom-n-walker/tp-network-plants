####################
### DE_TRANSALPS  ###
####################

source("R/ImportData/community_DE_TransAlps/loadcomm_TA.r")

#### Import Community ####
ImportCommunity_DE_TransAlps <- function(){
  #load cover data and metadata
  community_DE_TransAlps_raw <- load_cover_DE_TransAlps()
  return(community_DE_TransAlps_raw)
}


#### Cleaning Code ####
# Cleaning NO community data
CleanCommunity_DE_TransAlps <- function(community_DE_TransAlps_raw){
  dat <- community_DE_TransAlps_raw %>% 
    rename(Cover = biomass, Year = year, destPlotID = turfID) %>% 
    #filter(Year >2016 ) %>% # filtering out 2016, only available for a few sites
    # only select control, local control, warm/down transplant, sites low elev to high : BT, SP, FP
    mutate(Treatment = case_when(originSiteID == "BT" & destSiteID == 'BT' ~ "LocalControl",
                                 originSiteID == "BT" & destSiteID == 'FP' ~ "Cold",
                                 originSiteID == "BT" & destSiteID == 'SP' ~ "Cold",
                                 originSiteID == "FP" & destSiteID == 'BT' ~ "Warm",
                                 originSiteID == "FP" & destSiteID == 'FP' ~ "LocalControl",
                                 originSiteID == "SP" & destSiteID == 'BT' ~ "Warm",
                                 originSiteID == "SP" & destSiteID == 'SP' ~ "LocalControl")) %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>% 
    mutate(destPlotID = as.character(destPlotID),
           destBlockID = if (exists('destBlockID', where = .)){ as.character(destBlockID)} else {NA})
  
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>% #not creating other cover as biomass
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Moss', 'Dead biomass')) %>% 
    filter(Cover > 0)
  cover <- dat2 %>% filter(SpeciesName %in% c('Moss', 'Dead biomass')) %>%
    mutate(SpeciesName=recode(SpeciesName, 'Bryophyta'= 'Moss', 'Litter %'='Dead biomass')) %>%
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% 
    group_by(UniqueID, SpeciesName) %>% 
    summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  
  return(list(comm=comm, cover=cover))
}


# Clean taxa list (add these to end of above)
CleanTaxa_DE_TransAlps <- function(community_DE_TransAlps){
  taxa <- unique(community_DE_TransAlps$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_DE_TransAlps <- function(community_DE_TransAlps){
  dat <- community_DE_TransAlps %>% 
    select(destSiteID, Year) %>%
    group_by(destSiteID) %>%
    summarize(YearMin = min(Year), YearMax = max(Year)) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'BT' = 300, 'SP'= 1850, 'FP'= 2440)),
           Gradient = 'DE_TransAlps',
           Country = 'Germany/Switzerland',
           Longitude = as.numeric(recode(destSiteID, 'BT' = 11.3455, 'SP'= 11.1819, 'FP'= 8.2517)),
           Latitude = as.numeric(recode(destSiteID, 'BT' = 49.5516, 'SP'= 47.0744, 'FP'= 46.3436)),
           YearEstablished = 2016,
           PlotSize_m2 = 0.09) %>% #circle
    mutate(YearRange = (YearMax-YearEstablished)) %>% 
    select(Gradient, destSiteID, Longitude, Latitude, Elevation, YearEstablished, YearMin, YearMax, YearRange, PlotSize_m2, Country)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_DE_TransAlps <- function(){
  
  ### IMPORT DATA
  community_DE_TransAlps_raw = ImportCommunity_DE_TransAlps()
  
  ### CLEAN DATA SETS
  cleaned_DE_TransAlps = CleanCommunity_DE_TransAlps(community_DE_TransAlps_raw)
  community_DE_TransAlps = cleaned_DE_TransAlps$comm
  cover_DE_TransAlps = cleaned_DE_TransAlps$cover
  meta_DE_TransAlps = CleanMeta_DE_TransAlps(community_DE_TransAlps) 
  taxa_DE_TransAlps = CleanTaxa_DE_TransAlps(community_DE_TransAlps)
  
  
  # Make list
  DE_TransAlps = list(meta = meta_DE_TransAlps,
                    community = community_DE_TransAlps,
                    cover = cover_DE_TransAlps,
                    taxa = taxa_DE_TransAlps)
  
  return(DE_TransAlps)
}

