####################
### DE_SUSALPS  ###
####################

source("R/community_DE_Susalps/loadcomm_SA.r")

#### Import Community ####
ImportCommunity_DE_Susalps <- function(){
  #load cover data and metadata
  community_DE_Susalps_raw <- load_cover_DE_Susalps()
  return(community_DE_Susalps_raw)
}


#### Cleaning Code ####
# Cleaning NO community data
CleanCommunity_DE_Susalps <- function(community_DE_Susalps_raw){
  dat <- community_DE_Susalps_raw %>% 
    rename(Cover = bm, Year = year, destPlotID = turfID) %>% 
    filter(Year >2016 ) %>% # filtering out 2016, only available for a few sites
    # only select control, local control, warm/down transplant
    filter(destSiteID %in% c("FE", "GW", "EB"), originSiteID %in% c("FE", "GW", "EB")) %>% 
    mutate(Treatment = case_when(originSiteID == "FE" & destSiteID == 'FE' ~ "LocalControl",
                                 originSiteID == "GW" & destSiteID == 'GW' ~ "LocalControl",
                                 originSiteID == "EB" & destSiteID == 'EB' ~ "LocalControl",
                                 originSiteID == "GW" & destSiteID == 'FE' ~ "Warm",
                                 originSiteID == "EB" & destSiteID == 'FE' ~ "Warm",
                                 originSiteID == "EB" & destSiteID == 'GW' ~ "Warm",
                                 originSiteID == "FE" & destSiteID == 'EB' ~ "Cold",
                                 originSiteID == "FE" & destSiteID == 'GW' ~ "Cold",
                                 originSiteID == "GW" & destSiteID == 'EB' ~ "Cold")) %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>% 
    mutate(destPlotID = as.character(destPlotID),
           destBlockID = if (exists('destBlockID', where = .)){ as.character(destBlockID)} else {NA})
  
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover > 0)  %>% 
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other'))
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, destSiteID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, destSiteID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))
}


# Clean taxa list (add these to end of above)
CleanTaxa_DE_Susalps <- function(community_DE_Susalps){
  taxa <- unique(community_DE_Susalps$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_DE_Susalps <- function(community_DE_Susalps){
  dat <- community_DE_Susalps %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()  %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'FE' = '550', 'GW'= '900', 'EB'= '1300')),
           Gradient = 'DE_Susalps', #Already fixed this, just add dat above
           Country = 'DE',
           YearEstablished = 2016,
           PlotSize_m2 = 0.09) #circle
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_DE_Susalps <- function(){
  
  ### IMPORT DATA
  community_DE_Susalps_raw = ImportCommunity_DE_Susalps()
  
  ### CLEAN DATA SETS
  cleaned_DE_Susalps = CleanCommunity_DE_Susalps(community_DE_Susalps_raw)
  community_DE_Susalps = cleaned_DE_Susalps$comm
  cover_DE_Susalps = cleaned_DE_Susalps$cover
  meta_DE_Susalps = CleanMeta_DE_Susalps(community_DE_Susalps) 
  taxa_DE_Susalps = CleanTaxa_DE_Susalps(community_DE_Susalps)
  
  
  # Make list
  DE_Susalps = list(meta = meta_DE_Susalps,
                        community = community_DE_Susalps,
                        cover = cover_DE_Susalps,
                        taxa = taxa_DE_Susalps)
  
  return(DE_Susalps)
}

