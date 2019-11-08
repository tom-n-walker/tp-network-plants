####################
### IT_MatschMazia  ###
####################

source("R/community_IT_MatschMazia/loadcomm_IT.r")

#### Import Community ####
ImportCommunity_IT_MatschMazia <- function(){
  community_IT_MatschMazia_raw<-load_cover_IT_MatschMazia()
  return(community_IT_MatschMazia_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_IT_MatschMazia <- function(community_IT_MatschMazia_raw){
  dat <- community_IT_MatschMazia_raw %>% 
    rename(Year=year, Elevation=elevation, treat = Treatment) %>%
    gather(SpeciesName, Cover, -destSiteID, -destPlotID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(Treatment = case_when(treat == "receiving" & destSiteID == 'Low' & Elevation == 1000 ~ "LocalControl",
                                treat == "donor" & destSiteID == 'Low' & Elevation == 1500 ~ "Warm",
                                treat == "donor" & destSiteID == 'High' & Elevation == 1500 ~ "LocalControl",
                                treat == "donor" & destSiteID == 'High' & Elevation == 1950 ~ "Warm"),
            origSiteID = case_when(Elevation == 1000 ~ "Low",
                                   Elevation == 1500 ~ "Middle",
                                   Elevation == 1950 ~ "High")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -treat)
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_IT_MatschMazia <- function(community_IT_MatschMazia_raw){
  dat <- community_IT_MatschMazia_raw %>% 
    rename(Year=year, Elevation=elevation, treat = Treatment) %>%
    gather(SpeciesName, Cover, -destSiteID, -destPlotID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(Treatment = case_when(treat == "receiving" & destSiteID == 'Low' & Elevation == 1000 ~ "LocalControl",
                                 treat == "donor" & destSiteID == 'Low' & Elevation == 1500 ~ "Warm",
                                 treat == "donor" & destSiteID == 'High' & Elevation == 1500 ~ "LocalControl",
                                 treat == "donor" & destSiteID == 'High' & Elevation == 1950 ~ "Warm"),
           origSiteID = case_when(Elevation == 1000 ~ "Low",
                                  Elevation == 1500 ~ "Middle",
                                  Elevation == 1950 ~ "High")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -treat)
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia <- function(community_IT_MatschMazia_raw){
  dat <- community_IT_MatschMazia_raw %>% 
    rename(Year=year, Elevation=elevation, treat = Treatment) %>%
    gather(SpeciesName, Cover, -destSiteID, -destPlotID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(Treatment = case_when(treat == "receiving" & destSiteID == 'Low' & Elevation == 1000 ~ "LocalControl",
                                 treat == "donor" & destSiteID == 'Low' & Elevation == 1500 ~ "Warm",
                                 treat == "donor" & destSiteID == 'High' & Elevation == 1500 ~ "LocalControl",
                                 treat == "donor" & destSiteID == 'High' & Elevation == 1950 ~ "Warm"),
           origSiteID = case_when(Elevation == 1000 ~ "Low",
                                  Elevation == 1500 ~ "Middle",
                                  Elevation == 1950 ~ "High")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -treat) %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()  %>% 
    mutate(Gradient = 'IT_MatschMazia', #Already fixed this, just add dat above
           Country = 'IT',
           YearEstablished = 2010,
           PlotSize_m2 = NA) #need to figure this out!
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IT_MatschMazia <- function(){
  
  ### IMPORT DATA
  community_IT_MatschMazia_raw = ImportCommunity_IT_MatschMazia()
  
  
  ### CLEAN DATA SETS
  ## IT_MatschMazia
  ### CLEAN DATA SETS
  meta_IT_MatschMazia = CleanMeta_IT_MatschMazia(community_IT_MatschMazia_raw) 
  community_IT_MatschMazia = CleanCommunity_IT_MatschMazia(community_IT_MatschMazia_raw)
  taxa_IT_MatschMazia = CleanTaxa_IT_MatschMazia(community_IT_MatschMazia_raw)
  
  
  # Make list
  IT_MatschMazia = list(meta = meta_IT_MatschMazia,
                     community = community_IT_MatschMazia,
                     taxa = taxa_IT_MatschMazia,
                     trait = NA)
  
  return(IT_MatschMazia)
}

