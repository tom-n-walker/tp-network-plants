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
    rename(Year=year, Elevation=elevation, Donor = Treatment) %>%
    gather(SpeciesName, Cover, -destSiteID, -destPlotID, -Elevation, -Treatment, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    
    ###### START HERE< CANT FIGURE OUT PLOT IDS #####
    # mutate(Treatment = case_when(Donor == "receiving" & destSiteID == 'Low'& Elevation = 1000 ~ "Warm", 
    #                              Donor == "receiving" & destSiteID == 'Low'& Elevation = 1500 ~ "Warm",
    #                              Donor == "receiving" & destSiteID == 'Low' ~ "Warm")),
    #        origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
    #                               destSiteID == "L" & Treatment == 'CP' ~ "L",
    #                               destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot)
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_IT_MatschMazia <- function(community_IT_MatschMazia_raw){
  dat <- community_IT_MatschMazia_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot)
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia <- function(community_IT_MatschMazia_raw){
  dat <- community_IT_MatschMazia_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           origSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, origSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot) %>%
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

