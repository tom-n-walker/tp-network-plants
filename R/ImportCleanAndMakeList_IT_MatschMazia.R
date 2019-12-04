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
            originSiteID = case_when(Elevation == 1000 ~ "Low",
                                   Elevation == 1500 ~ "Middle",
                                   Elevation == 1950 ~ "High")) %>%
    select(Year, destSiteID, originSiteID, destPlotID, Treatment, SpeciesName, Cover, -treat) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) 
  
  dat2 <- dat %>%  
    filter(!is.na(Cover), Cover==0) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover >= 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other')) 
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover)) 
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_IT_MatschMazia <- function(community_IT_MatschMazia){
  taxa <- data.frame(taxa=unique(community_IT_MatschMazia$SpeciesName))
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia <- function(community_IT_MatschMazia){
  dat <- community_IT_MatschMazia %>%
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
  cleaned_IT_MatschMazia = CleanCommunity_IT_MatschMazia(community_IT_MatschMazia_raw)
  community_IT_MatschMazia = cleaned_IT_MatschMazia$comm
  cover_IT_MatschMazia = cleaned_IT_MatschMazia$cover
  meta_IT_MatschMazia = CleanMeta_IT_MatschMazia(community_IT_MatschMazia) 
  taxa_IT_MatschMazia = CleanTaxa_IT_MatschMazia(community_IT_MatschMazia)
  
  
  # Make list
  IT_MatschMazia = list(meta = meta_IT_MatschMazia,
                     community = community_IT_MatschMazia,
                     cover = cover_IT_MatschMazia,
                     taxa = taxa_IT_MatschMazia)
  
  return(IT_MatschMazia)
}

