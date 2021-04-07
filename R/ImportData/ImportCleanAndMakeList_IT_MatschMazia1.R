####################
### IT_MatschMazia1  ###
####################

source("R/ImportData/community_IT_MatschMazia/loadcomm_IT.r")

#### Import Community ####
ImportCommunity_IT_MatschMazia1 <- function(){
  community_IT_MatschMazia1_raw<-load_cover_IT_MatschMazia1()
  return(community_IT_MatschMazia1_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_IT_MatschMazia1 <- function(community_IT_MatschMazia1_raw){
  dat <- community_IT_MatschMazia1_raw %>% 
    rename(Year=year, Elevation=elevation) %>%
    gather(SpeciesName, Cover, -UniqueID, -Elevation, -treat, -Year) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\_', ' ', SpeciesName)) %>%
    mutate(destSiteID = case_when(Elevation == 1000 ~ "Low",
                                    Elevation == 1500 ~ "High"),
            Treatment = case_when(treat == "destControl" ~ "LocalControl",
                                 treat == "originControls"  ~ "LocalControl",
                                treat == "warmed" ~ "Warm"),
            originSiteID = case_when(Elevation == 1000 & treat == "destControl" ~ "Low",
                                   Elevation == 1500 ~ "High",
                                   treat == 'warmed' ~ 'High')) %>% 
    select(Year, destSiteID, originSiteID, UniqueID, Treatment, SpeciesName, Cover, -treat) %>% 
    extract(UniqueID, into = c("destPlotID", "year"), "(.*)_([^_]+)$") %>% 
    mutate(UniqueID = paste(originSiteID, destSiteID, destPlotID, sep='_')) %>%  
    select(-year)
  #  mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = pmax((100 - sum(Cover)), 0)) %>% 
    bind_rows(dat) %>% 
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other'))  %>% 
    filter(Cover > 0)
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover)) 
  
  return(dat)
}

# Clean taxa list (add these to end of above)
CleanTaxa_IT_MatschMazia1 <- function(community_IT_MatschMazia1){
  taxa <- unique(community_IT_MatschMazia1$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_IT_MatschMazia1 <- function(community_IT_MatschMazia1){
  dat <- community_IT_MatschMazia1 %>%
    select(-c('SpeciesName', 'Cover', 'Total_Cover', 'Rel_Cover')) %>% 
    distinct()  %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'Low' = '1000', 'High'= '1500')),
           Gradient = 'IT_MatschMazia1', #Already fixed this, just add dat above
           Country = 'IT',
           YearEstablished = 2010,
           PlotSize_m2 = NA) #need to figure this out!
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IT_MatschMazia1 <- function(){
  
  ### IMPORT DATA
  community_IT_MatschMazia1_raw = ImportCommunity_IT_MatschMazia1()
  
  ### CLEAN DATA SETS
  cleaned_IT_MatschMazia1 = CleanCommunity_IT_MatschMazia1(community_IT_MatschMazia1_raw)
  community_IT_MatschMazia1 = cleaned_IT_MatschMazia1$comm
  cover_IT_MatschMazia1 = cleaned_IT_MatschMazia1$cover
  meta_IT_MatschMazia1 = CleanMeta_IT_MatschMazia1(community_IT_MatschMazia1) 
  taxa_IT_MatschMazia1 = CleanTaxa_IT_MatschMazia1(community_IT_MatschMazia1)
  
  
  # Make list
  IT_MatschMazia1 = list(meta = meta_IT_MatschMazia1,
                     community = community_IT_MatschMazia1,
                     cover = cover_IT_MatschMazia1,
                     taxa = taxa_IT_MatschMazia1)
  
  return(IT_MatschMazia1)
}

