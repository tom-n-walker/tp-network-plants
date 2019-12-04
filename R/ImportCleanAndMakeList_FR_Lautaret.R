####################
### FR_Lautaret  ###
####################

#### Import Community ####
ImportCommunity_FR_Lautaret <- function(){
  community_FR_Lautaret_raw<-read.table(file = "data/FR_Lautaret/FR_Lautaret_commdata/TransPlant_Lautaret.txt")
  return(community_FR_Lautaret_raw)
} 


#### Cleaning Code ####
# Cleaning Lautaret community data
CleanCommunity_FR_Lautaret <- function(community_FR_Lautaret_raw){
  dat <- community_FR_Lautaret_raw %>% 
    mutate(plot=rownames(.)) %>%
    gather(SpeciesName, Cover, -plot) %>%
    filter(!is.na(Cover)) %>%
    mutate(SpeciesName = gsub('\\.', ' ', SpeciesName)) %>%
    mutate(Year = 2017, 
           destSiteID = substr(plot, 1, 1),
           destPlotID = substr(plot, 3, 4),
           Treatment = substr(plot, 6, 7),
           originSiteID = case_when(destSiteID == "L" & Treatment == 'TP' ~ "G", 
                                  destSiteID == "L" & Treatment == 'CP' ~ "L",
                                  destSiteID == "G" & Treatment == 'CP' ~ "G")) %>% 
    mutate(Treatment = recode(Treatment, "CP" = "LocalControl", "TP" = "Warm")) %>%
    select(Year, destSiteID, originSiteID, destPlotID, Treatment, SpeciesName, Cover, -plot) %>%
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

}

# Clean taxa list (add these to end of above)
CleanTaxa_FR_Lautaret <- function(community_FR_Lautaret){
taxa <- data.frame(taxa=unique(community_FR_Lautaret$SpeciesName))
  return(taxa)
}

# Clean metadata
CleanMeta_FR_Lautaret <- function(community_FR_Lautaret){
  dat <- community_FR_Lautaret %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()  %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'G' = '2450', 'L'= '1950')),
           Gradient = 'FR_Lautaret',
           Country = 'FR',
           YearEstablished = 2017,
           PlotSize_m2 = 1)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_FR_Lautaret <- function(){
  
  ### IMPORT DATA
  community_FR_Lautaret_raw = ImportCommunity_FR_Lautaret()
  
  ### CLEAN DATA SETS
  cleaned_FR_Lautaret = CleanCommunity_FR_Lautaret(community_FR_Lautaret_raw)
  community_FR_Lautaret = cleaned_FR_Lautaret$comm
  cover_FR_Lautaret = cleaned_FR_Lautaret$cover
  meta_FR_Lautaret = CleanMeta_FR_Lautaret(community_FR_Lautaret) 
  taxa_FR_Lautaret = CleanTaxa_FR_Lautaret(community_FR_Lautaret)
  
  
  # Make list
  FR_Lautaret = list(meta = meta_FR_Lautaret,
                   community = community_FR_Lautaret,
                   cover = cover_FR_Lautaret,
                   taxa = taxa_FR_Lautaret)
  
  return(FR_Lautaret)
}

