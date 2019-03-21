####################
###  DE_Grainau  ###
####################

#### Import Community ####
ImportCommunity_DE_Grainau <- function(){
  community_DE_Grainau_raw<-read_xlsx(path = "data/DE_Grainau/DE_Grainau_commdata/Vegetation 2014-17.xlsx")
  return(community_DE_Grainau_raw)
}


#### Cleaning Code ####
# Cleaning Grainau community data
CleanCommunity_DE_Grainau <- function(community_DE_Grainau_raw){
    dat2 <- community_DE_Grainau_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , turfID = plot.ID , Treatment = treatment , Year = year, Collector = collector)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 65.5 , `11` = 75.5 , `12` = 85.5 , `13` = 95.5 ))
  return(dat2)
}

# Clean metadata

CleanMeta_DE_Grainau <- function(community_DE_Grainau_raw){
  dat2 <- community_DE_Grainau_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , turfID = plot.ID , Treatment = treatment , Year = year, Collector = collector)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 65.5 , `11` = 75.5 , `12` = 85.5 , `13` = 95.5 ))%>% 
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = '1714', 'LOW' = '773')),
           Gradient = 'DE_Grainau',
           Country = 'Germany',
           YearEstablished = 2013,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning Kashmir species list
CleanTaxa_DE_Grainau <- function(community_DE_Grainau_raw){
  dat2 <- community_DE_Grainau_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , turfID = plot.ID , Treatment = treatment , Year = year , Collector = collector)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 65.5 , `11` = 75.5 , `12` = 85.5 , `13` = 95.5 ))
  
  taxa<-unique(dat2$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_DE_Grainau <- function(){
  
  ### IMPORT DATA
  community_DE_Grainau_raw = ImportCommunity_DE_Grainau()
 
  
  ### CLEAN DATA SETS
  ## DE_Grainau

  community_DE_Grainau = CleanCommunity_DE_Grainau(community_DE_Grainau_raw)
  meta_DE_Grainau = CleanMeta_DE_Grainau(community_DE_Grainau_raw)
  taxa_DE_Grainau = CleanTaxa_DE_Grainau(community_DE_Grainau_raw)
  
  
  # Make list
  DE_Grainau = list(meta =  meta_DE_Grainau,
                   community = community_DE_Grainau,
                   taxa = taxa_DE_Grainau,
                   trait = NA)
  
  return(DE_Grainau)
}

