####################
### CN_Damxung  ###
####################

#### Import Community ####

#This lists all the different files from years 2013-2018 and combines them
ImportCommunity_CN_Damxung <- function(){
  files <- list.files("data/CN_Damxung/CN_Damxung_commdata/")
  community_CN_Damxung_raw <- map_df(files, ~ read_excel(paste0("data/CN_Damxung/CN_Damxung_commdata/", .)))
  return(community_CN_Damxung_raw)
} 


#### Cleaning Code ####
# Cleaning Damxung community data
CleanCommunity_CN_Damxung <- function(community_CN_Damxung_raw){
    dat2 <- community_CN_Damxung_raw %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , destPlotID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80 )) 
  return(dat2)
}

# Clean metadata

CleanMeta_CN_Damxung <- function(community_CN_Damxung_raw){
  dat2 <- community_CN_Damxung_raw %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , destPlotID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80 )) %>% 
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = '4800', 'LOW' = '4313')),
           Gradient = 'CN_Damxung',
           Country = 'China',
           YearEstablished = 2013,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning Kashmir species list
CleanTaxa_CN_Damxung <- function(community_CN_Damxung_raw){
  dat2 <- community_CN_Damxung_raw %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , destPlotID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80 )) 
  
  taxa<-unique(dat2$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CN_Damxung <- function(){
  
  ### IMPORT DATA
  community_CN_Damxung_raw = ImportCommunity_CN_Damxung()
 
  
  ### CLEAN DATA SETS
  ## CN_Damxung

  community_CN_Damxung = CleanCommunity_CN_Damxung(community_CN_Damxung_raw)
  meta_CN_Damxung = CleanMeta_CN_Damxung(community_CN_Damxung_raw)
  taxa_CN_Damxung = CleanTaxa_CN_Damxung(community_CN_Damxung_raw)
  
  
  # Make list
  CN_Damxung = list(meta =  meta_CN_Damxung,
                   community = community_CN_Damxung,
                   taxa = taxa_CN_Damxung,
                   trait = NA)
  
  return(CN_Damxung)
}

