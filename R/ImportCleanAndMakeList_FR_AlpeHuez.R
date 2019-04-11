####################
###  FR_AlpeHuez  ###
####################



#### Import Community ####
ImportCommunity_FR_AlpeHuez <- function(){
 
   #import data
  files <- list.files("data/FR_AlpeHuez/FR_AlpeHuez_commdata/")
  community_FR_AlpeHuez_raw <- map_df(files, ~ read_excel(paste0("data/FR_AlpeHuez/FR_AlpeHuez_commdata/", .), sheet = "relevee"))
  
  return(community_FR_AlpeHuez_raw)
  }
 
  

#### Cleaning Code ####
# Cleaning AlpeHuez community data
CleanCommunity_FR_AlpeHuez <- function(community_FR_AlpeHuez_raw){
    dat2 <- community_FR_AlpeHuez_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , destPlotID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
      filter(Treatment %in% c("HIGH_TURF", "LOW_TURF")) %>% 
      mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="LOW_TURF" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"),
           Year = year(as.Date(Date, format='%Y-%m-%d')),
           Cover = recode(Cover, `<1` = "0.5" , `2-5` = "3.5" , `6-10` = "8"),
           Cover= as.numeric(as.character(Cover))) %>% 
      select(-Date)
  return(dat2)
}

# Clean metadata

CleanMeta_FR_AlpeHuez <- function(community_FR_AlpeHuez_raw){
  dat2 <- community_FR_AlpeHuez_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , destPlotID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
    filter(Treatment %in% c("HIGH_TURF", "LOW_TURF")) %>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="LOW_TURF" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"),
           Year = year(as.Date(Date, format='%Y-%m-%d')),
           Cover = recode(Cover, `<1` = "0.5" , `2-5` = "3.5" , `6-10` = "8"),
           Cover= as.numeric(as.character(Cover))) %>% 
    select(-c('Date', 'SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = '1714', 'LOW' = '773')),
           Gradient = 'FR_AlpeHuez',
           Country = 'France',
           YearEstablished = 2014,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning Kashmir species list
CleanTaxa_FR_AlpeHuez <- function(community_FR_AlpeHuez_raw){
  dat2 <- community_FR_AlpeHuez_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , destPlotID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="LOW_TURF" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"),
           Year = year(as.Date(Date, format='%Y-%m-%d')),
           Cover = recode(Cover, `<1` = "0.5" , `2-5` = "3.5" , `6-10` = "8"),
           Cover= as.numeric(as.character(Cover))) %>% 
    select(-Date)
  taxa<-unique(dat2$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_FR_AlpeHuez <- function(){
  
  ### IMPORT DATA
  community_FR_AlpeHuez_raw = ImportCommunity_FR_AlpeHuez()
 
  
  ### CLEAN DATA SETS
  ## FR_AlpeHuez

  community_FR_AlpeHuez = CleanCommunity_FR_AlpeHuez(community_FR_AlpeHuez_raw)
  meta_FR_AlpeHuez = CleanMeta_FR_AlpeHuez(community_FR_AlpeHuez_raw)
  taxa_FR_AlpeHuez = CleanTaxa_FR_AlpeHuez(community_FR_AlpeHuez_raw)
  
  
  # Make list
  FR_AlpeHuez = list(meta =  meta_FR_AlpeHuez,
                   community = community_FR_AlpeHuez,
                   taxa = taxa_FR_AlpeHuez,
                   trait = NA)
  
  return(FR_AlpeHuez)
}

