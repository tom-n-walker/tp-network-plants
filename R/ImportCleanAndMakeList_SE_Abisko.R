####################
###  SE_Abisko  ####
####################



#### Import Community ####
ImportCommunity_SE_Abisko <- function(){
  
  #import data
  files <- list.files("data/SE_Abisko/SE_Abisko_commdata/")
  dat <- map_df(files, ~ read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", .), sheet = "Vegetation control treatments"))
  
  return(dat)
}



#### Cleaning Code ####
# Cleaning AlpeHuez community data
CleanCommunity_SE_Abisko <- function(community_SE_Abisko_raw){
  dat2 <- community_SE_Abisko_raw %>% 
    select(-c(El, Ori, Yr, `Spot ID`, Tag, `Bare soil`:Mosses)) %>% 
    rename(originSiteID = `Elevation of origin`, destSiteID = `Transplant elevation` , destBlockID = Block , turfID = `Core ID` , Treatment = Treatment , Date = Year) %>% 
    mutate(Treatment = case_when(originSiteID =="High" & destSiteID == "High" ~ "LocalControl" , 
                                 originSiteID =="Mid" & destSiteID == "Mid" ~ "LocalControl" ,
                                 originSiteID =="Low" & destSiteID == "Low" ~ "LocalControl" , 
                                 originSiteID =="High" & destSiteID == "Low" ~ "Warm" ,
                                 originSiteID =="High" & destSiteID == "Mid" ~ "Warm" ,
                                 originSiteID =="Mid" & destSiteID == "Low" ~ "Warm" , 
                                 originSiteID =="Low" & destSiteID == "Mid" ~ "Cold" , 
                                 originSiteID =="Low" & destSiteID == "High" ~ "Cold" ,
                                 originSiteID =="Mid" & destSiteID == "High" ~ "Cold")) %>%
    gather(SpeciesName, 'cover', -destSiteID:Date)
  return(dat2)
}

# Clean metadata

CleanMeta_SE_Abisko <- function(community_SE_Abisko_raw){
  dat2 <- community_SE_Abisko_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , turfID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
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
           Gradient = 'SE_Abisko',
           Country = 'France',
           YearEstablished = 2014,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning Kashmir species list
CleanTaxa_SE_Abisko <- function(community_SE_Abisko_raw){
  dat2 <- community_SE_Abisko_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , turfID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
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
ImportClean_SE_Abisko <- function(){
  
  ### IMPORT DATA
  community_SE_Abisko_raw = ImportCommunity_SE_Abisko()
  
  
  ### CLEAN DATA SETS
  ## SE_Abisko
  
  community_SE_Abisko = CleanCommunity_SE_Abisko(community_SE_Abisko_raw)
  meta_SE_Abisko = CleanMeta_SE_Abisko(community_SE_Abisko_raw)
  taxa_SE_Abisko = CleanTaxa_SE_Abisko(community_SE_Abisko_raw)
  
  
  # Make list
  SE_Abisko = list(meta =  meta_SE_Abisko,
                     community = community_SE_Abisko,
                     taxa = taxa_SE_Abisko,
                     trait = NA)
  
  return(SE_Abisko)
}

