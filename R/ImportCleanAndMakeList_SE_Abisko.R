####################
###  SE_Abisko  ####
####################



#### Import Community ####
ImportCommunity_SE_Abisko <- function(){
  
  #import data
  files <- list.files("data/SE_Abisko/SE_Abisko_commdata/") %>% 
       grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)

  community_SE_Abisko_raw <- read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", files), sheet = "Vegetation control treatments")
  
  return(community_SE_Abisko_raw)
}



#### Cleaning Code ####
# Cleaning Abisko community data
CleanCommunity_SE_Abisko <- function(community_SE_Abisko_raw){
  dat <- community_SE_Abisko_raw %>% 
    select(-c(El, Ori, Yr, `Spot ID`, Tag, `Bare soil`:Mosses)) %>% 
    rename(originSiteID = `Elevation of origin`, destSiteID = `Transplant elevation` , destBlockID = Block , destPlotID = `Core ID` , Treatment = Treatment) %>% 
    mutate(Treatment = case_when(originSiteID =="High" & destSiteID == "High" ~ "LocalControl" , 
                                 originSiteID =="Mid" & destSiteID == "Mid" ~ "LocalControl" ,
                                 originSiteID =="Low" & destSiteID == "Low" ~ "LocalControl" , 
                                 originSiteID =="High" & destSiteID == "Low" ~ "Warm" ,
                                 originSiteID =="High" & destSiteID == "Mid" ~ "Warm" ,
                                 originSiteID =="Mid" & destSiteID == "Low" ~ "Warm" , 
                                 originSiteID =="Low" & destSiteID == "Mid" ~ "Cold" , 
                                 originSiteID =="Low" & destSiteID == "High" ~ "Cold" ,
                                 originSiteID =="Mid" & destSiteID == "High" ~ "Cold")) %>%
    gather(SpeciesName, 'Cover', -destSiteID, -originSiteID, -turfID, -destBlockID, -Treatment, -Year) 
  return(dat)
}

# Clean metadata

CleanMeta_SE_Abisko <- function(community_SE_Abisko_raw){
  dat <- community_SE_Abisko_raw %>% 
    select(-c(El, Ori, Yr, `Spot ID`, Tag, `Bare soil`:Mosses)) %>% 
    rename(originSiteID = `Elevation of origin`, destSiteID = `Transplant elevation` , destBlockID = Block , destPlotID = `Core ID` , Treatment = Treatment) %>% 
    mutate(Treatment = case_when(originSiteID =="High" & destSiteID == "High" ~ "LocalControl" , 
                                 originSiteID =="Mid" & destSiteID == "Mid" ~ "LocalControl" ,
                                 originSiteID =="Low" & destSiteID == "Low" ~ "LocalControl" , 
                                 originSiteID =="High" & destSiteID == "Low" ~ "Warm" ,
                                 originSiteID =="High" & destSiteID == "Mid" ~ "Warm" ,
                                 originSiteID =="Mid" & destSiteID == "Low" ~ "Warm" , 
                                 originSiteID =="Low" & destSiteID == "Mid" ~ "Cold" , 
                                 originSiteID =="Low" & destSiteID == "High" ~ "Cold" ,
                                 originSiteID =="Mid" & destSiteID == "High" ~ "Cold")) %>%
    gather(SpeciesName, 'Cover', -destSiteID, -originSiteID, -turfID, -destBlockID, -Treatment, -Year) %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'High' = '690', 'Mid' = '690', 'LOW' = '500')),
           Gradient = 'SE_Abisko',
           Country = 'Sweden',
           YearEstablished = 2012,
           PlotSize_m2 = 0.0177
    )
  
  
  return(dat)
}

# Cleaning species list (full name found in other sheet of excel)
CleanTaxa_SE_Abisko <- function(community_SE_Abisko_raw){
  files <- list.files("data/SE_Abisko/SE_Abisko_commdata/") %>% 
    grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)
  splist <- map_df(files, ~ read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", .), sheet = "Species list 2012"))
  rar <- c(splist$Name...2, splist$Name...4)
  rar <- rar[!is.na(rar)]
  taxa <- rar[!rar %in% c('Lichen', 'Litter', 'Moss')]
  
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

