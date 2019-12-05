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
    rename(originSiteID = 'Elevation of origin', destSiteID = 'Transplant elevation' , destBlockID = Block , destPlotID = `Core ID` , Treatment = Treatment) %>% 
    mutate(Treatment = case_when(originSiteID =="High" & destSiteID == "High" ~ "LocalControl" , 
                                 originSiteID =="Mid" & destSiteID == "Mid" ~ "LocalControl" ,
                                 originSiteID =="Low" & destSiteID == "Low" ~ "LocalControl" , 
                                 originSiteID =="High" & destSiteID == "Low" ~ "Warm" ,
                                 originSiteID =="High" & destSiteID == "Mid" ~ "Warm" ,
                                 originSiteID =="Mid" & destSiteID == "Low" ~ "Warm" , 
                                 originSiteID =="Low" & destSiteID == "Mid" ~ "Cold" , 
                                 originSiteID =="Low" & destSiteID == "High" ~ "Cold" ,
                                 originSiteID =="Mid" & destSiteID == "High" ~ "Cold")) %>%
    gather(SpeciesName, 'Cover', -destSiteID, -originSiteID, -destPlotID, -destBlockID, -Treatment, -Year) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) 
  
  dat2 <- dat %>%  
    filter(!is.na(Cover)) %>%
    group_by_at(vars(-SpeciesName, -Cover)) %>%
    summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
    bind_rows(dat) %>% 
    filter(Cover > 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
  comm <- dat2 %>% filter(!SpeciesName %in% c('Other')) #there are Litter, Lichen and Moss in the species list below, but nothing here? Hmmm...
  cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover)) 
}

# Clean metadata

CleanMeta_SE_Abisko <- function(community_SE_Abisko){
  dat <- community_SE_Abisko %>% 
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
CleanTaxa_SE_Abisko <- function(){
  files <- list.files("data/SE_Abisko/SE_Abisko_commdata/") %>% 
    grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)
  splist <- map_df(files, ~ read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", .), sheet = "Species list 2012"))
  taxa <- c(splist$Name...2, splist$Name...4)
  taxa <- taxa[!is.na(taxa)]
  taxa <- taxa[!taxa %in% c('Lichen', 'Litter', 'Moss')] 
  
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_SE_Abisko <- function(){
  
  ### IMPORT DATA
  community_SE_Abisko_raw = ImportCommunity_SE_Abisko()
  
  ### CLEAN DATA SETS
  cleaned_SE_Abisko = CleanCommunity_SE_Abisko(community_SE_Abisko_raw)
  community_SE_Abisko = cleaned_SE_Abisko$comm
  cover_SE_Abisko = cleaned_SE_Abisko$cover
  meta_SE_Abisko = CleanMeta_SE_Abisko(community_SE_Abisko) 
  taxa_SE_Abisko = CleanTaxa_SE_Abisko()
  
  
  # Make list
  SE_Abisko = list(meta = meta_SE_Abisko,
                    community = community_SE_Abisko,
                    cover = cover_SE_Abisko,
                    taxa = taxa_SE_Abisko)
  
  return(SE_Abisko)
}

