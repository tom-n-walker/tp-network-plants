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
    dat <- community_FR_AlpeHuez_raw %>% 
    select(c(site:cover.class), -plot) %>% 
    rename(SpeciesName = `species.name` , Cover = `cover.class` , destSiteID = site , destBlockID = block , plotID = plot.ID , Treatment = treatment , Date = date, Collector = collector)%>% 
     mutate(SpeciesName = sub("^(\\S*\\s+\\S+).*", "\\1", SpeciesName)) %>%     # This selects only the first two words in SpeciesName.
      filter(Treatment %in% c("HIGH_TURF", "LOW_TURF")) %>% 
      mutate(originSiteID = str_replace(Treatment, '(.*)_.*', "\\1"), 
             originSiteID = toupper(originSiteID),
             Treatment = case_when(Treatment =="LOW_TURF" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="HIGH_TURF" & destSiteID == "HIGH" ~ "LocalControl"),
           Year = year(as.Date(Date, format='%Y-%m-%d')),
           Cover = recode(Cover, `<1` = "0.5" , `2-5` = "3.5" , `6-10` = "8"),
           Cover= as.numeric(as.character(Cover))) %>% 
           select(-Date) %>% 
      mutate(UniqueID = paste(Year, originSiteID, destSiteID, plotID, sep='_')) %>% 
      mutate(destPlotID = paste(originSiteID, destSiteID, plotID, sep='_')) %>% 
      mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA)
    
    dat2 <- dat %>%  
      filter(!is.na(Cover)) %>%
      group_by_at(vars(-SpeciesName, -Cover)) %>%
      summarise(SpeciesName = "Other",Cover = 100 - sum(Cover)) %>%
      bind_rows(dat) %>% 
      filter(Cover > 0)  %>% #omg so inelegant
      mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
    
    comm <- dat2 %>% filter(!SpeciesName %in% c('Other')) 
    cover <- dat2 %>% filter(SpeciesName %in% c('Other')) %>% 
      select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
      rename(CoverClass=SpeciesName)
    return(list(comm=comm, cover=cover)) 
    
}

# Clean metadata

CleanMeta_FR_AlpeHuez <- function(community_FR_AlpeHuez){
  dat2 <- community_FR_AlpeHuez %>%
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = '1714', 'LOW' = '773')),
           Gradient = 'FR_AlpeHuez',
           Country = 'France',
           YearEstablished = 2014,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning species list
CleanTaxa_FR_AlpeHuez <- function(community_FR_AlpeHuez){
  taxa <- unique(community_FR_AlpeHuez$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_FR_AlpeHuez <- function(){
  
  ### IMPORT DATA
  community_FR_AlpeHuez_raw = ImportCommunity_FR_AlpeHuez()
  
  ### CLEAN DATA SETS
  cleaned_FR_AlpeHuez = CleanCommunity_FR_AlpeHuez(community_FR_AlpeHuez_raw)
  community_FR_AlpeHuez = cleaned_FR_AlpeHuez$comm
  cover_FR_AlpeHuez = cleaned_FR_AlpeHuez$cover
  meta_FR_AlpeHuez = CleanMeta_FR_AlpeHuez(community_FR_AlpeHuez) 
  taxa_FR_AlpeHuez = CleanTaxa_FR_AlpeHuez(community_FR_AlpeHuez)
  
  
  # Make list
  FR_AlpeHuez = list(meta = meta_FR_AlpeHuez,
                        community = community_FR_AlpeHuez,
                        cover = cover_FR_AlpeHuez,
                        taxa = taxa_FR_AlpeHuez)
  
  
  return(FR_AlpeHuez)
}

