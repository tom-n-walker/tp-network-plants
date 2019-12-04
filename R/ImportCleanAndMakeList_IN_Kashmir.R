####################
### IN_Kashmir  ###
####################

#### Import Community ####
ImportCommunity_IN_Kashmir <- function(){
  community_IN_Kashmir_14<-read_xlsx(path ="data/IN_Kashmir/IN_Kashmir_commdata/background 2014.xlsx")
  community_IN_Kashmir_15<-read_xlsx(path ="data/IN_Kashmir/IN_Kashmir_commdata/2015.xlsx")
  community_IN_Kashmir_raw <- bind_rows(community_IN_Kashmir_14, community_IN_Kashmir_15)
  return(community_IN_Kashmir_raw)
} 


#### Cleaning Code ####
# Cleaning Kashmir community data
CleanCommunity_IN_Kashmir <- function(community_IN_Kashmir_raw){
    dat <- community_IN_Kashmir_raw %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
      rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , destPlotID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>%
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 70 , `11` = 90)) %>% 
      mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'))

    
 dat2<- dat %>%  
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

# Clean metadata

CleanMeta_IN_Kashmir <- function(community_IN_Kashmir){
  dat2 <- community_IN_Kashmir %>% 
       select(-c('SpeciesName', 'Cover')) %>% 
    distinct()%>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'HIGH' = '2684', 'LOW' = '1951')),
           Gradient = 'IN_Kashmir',
           Country = 'India',
           YearEstablished = 2013,
           PlotSize_m2 = 0.25
    )
  
  
  return(dat2)
}

# Cleaning Kashmir species list
CleanTaxa_IN_Kashmir <- function(community_IN_Kashmir){
  dat2 <- community_IN_Kashmir 

  taxa <- unique(dat2$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IN_Kashmir <- function(){
  
  ### IMPORT DATA
  community_IN_Kashmir_raw = ImportCommunity_IN_Kashmir()
 
  
  ### CLEAN DATA SETS
  ## IN_Kashmir
  
  cleaned_IN_Kashmir = CleanCommunity_IN_Kashmir(community_IN_Kashmir_raw)
  community_IN_Kashmir = cleaned_IN_Kashmir$comm
  cover_IN_Kashmir = cleaned_IN_Kashmir$cover
  meta_IN_Kashmir = CleanMeta_IN_Kashmir(community_IN_Kashmir) 
  taxa_IN_Kashmir = CleanTaxa_IN_Kashmir(community_IN_Kashmir)

  
  
  # Make list
  IN_Kashmir = list (community = community_IN_Kashmir,
                     meta =  meta_IN_Kashmir,
                     cover = cover_IN_Kashmir,
                    taxa = taxa_IN_Kashmir,
                    trait = NA)
  
  return(IN_Kashmir)
}

