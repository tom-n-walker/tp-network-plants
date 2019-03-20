####################
### IN_Kashmir  ###
####################

#### Import Community ####
ImportCommunity_IN_Kashmir <- function(){
  community_IN_Kashmir_14<-read_xlsx(path ="data/IN_Kashmir/background 2014.xlsx")
  community_IN_Kashmir_15<-read_xlsx(path ="data/IN_Kashmir/2015.xlsx")
  return(list(community_IN_Kashmir_14, community_IN_Kashmir_15))
} 


#### Cleaning Code ####
# Cleaning Kashmir community data
CleanCommunity_IN_Kashmir <- function(community_IN_Kashmir_raw){
    dat2 <- community_IN_Kashmir_raw %>% 
    bind_rows() %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , turfID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
      mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80)) 
  return(dat2)
}

# Clean metadata

CleanMeta_IN_Kashmir <- function(community_IN_Kashmir_raw){
  dat2 <- community_IN_Kashmir_raw %>% 
    bind_rows() %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , turfID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl")) %>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80)) %>% 
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
CleanTaxa_IN_Kashmir <- function(community_IN_Kashmir_raw){
  dat2 <- community_IN_Kashmir_raw %>% 
    bind_rows() %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , turfID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl"))%>% 
    mutate(Cover = recode(Cover, `1` = 0.5 , `2` = 1 , `3` = 3.5 , `4` = 8 , `5` = 15.5 , `6` = 25.5 , `7` = 35.5 , `8` = 45.5 , `9` = 55.5 , `10` = 80)) 
  
  taxa<-unique(dat2$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IN_Kashmir <- function(){
  
  ### IMPORT DATA
  community_IN_Kashmir_raw = ImportCommunity_IN_Kashmir()
 
  
  ### CLEAN DATA SETS
  ## IN_Kashmir

  community_IN_Kashmir = CleanCommunity_IN_Kashmir(community_IN_Kashmir_raw)
  meta_IN_Kashmir = CleanMeta_IN_Kashmir(community_IN_Kashmir_raw)
  taxa_IN_Kashmir = CleanTaxa_IN_Kashmir(community_IN_Kashmir_raw)
  
  
  # Make list
  IN_Kashmir = list(meta =  meta_IN_Kashmir,
                   community = community_IN_Kashmir,
                   taxa = taxa_IN_Kashmir,
                   trait = NA)
  
  return(IN_Kashmir)
}

