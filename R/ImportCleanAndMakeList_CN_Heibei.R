####################
### CN_Heibei  ###
####################

#### Import Community ####

ImportCommunity_CN_Heibei <- function(){
  community_CN_Heibei_raw<-read_excel(file_in("data/CN_Heibei/data to J Ecology.xlsx"))
  return(community_CN_Heibei_raw)
} 



#### Cleaning Code ####
# Cleaning Heibei community data

CleanCommunity_CN_Heibei <- function(community_CN_Heibei_raw){
  dat2 <- community_CN_Heibei_raw %>% 
    rename(SpeciesName = `species` , Cover = `Coverage(%)` , PlotID = Treatment , destSiteID = `away` , originSiteID = `home`, Year = year, destPlotID = `replicate`)%>% 
    filter(!(destSiteID == 3600 | originSiteID == 3600)) %>% 
    mutate(Treatment = case_when(destSiteID =="3200" & originSiteID == "3200" ~ "LocalControl" , 
                                 destSiteID =="3400" & originSiteID == "3400" ~ "LocalControl" , 
                                 destSiteID =="3800" & originSiteID == "3800" ~ "LocalControl" , 
                                 destSiteID =="3200" & originSiteID == "3400" ~ "Warm" , 
                                 destSiteID =="3200" & originSiteID == "3800" ~ "Warm" , 
                                 destSiteID =="3400" & originSiteID == "3200" ~ "Cold" , 
                                 destSiteID =="3400" & originSiteID == "3800" ~ "Warm" , 
                                 destSiteID =="3800" & originSiteID == "3200" ~ "Cold" ,
                                 destSiteID =="3800" & originSiteID == "3400" ~ "Cold"))
           
    return(dat2)
}


# Clean metadata

CleanMeta_CN_Heibei <- function(community_CN_Heibei){
  dat2 <- community_CN_Heibei %>% 
  select(-SpeciesName, -Cover, -PlotID) %>% 
    distinct()%>% 
    mutate(Elevation = destSiteID , 
           Gradient = 'CN_Heibei',
           Country = 'China',
           YearEstablished = 2007,
           PlotSize_m2 = 1
    )
  
  
  return(dat2)
}

# Cleaning Heibei species list

CleanTaxa_CN_Heibei <- function(community_CN_Heibei){
  taxa<-unique(community_CN_Heibei$SpeciesName)
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CN_Heibei <- function(){
  
  ### IMPORT DATA
  community_CN_Heibei_raw = ImportCommunity_CN_Heibei()
 
  
  ### CLEAN DATA SETS
  ## CN_Heibei

  community_CN_Heibei = CleanCommunity_CN_Heibei(community_CN_Heibei_raw)
  meta_CN_Heibei = CleanMeta_CN_Heibei(community_CN_Heibei)
  taxa_CN_Heibei = CleanTaxa_CN_Heibei(community_CN_Heibei)
  
  
  # Make list
  CN_Heibei = list(meta =  meta_CN_Heibei,
                   community = community_CN_Heibei,
                   taxa = taxa_CN_Heibei,
                   trait = NA)
  
  return(CN_Heibei)
}

