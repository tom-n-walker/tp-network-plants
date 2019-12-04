#####################
#### CH_CALANDA2  ###
#####################

source("R/community_CH_Calanda2/loadcomm_JL.r")

#### Import Community ####
ImportCommunity_CH_Calanda <- function(){

  #load cover data and metadata
  community_CH_Calanda_raw <- load_cover_CH_Calanda()
  
  return(community_CH_Calanda_raw)
}

#### Cleaning Code ####
# Cleaning Calanda2 community data
CleanCommunity_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(Collector='Jacob', originSiteID = 'Cal',
           Treatment = case_when(Site == "Cal" ~ "LocalControl", 
                                 Site != "Cal" ~ "Warm")) %>%
    rename(destSiteID = Site, Cover = cover, Year = year, destPlotID = turfID) 
  
  return(dat)
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(Collector='Jacob', originSiteID = 'Cal',
           Treatment = case_when(Site == "Cal" ~ "LocalControl", 
                                 Site != "Cal" ~ "Warm")) %>%
    rename(destSiteID = Site, Cover = cover, Year = year, destPlotID = turfID) 
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(Collector='Jacob', originSiteID = 'Cal',
           Treatment = case_when(Site == "Cal" ~ "LocalControl", 
                                 Site != "Cal" ~ "Warm")) %>%
    rename(destSiteID = Site, Cover = cover, Year = year, destPlotID = turfID) %>% 
    #add block ID because blerg
    mutate(destBlockID=NA) %>%
    select(-SpeciesName, -Cover) %>%
    distinct() %>% 
    # only select control, local control, warm/down transplant
    mutate(Elevation = as.numeric(recode(destSiteID, 'Are'= '1000', 'Nes'='1400', 'Bar'='1600','Neu'='1800', 'Cal'='2000')),
           Gradient = "CH_Calanda2",
           Country = as.character("Switzerland"),
           YearEstablished = 2016, #double check this with Jacob
           PlotSize_m2 = 0.071) 
  
  return(dat)
}

# Clean trait data (example)
# CleanTrait_NO_Norway <- function(trait_NO_Norway_raw){
#   dat2 <- trait_NO_Norway_raw %>% 
#     rename(SpeciesName = Taxon, Collector = Data_collected_by, Leaf_Thickness_Ave_mm = Leaf_Thicness_Ave_mm, PlantID = ID) %>%
#     mutate(SpeciesName = trimws(SpeciesName),
#            Year = year(Date),
#            Country = "Norway",
#            Site = recode(Site, "Lav" = "Lavisdalen", "Hog" = "Hogsete", "Ulv" =  "Ulvhaugen", "Vik" = "Vikesland", "Gud" = "Gudmedalen", "Ram" = "Rambera", "Arh" = "Arhelleren", "Skj" = "Skjellingahaugen", "Ves" = "Veskre", "Alr" = "Alrust", "Ovs" = "Ovstedal", "Fau" = "Fauske")) %>% 
#     select(-X1, -Date, -Longitude, -Latitude, -Elevation, -Project, -Gradient) %>%
#     gather(key = Trait, value = Value, -Country, -Year, -Site, -Individual_number, -SpeciesName, -PlantID, -Collector) %>% 
#     filter(!is.na(Value))
#   
#   return(dat2)
# }


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CH_Calanda2 <- function(){
  
  ### IMPORT DATA
  community_CH_Calanda_raw = ImportCommunity_CH_Calanda()
  
  ### CLEAN DATA SETS
  meta_CH_Calanda = CleanMeta_CH_Calanda(community_CH_Calanda_raw) 
  community_CH_Calanda = CleanCommunity_CH_Calanda(community_CH_Calanda_raw)
  taxa_CH_Calanda = CleanTaxa_CH_Calanda(community_CH_Calanda_raw)
  #trait_NO_Norway = CleanTrait_NO_Norway(trait_NO_Norway_raw) 
  
  # Make list
  CH_Calanda = list(meta = meta_CH_Calanda,
                    community = community_CH_Calanda,
                    taxa = taxa_CH_Calanda)
  
  return(CH_Calanda)
}

