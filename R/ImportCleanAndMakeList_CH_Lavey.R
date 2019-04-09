##################
### CH_LAVEY  ####
##################

source("R/community_CH_Lavey/load_comm.r")

#### Import Community ####
ImportCommunity_CH_Lavey <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  community_CH_Lavey_raw <- load_cover_CH_Lavey()
  
  return( community_CH_Lavey_raw)
}

sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')

#### Cleaning Code ####
# Cleaning Lavey community data
CleanCommunity_CH_Lavey <- function(community_CH_Lavey_raw) {
  dat <- 
    community_CH_Lavey_raw %>% gather(SpeciesName, cover, -year, -siteID, -turfID) %>%
    mutate(Treatment = recode(siteID, "CRE_CRE"= "LocalControl", "RIO_RIO"= "LocalControl", "MAR_MAR"= "LocalControl", "PRA_PRA"= "LocalControl", 
                              "CRE_RIO" = "Warm", "MAR_RIO" = "Warm", "PRA_RIO" = "Warm"),
           SpeciesShort= sapply(strsplit(SpeciesName, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = ifelse(year==2017, 'Jean', 'Loic'),
           cover = as.numeric(cover)) %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Cover = cover, Year = year, destSiteID = destsiteID, destPlotID = turfID  ) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) 
  
  return(dat)

}

# Clean taxa list (add these to end of above)
CleanTaxa_CH_Lavey <- function(community_CH_Lavey_raw) {
  dat <- 
    community_CH_Lavey_raw %>% gather(SpeciesName, cover, -year, -siteID, -turfID) %>%
    mutate(Treatment = recode(siteID, "CRE_CRE"= "LocalControl", "RIO_RIO"= "LocalControl", "MAR_MAR"= "LocalControl", "PRA_PRA"= "LocalControl", 
                              "CRE_RIO" = "Warm", "MAR_RIO" = "Warm", "PRA_RIO" = "Warm"),
           SpeciesShort= sapply(strsplit(SpeciesName, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = ifelse(year==2017, 'Jean', 'Loic'),
           cover = as.numeric(cover)) %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Cover = cover, Year = year, destSiteID = destsiteID, destPlotID = turfID  ) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) 
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Lavey <- function(community_CH_Lavey_raw){
  dat <- 
    community_CH_Lavey_raw %>% gather(SpeciesName, cover, -year, -siteID, -turfID) %>%
    mutate(Treatment = recode(siteID, "CRE_CRE"= "LocalControl", "RIO_RIO"= "LocalControl", "MAR_MAR"= "LocalControl", "PRA_PRA"= "LocalControl", 
                              "CRE_RIO" = "Warm", "MAR_RIO" = "Warm", "PRA_RIO" = "Warm"),
           SpeciesShort= sapply(strsplit(SpeciesName, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = ifelse(year==2017, 'Jean', 'Loic'),
           cover = as.numeric(cover)) %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Cover = cover, Year = year, destSiteID = destsiteID, destPlotID = turfID  ) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'PRA'=1400, 'MAR'= 1750, 'CRE'=1950, 'RIO'=2200)),
           Gradient = "CH_Lavey",
           Country = as.character("Switzerland"),
           YearEstablished = 2016,
           PlotSize_m2 = 1)
  
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
ImportClean_CH_Lavey <- function(){
  
  ### IMPORT DATA
  community_CH_Lavey_raw = ImportCommunity_CH_Lavey()
  
  ### CLEAN DATA SETS
  meta_CH_Lavey = CleanMeta_CH_Lavey(community_CH_Lavey_raw) 
  community_CH_Lavey = CleanCommunity_CH_Lavey(community_CH_Lavey_raw)
  taxa_CH_Lavey = CleanTaxa_CH_Lavey(community_CH_Lavey_raw)
  #trait_NO_Norway = CleanTrait_NO_Norway(trait_NO_Norway_raw) 
  
  # Make list
  CH_Lavey = list(meta = meta_CH_Lavey,
                  community = community_CH_Lavey,
                  taxa = taxa_CH_Lavey)
  
  return(CH_Lavey)
}


