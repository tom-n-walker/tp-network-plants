##################
### CH_LAVEY  ####
##################

source("R/community_CH_Lavey/load_comm.r")

#### Import Community ####
ImportCommunity_CH_Lavey <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  cover_CH_Lavey <- load_cover_CH_Lavey()
  
  return(cover_CH_Lavey)
}

#### Cleaning Code ####
# Cleaning Lavey community data
CleanCommunity_CH_Lavey <- function(community_CH_Lavey_raw){
  dat <- 
    community_CH_Lavey_raw %>% gather(species, 'cover', 3:221) %>%
    mutate(Treatment = recode(siteID, "RIO_RIO" = "Control", "PRA_PRAturf 2.xlsx" = "LocalControl", "PRA_RIO" = "Warm"),
           speccode= sapply(strsplit(species, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = 'Jean') %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Cover = cover, SpeciesShort = speccode, Year = year, SpeciesName = species) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("Control", "LocalControl", "Warm")) 
  
  return(dat)
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Lavey <- function(community_CH_Lavey_raw) {
  dat <- community_CH_Lavey_raw %>% gather(species, 'cover', 3:221) %>%
    mutate(Treatment = recode(siteID, "RIO_RIO" = "Control", "PRA_PRAturf 2.xlsx" = "LocalControl", "PRA_RIO" = "Warm"),
           speccode= sapply(strsplit(species, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = 'Jean') %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Cover = cover, SpeciesShort = speccode, Year = year, SpeciesName = species) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("Control", "LocalControl", "Warm")) 
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Lavey <- function(community_CH_Lavey_raw){
  dat <- 
    community_CH_Lavey_raw %>% 
    select(siteID, turfID, year) %>%
    mutate(Treatment = recode(siteID, "RIO_RIO" = "Control", "PRA_PRAturf 2.xlsx" = "LocalControl", "PRA_RIO" = "Warm"),
           Collector = 'Jean') %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Year = year) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("Control", "LocalControl", "Warm")) %>%
    mutate(Elevation = as.numeric(recode(Treatment, 'Control'='1200', 'LocalControl'='1500', 'Warm'='1500')),
           Gradient = "CH_Lavey",
           Country = as.character("Switzerland"),
           YearEstablished = 2015,
           PlotSize_m2 = 0.0625)
  
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


