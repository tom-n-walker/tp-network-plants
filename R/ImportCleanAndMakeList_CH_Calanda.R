#####################
#### CH_CALANDA  ####
#####################

source("R/community_CH_Calanda/load_comm_cal.r")

#### Import Community ####
ImportCommunity_CH_Calanda <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  cover_CH_Calanda <- load_cover_CH_Calanda()
  
  return(cover_CH_Calanda)
}

#### Cleaning Code ####
# Cleaning Calanda community data
CleanCommunity_CH_Calanda <- function(community_CH_Calanda_raw){
  dat <- community_CH_Calanda_raw %>% 
    mutate(Treatment = case_when(Treatment == "veg_away" ~ "Warm", 
                                 Treatment == "veg_home" & Site == "Cal" ~ "Control",
                                 Treatment == "veg_home" & Site == c("Nes","Pea") ~ "LocalControl"),
           speccode = sapply(strsplit(Species_Name, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse=''))) #%>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = Site, Cover = cover, SpeciesShort = speccode, Year = year, SpeciesName = Species_Name, Collector = Botanist_Rel1) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("Control", "LocalControl", "Warm")) 
  
  return(dat)
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% gather(species, 'cover', 3:221) %>%
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
CleanMeta_CH_Calanda <- function(community_CH_Calanda_raw){
  dat <- 
    community_CH_Calanda_raw %>% 
    select(siteID, turfID, year) %>%
    mutate(Treatment = recode(siteID, "RIO_RIO" = "Control", "PRA_PRAturf 2.xlsx" = "LocalControl", "PRA_RIO" = "Warm"),
           Collector = 'Jean') %>%
    separate(siteID, c('siteID', 'destsiteID'), sep='_') %>%
    rename(originSiteID = siteID, Year = year) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("Control", "LocalControl", "Warm")) %>%
    mutate(Elevation = as.numeric(recode(Treatment, 'Control'='1200', 'LocalControl'='1500', 'Warm'='1500')),
           Gradient = "CH_Calanda",
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
ImportClean_CH_Calanda <- function(){
  
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
