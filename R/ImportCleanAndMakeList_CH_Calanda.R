#####################
#### CH_CALANDA  ####
#####################

source("R/community_CH_Calanda/load_comm_cal.r")

#### Import Community ####
ImportCommunity_CH_Calanda <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  community_CH_Calanda_raw <- load_cover_CH_Calanda()
  
  return(community_CH_Calanda_raw)
}

#### Cleaning Code ####
# Cleaning Calanda community data
CleanCommunity_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(originSiteID = case_when(Treatment == "veg_away" & Site %in% c("Cal", "Nes") ~ "Pea",
                                  Treatment == "veg_home" & Site == "Nes" ~ "Nes",
                                  Treatment == "veg_home" & Site == "Pea" ~ "Pea",
                                  Treatment == "veg_home" & Site == "Cal" ~ "Cal"),
           Treatment = case_when(Treatment == "veg_away" & Site == "Cal" ~ "Warm", 
                                 Treatment == "veg_away" & Site == "Nes" ~ "Warm",
                                 Treatment == "veg_away" & Site == "Pea" ~ "Warm",
                                 Treatment == "veg_home" & Site %in% c("Nes","Pea","Cal") ~ "LocalControl")) %>%
    rename(destSiteID = Site, Cover = Cov_Rel1, Year = year, SpeciesName = Species_Name, Collector = Botanist_Rel1, destPlotID = plot_id) %>% 
    mutate(Cover=if_else(grepl("^\\d", Cover), Cover, NA_character_)) %>% 
    mutate(Cover = as.numeric(gsub("[-|,]", ".", Cover))) %>%
    filter(!is.na(Cover)) %>%
    #add block ID because blerg
    mutate(destBlockID=Block) %>% select(-Block) %>%
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) %>% 
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%
    group_by(UniqueID, Year, originSiteID, destSiteID, destBlockID, destPlotID, Treatment, Collector) %>%
    mutate(Total_Cover = sum(Cover), Other=100-Total_Cover) %>% Rel_Cover = Cover / Total_Cover)
  dat %>% group_by(UniqueID) %>% filter(Total_Cover <100) %>% distinct(Total_Cover) #lots of plots <100
  comm <- dat %>% filter(!SpeciesName %in% c('Dead', 'Bare ground', 'bare ground', 'Bryophyta', 'Stone', 'Fungi'))
  cover <- dat %>% filter(SpeciesName %in% c('Dead', 'Bare ground', 'bare ground', 'Bryophyta', 'Stone', 'Fungi')) %>% 
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover))
  return(list(comm=comm, cover=cover))
  
  #To check unique combos of sites*treatments
  #distinct(expand.grid(community_CH_Calanda_raw$Treatment,community_CH_Calanda_raw$Site))#%>%
  return(dat)
}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(originSiteID = case_when(Treatment == "veg_away" & Site %in% c("Cal", "Nes") ~ "PEA",
                                    Treatment == "veg_home" & Site == "Nes" ~ "Nes",
                                    Treatment == "veg_home" & Site == "Pea" ~ "Pea"),
           Treatment = case_when(Treatment == "veg_away" & Site == "Cal" ~ "Warm", 
                                 Treatment == "veg_away" & Site == "Nes" ~ "Warm",
                                 Treatment == "veg_home" & Site %in% c("Nes","Pea","Cal") ~ "LocalControl")) %>%
    rename(destSiteID = Site, Cover = Cov_Rel1, Year = year, SpeciesName = Species_Name, Collector = Botanist_Rel1, destPlotID = plot_id) %>% 
    mutate(Cover=if_else(grepl("^\\d", Cover), Cover, NA_character_)) %>% 
    mutate(Cover = as.numeric(gsub("[-|,]", ".", Cover))) %>% 
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) 
  taxa <- unique(dat$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Calanda <- function(community_CH_Calanda_raw) {
  dat <- community_CH_Calanda_raw %>% 
    mutate(originSiteID = case_when(Treatment == "veg_away" & Site %in% c("Cal", "Nes") ~ "PEA",
                                    Treatment == "veg_home" & Site == "Nes" ~ "Nes",
                                    Treatment == "veg_home" & Site == "Pea" ~ "Pea"),
           Treatment = case_when(Treatment == "veg_away" & Site == "Cal" ~ "Warm", 
                                 Treatment == "veg_away" & Site == "Nes" ~ "Warm",
                                 Treatment == "veg_home" & Site %in% c("Nes","Pea","Cal") ~ "LocalControl")) %>%
    rename(destSiteID = Site, Cover = Cov_Rel1, Year = year, SpeciesName = Species_Name, Collector = Botanist_Rel1, destPlotID = plot_id) %>% 
    mutate(Cover=if_else(grepl("^\\d", Cover), Cover, NA_character_)) %>% 
    mutate(Cover = as.numeric(gsub("[-|,]", ".", Cover))) %>% 
    #add block ID because blerg
    mutate(destBlockID=NA) %>%
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) %>%
    mutate(Elevation = as.numeric(recode(destSiteID, 'Pea'='2800', 'Cal'='2000', 'Nes'='1400')),
           Gradient = "CH_Calanda",
           Country = as.character("Switzerland"),
           YearEstablished = 2012,
           PlotSize_m2 = 0.75) 
  
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

