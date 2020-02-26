##################
### CH_LAVEY  ####
##################

source("R/community_CH_Lavey/load_comm.r")

#### Import Community ####
ImportCommunity_CH_Lavey <- function(){
  ## ---- load_community
  
  #load cover data and metadata
  community_CH_Lavey_raw <- load_cover_CH_Lavey()
  
  return(community_CH_Lavey_raw)
}


#### Cleaning Code ####
# Cleaning Lavey community data
CleanCommunity_CH_Lavey <- function(community_CH_Lavey_raw) {
  dat <- 
    community_CH_Lavey_raw %>% 
    mutate(Treatment = recode(siteID, "CRE_CRE"= "LocalControl", "RIO_RIO"= "LocalControl", "MAR_MAR"= "LocalControl", "PRA_PRA"= "LocalControl", 
                              "CRE_RIO" = "Warm", "MAR_RIO" = "Warm", "PRA_RIO" = "Warm"),
           #SpeciesShort= sapply(strsplit(SpeciesName, ' '), function(x) paste(toupper(substr(x, 1,3)), collapse='')),
           Collector = ifelse(year==2017, 'Jean', 'Loic'),
           cover = as.numeric(cover)) %>%
    separate(siteID, c('destSiteID', 'originSiteID'), sep='_') %>%
    rename(Cover = cover, Year = year, plotID = turfID  ) %>%         
    # DDE: What now is plotID - used to be destPlotID. I now created a destPlotID below, including origin and dest siteID.
    # only select control, local control, warm/down transplant
    filter(Treatment %in% c("LocalControl", "Warm")) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, plotID, sep='_'),
           destPlotID = paste(originSiteID, destSiteID, plotID, sep='_')) %>%
    mutate(destPlotID = as.character(destPlotID), destBlockID = if (exists('destBlockID', where = .)) as.character(destBlockID) else NA) %>%
    #transmute_at(vars(one_of(destBlockID, destPlotID)), as.character) #%>%
    group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment, Collector) %>%
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover) %>% filter(Cover>0) 
  
  #Check relative cover sums to >=100
  #dat %>% group_by(UniqueID) %>% filter(Total_Cover <100) #nothing, no need to add an other category
  
  #Create comm and cover class dataframes
  comm <- dat %>% filter(!SpeciesName %in% c('Dead', 'Bare ground', 'bare ground', 'Bryophyta', 'Stone', 'Fungi'))
  cover <- dat %>% filter(SpeciesName %in% c('Dead', 'Bare ground', 'bare ground', 'Bryophyta', 'Stone', 'Fungi')) %>%
    mutate(SpeciesName=recode(SpeciesName, 'Fungi'="Lichen", "bare ground"='Bareground', "Bare ground"='Bareground', 'Bryophyta'= 'Moss', 'Stone'='Rock')) %>%
    select(UniqueID, SpeciesName, Cover, Rel_Cover) %>% group_by(UniqueID, SpeciesName) %>% summarize(OtherCover=sum(Cover), Rel_OtherCover=sum(Rel_Cover)) %>%
    rename(CoverClass=SpeciesName)
  return(list(comm=comm, cover=cover))

}


# Clean taxa list (add these to end of above)
CleanTaxa_CH_Lavey <- function(community_CH_Lavey) {
  taxa <- unique(community_CH_Lavey$SpeciesName)
  return(taxa)
}

# Clean metadata
CleanMeta_CH_Lavey <- function(community_CH_Lavey){
  dat <- 
    community_CH_Lavey %>%
    #mutate(destBlockID=NA) %>%
    select(-SpeciesName, -Cover) %>%
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'PRA'=1400, 'MAR'= 1750, 'CRE'=1950, 'RIO'=2200)),
           Gradient = "CH_Lavey",
           Country = as.character("Switzerland"),
           YearEstablished = 2016,
           PlotSize_m2 = 1)
  
  return(dat)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_CH_Lavey <- function(){
  
  ### IMPORT DATA
  community_CH_Lavey_raw = ImportCommunity_CH_Lavey()
  
  ### CLEAN DATA SETS
  cleaned_CH_Lavey = CleanCommunity_CH_Lavey(community_CH_Lavey_raw)
  community_CH_Lavey = cleaned_CH_Lavey$comm
  cover_CH_Lavey = cleaned_CH_Lavey$cover
  meta_CH_Lavey = CleanMeta_CH_Lavey(community_CH_Lavey)
  taxa_CH_Lavey = CleanTaxa_CH_Lavey(community_CH_Lavey)
  #trait_NO_Norway = CleanTrait_NO_Norway(trait_NO_Norway_raw) 
  
  # Make list
  CH_Lavey = list(meta = meta_CH_Lavey,
                  community = community_CH_Lavey,
                  cover = cover_CH_Lavey,
                  taxa = taxa_CH_Lavey)
  
  return(CH_Lavey)
}


