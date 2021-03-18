###### DE_Susalps community data functions ####
#Chelsea Chisholm & Joshua Lynn, 18.03.2021

load_cover_DE_Susalps <- function(){
  #import data
  comm <- read_delim(file = "./data/DE_Susalps/DE_Susalps_commdata/SusAlps_biomass_2016-2020_cleanedFINAL.csv", delim=";")
  cover <- comm %>% 
    rename(year = Harvest_title, turfID = Plot_label, originSiteID = Origin_site, destSiteID = Transplant_site, SpeciesName = Species_name, FunctionalGroup = Functional_group) %>%
    separate(year, c("year", "harvest"), ' ') %>% #all controls harvested at peak season, don't need to keep this column
    mutate(year = gsub('\\..*', '', year)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(bm = as.numeric(gsub("\\,", ".", bm))) %>%
    filter(treatment == 'ctrl') %>%
    select(originSiteID, destSiteID, year, turfID, SpeciesName, FunctionalGroup, bm)
  
  return(cover)
}
