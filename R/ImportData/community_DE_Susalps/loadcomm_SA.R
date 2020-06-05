###### DE_Susalps community data functions ####
#Chelsea Chisholm, 20.04.2020

load_cover_DE_Susalps <- function(){
  #import data
  comm <- read_excel(path = "./data/DE_Susalps/DE_Susalps_commdata/SUSALPS_TransplantNet_2016-2019_Stand 28April2020.xlsx", sheet=2)
  plotid <- read_excel(path = "./data/DE_Susalps/DE_Susalps_commdata/SUSALPS_TransplantNet_2016-2019_Stand 28April2020.xlsx", sheet=3)
  cover <- comm %>% 
    left_join(., plotid) %>%
    rename(year = Harvest_title, turfID = Plot_label, originSiteID = Origin_site, destSiteID = Transplant_site, SpeciesName = Species_name, FunctionalGroup = Functional_group) %>%
    separate(year, c("year", "harvest"), ' ') %>% #all controls harvested at peak season, don't need to keep this column
    mutate(year = gsub('\\..*', '', year)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    filter(treatment == 'ctrl') %>%
    select(originSiteID, destSiteID, year, turfID, SpeciesName, FunctionalGroup, bm)
  
  return(cover)
}
