###### DE_Susalps community data functions ####
#Chelsea Chisholm, 20.04.2020

load_cover_DE_Susalps <- function(){
  #import data
  comm <- read_excel(path = "./data/DE_Susalps/SUSALPS_TransplantNet_2016-2019.xlsx", sheet=2)
  plotid <- read_excel(path = "./data/DE_Susalps/SUSALPS_TransplantNet_2016-2019.xlsx", sheet=3)
  cover <- comm %>% 
    left_join(., plotid) %>%
    rename(year = Harvest_title, turfID = Plot_label, originSiteID = Origin_site, destSiteID = Transplant_site, SpeciesName = Species_name, FunctionalGroup = Functional_group) %>%
    mutate(year = gsub('\\..*', '', year)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    select(originSiteID, destSiteID, year, turfID, SpeciesName, FunctionalGroup, bm)
  
  return(cover)
}
