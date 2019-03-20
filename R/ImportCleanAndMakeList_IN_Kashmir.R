####################
### IN_Kashmir  ###
####################

#### Import Community ####
ImportCommunity_IN_Kashmir <- function(){
  community_IN_Kashmir_14<-read_xlsx(path ="data/IN_Kashmir/background 2014.xlsx")
  community_IN_Kashmir_15<-read_xlsx(path ="data/IN_Kashmir/2015.xlsx")
  return(list(community_IN_Kashmir_14, community_IN_Kashmir_15))
} 


#### Cleaning Code ####
# Cleaning Kashmir community data
CleanCommunity_IN_Kashmir <- function(community_IN_Kashmir_raw){
    dat2 <- community_IN_Kashmir_raw %>% 
    bind_rows() %>% 
    select(c(SITE:`cover class`), -PLOT) %>% 
    rename(SpeciesName = `Species name` , Cover = `cover class` , destSiteID = SITE , destBlockID = BLOCK , turfID = PLOT.ID , Treatment = TREATMENT , Year = YEAR)%>% 
    mutate(originSiteID = strsplit(Treatment, '_')[[1]][1], 
           Treatment = case_when(Treatment =="low_turf" & destSiteID == "LOW" ~ "LocalControl" , 
                                 Treatment =="high_turf" & destSiteID == "LOW" ~ "Warm" , 
                                 Treatment =="high_turf" & destSiteID == "HIGH" ~ "LocalControl"))
    
    
  return(dat2)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_IN_Kashmir <- function(){
  
  ### IMPORT DATA
  community_IN_Kashmir_raw = ImportCommunity_IN_Kashmir()
 
  
  ### CLEAN DATA SETS
  ## IN_Kashmir

  community_IN_Kashmir = CleanCommunity_IN_Kashmir(community_IN_Kashmir_raw)
  
  # Make list
  IN_Kashmir = list(meta = NA,
                   metaCommunity = NA,
                   community = community_IN_Kashmir,
                   taxa = NA,
                   trait = NA)
  
  return(IN_Kashmir)
}

# Clean metadata

CleanMeta_IN_Kashmir <- function(community_IN_Kashmir_raw){
  dat <- 
    community_IN_Kashmir_raw %>% 
    select(siteID, turfID, Year) %>%
   # mutate(Treatment = recode(siteID, "RIO_RIO" = "Control", "PRA_PRAturf 2.xlsx" = "LocalControl", "PRA_RIO" = "Warm"),
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
