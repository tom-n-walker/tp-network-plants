####################
###  US_Arizona ####
####################



#### Import Community ####
ImportCommunity_US_Arizona <- function(){
  community_US_Arizona_raw<-read_excel("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "Community Data 2014-2018")
  return(community_US_Arizona_raw)
} 



#### Cleaning Code ####
# Cleaning Arizona community data
CleanCommunity_US_Arizona <- function(community_US_Arizona_raw){
  dat <- community_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) %>%
    gather('SpeciesName', 'Cover', -Year, -Date, -originSiteID, -destSiteID, -Treatment,-destPlotID) %>%
    mutate(
      SpeciesName = case_when(
        SpeciesName=="unk.grass"~"Poacae sp.",
        SpeciesName=="unk.forb"~"Forb sp.",
        SpeciesName=="unk.germinant"~"Germinant sp.",
        TRUE~SpeciesName))
    

  return(dat)
}



CleanMeta_US_Arizona <- function(community_US_Arizona){
  dat <- community_US_Arizona %>% 
    select(-c('SpeciesName', 'Cover')) %>% 
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'MC' = '2620', 'PP' = '2344')),
           Gradient = 'US_Arizona',
           Country = 'USA',
           YearEstablished = 2014,
           PlotSize_m2 = 0.09
    )
  
  
  return(dat)
}

# Cleaning species list 
# NOTE This species list now can't talk to the community data. How do we think about this?

CleanTaxa_US_Arizona <- function(){
  splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(
    SpeciesName = paste(Genus, Species),
    SpeciesName = case_when(
      Code=="unk.grass"~"Poacae sp.",
      Code=="unk.forb"~"Forb sp.",
      Code=="unk.germinant"~"Germinant sp.",
      TRUE~SpeciesName)
  )
  taxa <- splist$SpeciesName
      
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Arizona <- function(){
  
  ### IMPORT DATA
  community_US_Arizona_raw = ImportCommunity_US_Arizona()
  
  
  ### CLEAN DATA SETS
  ## US_Arizona
  
  community_US_Arizona = CleanCommunity_US_Arizona(community_US_Arizona_raw)
  meta_US_Arizona = CleanMeta_US_Arizona(community_US_Arizona)
  taxa_US_Arizona = CleanTaxa_US_Arizona()
  
  
  # Make list
  US_Arizona = list(meta =  meta_US_Arizona,
                     community = community_US_Arizona,
                     taxa = taxa_US_Arizona,
                     trait = NA)
  
  return(US_Arizona)
}

