####################
###  US_Arizona ####
####################



#### Import Community ####
ImportCommunity_US_Arizona <- function(){
  community_US_Arizona_raw<-read_excel("data/US_Arizona/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "Community Data 2014-2018")
  return(community_US_Arizona_raw)
} 



#### Cleaning Code ####
# Cleaning Arizona community data
CleanCommunity_US_Arizona <- function(community_US_Arizona_raw){
  dat <- community_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) 
    
           
# Actions to take:  
# Split column destPlotID in two columns by '_' , call the first column containing MC or PP destSiteID.
# Change names in column Treatment from 'Warming' to Warm. DONE

  return(dat)
}

# Clean metadata 
# This is mostly correct. Just fill in the 'actions to take' as above.

CleanMeta_US_Arizona <- function(community_US_Arizona_raw){
  dat <- community_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) %>% 
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

# full names found in other sheet of excel. This is still DIRECTLY COPIED from Abisko, but the principle is similar. Need to figure out.
# Do we need to collapse all the species under each other? How?

CleanTaxa_US_Arizona <- function(community_US_Arizona_raw){
  files <- list.files("data/US_Arizona/US_Arizona_commdata/") %>% 
    grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)
  splist <- map_df(files, ~ read_excel(paste0("data/US_Arizona/US_Arizona_commdata/", .), sheet = "Species list 2012"))
  rar <- c(splist$Name...2, splist$Name...4)
  rar <- rar[!is.na(rar)]
  taxa <- rar[!rar %in% c('Lichen', 'Litter', 'Moss')]
  
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Arizona <- function(){
  
  ### IMPORT DATA
  community_US_Arizona_raw = ImportCommunity_US_Arizona()
  
  
  ### CLEAN DATA SETS
  ## US_Arizona
  
  community_US_Arizona = CleanCommunity_US_Arizona(community_US_Arizona_raw)
  meta_US_Arizona = CleanMeta_US_Arizona(community_US_Arizona_raw)
  taxa_US_Arizona = CleanTaxa_US_Arizona(community_US_Arizona_raw)
  
  
  # Make list
  US_Arizona = list(meta =  meta_US_Arizona,
                     community = community_US_Arizona,
                     taxa = taxa_US_Arizona,
                     trait = NA)
  
  return(US_Arizona)
}

