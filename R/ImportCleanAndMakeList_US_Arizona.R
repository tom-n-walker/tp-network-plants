####################
###  US_Arizona ####
####################



#### Import Community ####
ImportCommunity_US_Arizona <- function(){
  community_US_Arizona_raw<-read_excel("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "Community Data 2014-2018")
  return(community_US_Arizona_raw)
} 

# Import Cover class #

ImportCover_US_Arizona <- function(){
  cover_US_Arizona_raw<-read_excel("data/US_Arizona/US_Arizona_commdata/Arizona percent cover data_TransplantNET_Rubin & Hungate 2019.xlsx", sheet = "% green 2014-2018")
  return(cover_US_Arizona_raw)
} 


#### Cleaning Code ####

# Cleaning Arizona community data
CleanCommunity_US_Arizona <- function(community_US_Arizona_raw){
  dat <- community_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) %>%
    gather('SpeciesName', 'Individuals', -Year, -Date, -originSiteID, -destSiteID, -Treatment,-destPlotID) %>%
#adding species names from species list (Taxa: splist) to dataframe
 #   left_join(splist, by = c("code" = "Code")) %>% 
 #   select(-c('Genus', 'Species', 'Family', 'Group', 'Common name')) %>% 
#creating unique ID
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_'), Collector='Rubin') %>% # I think we can leave out the destSiteID here because it is embedded in destPlotID..
    #group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment ) %>% why do we do this?
#calculate percentage cover per individual
    left_join(cover_US_Arizona)  

  dat2 <- dat %>%
    group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment, Collector) %>%
    summarise(Rel_Cover = (VascCover / sum(Individuals, na.rm=T) * Individuals)/100) %>%
    bind_rows(dat) %>% 
    filter(Cover >= 0)  %>% #omg so inelegant
    mutate(Total_Cover = sum(Cover), Rel_Cover = Cover / Total_Cover)
  
    mutate(Rel_Cover = (VascCover / IndPlot * Individuals)/100) %>% 
    select(-c('code', 'OtherCover'))
    

  return(dat)
}



# Cleaning Arizona meta data
CleanMeta_US_Arizona <- function(community_US_Arizona){
  dat <- community_US_Arizona %>% 
    select(-c('SpeciesName', 'Date', 'Individuals', 'VascCover', 'IndPlot', 'Rel_Cover')) %>% 
    distinct() %>% 
    mutate(Elevation = as.numeric(recode(destSiteID, 'MC' = '2620', 'PP' = '2344')),
           Gradient = 'US_Arizona',
           Country = 'USA',
           YearEstablished = 2014,
           PlotSize_m2 = 0.09
    )
  
  
  return(dat)
}

# Clean Arizona Cover Class 
CleanCover_US_Arizona <- function(cover_US_Arizona){
  classcover <- cover_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot', VascCover = '% cover at peak biomass') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%
    #group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment ) %>% 
    mutate(OtherCover = 100-VascCover)
  
  return(classcover)
}





# Cleaning Arizona species list 

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
  cover_US_Arizona_raw = ImportCover_US_Arizona()
  
  
  
  ### CLEAN DATA SETS
  ## US_Arizona
  
  community_US_Arizona = CleanCommunity_US_Arizona(community_US_Arizona_raw)
  meta_US_Arizona = CleanMeta_US_Arizona(community_US_Arizona)
  cover_US_Arizona = CleanCover_US_Arizona()
  taxa_US_Arizona = CleanTaxa_US_Arizona()
  
  
  
  # Make list
  US_Arizona = list(community = community_US_Arizona,
                     meta =  meta_US_Arizona,
                     cover = cover_US_Arizona,
                     taxa = taxa_US_Arizona,
                     trait = NA)
  
  
  return(US_Arizona)
}

