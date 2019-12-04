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
CleanCommunity_US_Arizona <- function(community_US_Arizona_raw, cover_US_Arizona){
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
#calculate percentage cover per individual
    left_join(cover_US_Arizona)  %>% spread('CoverClass', 'OtherCover') %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) 

  dat2 <- dat %>%
    group_by(UniqueID, Year, originSiteID, destSiteID, destPlotID, Treatment, Collector) %>%
    mutate(Rel_Cover = (VascCover / sum(Individuals, na.rm=T) * Individuals)/100) %>%
    bind_rows(dat) %>% 
    filter(Rel_Cover > 0)   #omg so inelegant
   

   dat3 <- dat2 %>% select(-'OtherCover', -'VascCover', -'Rel_OtherCover', -'<NA>') #what's with this NA? Fix it...
    

  return(dat3)
}



# Cleaning Arizona meta data
CleanMeta_US_Arizona <- function(community_US_Arizona){
  dat <- community_US_Arizona %>% 
    select(-c('SpeciesName', 'Date', 'Individuals', 'Rel_Cover')) %>% 
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
CleanCover_US_Arizona <- function(cover_US_Arizona_raw){
  classcover <- cover_US_Arizona_raw %>% 
    select(-c('Teabag number', 'TransplantNET Treatment')) %>% 
    mutate(destSiteID = str_extract(Plot, pattern = "^.{2}")) %>% 
    rename(Date = 'Date Collected', originSiteID = 'Ecosystem', Treatment = 'Warming.Treat', destPlotID = 'Plot', VascCover = '% cover at peak biomass') %>% 
    mutate(Treatment = recode (Treatment, "Warming" = "Warm")) %>%
    mutate(UniqueID = paste(Year, originSiteID, destSiteID, destPlotID, sep='_')) %>%
    mutate(OtherCover = 100-VascCover) %>%
    gather('CoverClass', 'OtherCover', c(VascCover, OtherCover)) %>% 
    mutate(Rel_OtherCover=100) #and all this sums to 100 which is perfect
    
  return(classcover)
}





# Cleaning Arizona species list 

CleanTaxa_US_Arizona <- function(){
  splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(
    Species_FullName = paste(Genus, Species)) %>%
    rename(SpeciesName='Code')
  
  #,
    #SpeciesName = case_when(
    #  Code=="unk.grass"~"Poacae sp.",
    #  Code=="unk.forb"~"Forb sp.",
    # Code=="unk.germinant"~"Germinant sp.",
    #  TRUE~SpeciesName))
  
  taxa <- splist
      
  return(taxa)
}


#### IMPORT, CLEAN AND MAKE LIST #### 
ImportClean_US_Arizona <- function(){
  
  ### IMPORT DATA
  community_US_Arizona_raw = ImportCommunity_US_Arizona()
  cover_US_Arizona_raw = ImportCover_US_Arizona()
  
  
  ### CLEAN DATA SETS
  ## US_Arizona
  cover_US_Arizona = CleanCover_US_Arizona(cover_US_Arizona_raw)
  community_US_Arizona = CleanCommunity_US_Arizona(community_US_Arizona_raw, cover_US_Arizona)
  meta_US_Arizona = CleanMeta_US_Arizona(community_US_Arizona)
  taxa_US_Arizona = CleanTaxa_US_Arizona()
  
  
  # Make list
  US_Arizona = list(community = community_US_Arizona,
                     meta =  meta_US_Arizona,
                     cover = cover_US_Arizona,
                     taxa = taxa_US_Arizona)
  
  
  return(US_Arizona)
}

