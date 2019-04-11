##### Simple Test Analysis ####
#C&D 14.12.2018

#AnalyzeSR <- function(p) {

  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
data.list <- list(CH_Calanda, CN_Damxung, CN_Gongga, DE_Grainau, FR_AlpeHuez, IN_Kashmir, SE_Abisko, US_Colorado)
  #not lavey because it's an ass
#JOIN METADATA AND COMMUNITY DATA
  # SR <-
  #   data.list %>% 
  #   map(~right_join(.$community, .$meta, by='destSiteID')) %>%
  #   bind_rows(., .id='Gradient')

data.list=list(CN_Damxung, DE_Grainau)
  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
  #bind first then calculate
  SR <-data.list %>% map_df('community', .id='Gradient') %>% 
    select(Gradient, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    group_by(Gradient, destSiteID, Treatment) %>% 
    nest() %>% 
    mutate(specrich = map(data, ~summarize(., SR=n_distinct(SpeciesName)))) %>%
    unnest(specrich) #%>%
    #ggplot(aes(x=Treatment, y=SR)) + geom_boxplot() + facet_wrap(~destSiteID)
  
  #fix block issue and then bind
  SR <-data.list %>% map(~mutate(.$community, destBlockID=as.character(destBlockID))) #%>%
    map_df('community', .id='Gradient') %>% 
    select(Gradient, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    group_by(Gradient, destSiteID, Treatment) %>% 
    nest() %>% 
    mutate(specrich = map(data, ~summarize(., SR=n_distinct(SpeciesName)))) %>%
    unnest(specrich)
 
  #GET OVERLAP OF HIGH AND LOW SPECIES
  data.list %>% map_df('community', .id='Gradient') %>% 
    select(Gradient, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    group_by(Gradient, destSiteID, Treatment) %>% 
    nest() %>% 
    mutate(splist = map(data, ~select(., SpeciesName))) %>%
    filter(!Treatment=='Warm') %>%
    select(-data, -Treatment) %>%
    arrange(destSiteID) %>%
    mutate(overlap = map(splist, ~summarise(overlap = length(intersect(splist[[1]]$SpeciesName, splist[[2]]$SpeciesName))))) %>%
    unnest(overlap)

  
 #### Code to produce RDA per site (for final year, low site treatments)  
   library(vegan)
   data.list<- list(a = DE_Grainau, b=FR_AlpeHuez)
   
   rda1<- data.list %>% 
     map(~full_join(.$community, .$meta, by='destSiteID')) %>%
     bind_rows(., .id='Gradient') %>%
     select(Gradient, destSiteID, destPlotID, Treatment, Year, SpeciesName, Elevation, Cover) %>% 
     filter(!is.na(SpeciesName), Treatment %in% c('Warm', 'LocalControl')) %>%
     group_by(Gradient, Treatment) %>% 
     filter(Year==max(Year), Elevation==min(Elevation)) %>% 
     select(-Year, -destSiteID, -Elevation) %>%
     nest() %>%
      mutate(comm = map(data, ~spread(., SpeciesName, Cover, fill=0)),
             rda = map(comm, ~{
               com <- select(., -(turfID:Treatment))
               pred <- select(., turfID:Treatment)
               rda(com ~ Treatment, pred)}),
             rda_output = map2(.x=rda, .y=comm, ~fortify(.x, display = c("wa","cn"))))
   
   
   #Trying to first merge metadata (elevation) and community data
   rda1<- 
     
     
     #map over data.list, left_join(.$comm, .$)
     
     data.list %>% map_df('community', .id='Gradient') %>% 
     select(Gradient, destSiteID, turfID, Treatment, Year, SpeciesName, Cover) %>% 
     filter(!is.na(SpeciesName), destSiteID %in% c('Fauske','Ovstedal'), Treatment %in% c('Warm', 'Control')) %>%
     group_by(Gradient, Treatment) %>% 
     filter(Year==max(Year)) %>% select(-Year, -destSiteID) %>%
     nest() %>%
     mutate(comm = map(data, ~spread(., SpeciesName, Cover, fill=0)),
            rda = map(comm, ~{
              com <- select(., -(turfID:Treatment))
              pred <- select(., turfID:Treatment)
              rda(com ~ Treatment, pred)}),
            rda_output = map2(.x=rda, .y=comm, ~fortify(.x, display = c("wa","cn"))))
             
               
            #pca_graph = m)
                       #layers = c("sites", "centroids", "biplot")
                      


           