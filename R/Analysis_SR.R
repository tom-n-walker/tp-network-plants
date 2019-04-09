##### Simple Test Analysis ####
#C&D 14.12.2018

#summarize data and analyze Species richness ~treatment by Year and Site
NMDS <- function(x) {
  
  x %>% metaMDS(.) 
}

library(ggfortify) #use pack for github repos

AnalyzeSR <- function(p) {

  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
  SR <- x$community %>% group_by(destSiteID, Treatment, turfID) %>% summarize(SR=n_distinct(SpeciesName)) %>%
    ggplot(aes(x=Treatment, y=SR)) + geom_boxplot() + facet_wrap(~destSiteID)
    #overlap of species between low and high sites
 
  #GET OVERLAP OF HIGH AND LOW SPECIES
  SR <- x$community %>% group_by(destSiteID, Treatment) %>% 
     nest() %>% 
     mutate(splist = map(data, ~select(., SpeciesName))) %>%
     filter(!Treatment=='Warm') %>%
     select(-data, -Treatment) %>%
     arrange(destSiteID) %>%
     summarise(overlap = length(intersect(splist[[1]]$SpeciesName, splist[[2]]$SpeciesName)))

  
 #### Code to produce RDA per site (for final year, low site treatments)  
   library(vegan)
   data.list<- list(a = NO_Ulvhaugen, b=NO_Skjellingahaugen)
   
   rda1<- data.list %>% map_df('community', .id='Gradient') %>% 
     select(Gradient, destSiteID, turfID, Treatment, Year, SpeciesName, Cover) %>% 
     filter(!is.na(SpeciesName), destSiteID %in% c('Fauske','Ovstedal'), Treatment %in% c('TT2', 'Control')) %>%
     group_by(Gradient, Treatment) %>% 
     filter(Year==max(Year)) %>% select(-Year, -destSiteID) %>%
     nest() %>%
      mutate(comm = map(data, ~spread(., SpeciesName, Cover, fill=0)),
             rda = map(comm, ~{
               com <- select(., -(turfID:Treatment))
               pred <- select(., turfID:Treatment)
               rda(com ~ Treatment, pred)}),
             rda_output = map2(.x=rda, .y=comm, ~fortify(.x, display = c("wa","cn"))))
   
   rda1<- data.list %>% map2(.x='community', .y='meta', ~full_join(., by='destSiteID'))
     reduce(left_join, by=c('destSiteID'))
     
     data.list %>% map_df('community', .id='Gradient') %>% 
     select(Gradient, destSiteID, turfID, Treatment, Year, SpeciesName, Cover) %>% 
     filter(!is.na(SpeciesName), destSiteID %in% c('Fauske','Ovstedal'), Treatment %in% c('TT2', 'Control')) %>%
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
                      


           