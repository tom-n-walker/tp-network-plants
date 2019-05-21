##### Simple Test Analysis ####
#C&D 14.12.2018

#AnalyzeSR <- function(p) {

  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
data.list <- list(CH_Calanda, CN_Damxung, CN_Gongga, DE_Grainau, FR_AlpeHuez, IN_Kashmir, SE_Abisko, US_Colorado)
data.list <- list(NO_Gudmedalen, NO_Lavisdalen, NO_Skjellingahaugen, NO_Ulvhaugen, CH_Lavey)
  #not lavey because it's an ass
#JOIN METADATA AND COMMUNITY DATA
  # SR <-
  #   data.list %>% 
  #   map(~right_join(.$community, .$meta, by='destSiteID')) %>%
  #   bind_rows(., .id='Gradient')
names(data.list) <- c("CH_Calanda", "CN_Damxung", "CN_Gongga", "DE_Grainau", "FR_AlpeHuez", "IN_Kashmir", "SE_Abisko", "US_Colorado")
  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
  #fix block issue and then bind
  SR <- data.list %>% map(~mutate(.$community, destBlockID=as.character(destBlockID), Cover=as.numeric(Cover))) %>%
    bind_rows(.id='Region') %>% 
    select(Region, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    filter(!Cover==0) %>%
    group_by(Region, destSiteID, Treatment, destPlotID) %>% 
    nest() %>% 
    mutate(specrich = map(data, ~summarize(., SR=n_distinct(SpeciesName)))) %>%
    unnest(specrich) %>%
    filter(!Treatment %in% c('NettedControl','Control')) 
    
  
  #add metadata to organize elevations
  meta <- data.list %>% map(~mutate(.$meta, destBlockID=as.character(destBlockID))) %>%
    bind_rows(.id='Region') %>% 
    select(Region, destSiteID, Elevation) %>% 
    distinct()
 
  dat <- left_join(meta, SR)
  
  #PLOT SR
  dat %>% group_by(Region) %>% filter(Elevation==min(Elevation), !Treatment=='Cold') %>%
  ggplot(aes(x=Treatment, y=SR)) + geom_violin() + facet_wrap(~Region) + theme_bw() +
    theme(panel.grid = element_blank()) + ylab('Species Richness') + ggtitle('Lowest elevation sites')
  
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
   
   rda1<- data.list %>% map(~mutate(.$community, destBlockID=as.character(destBlockID), Cover=as.numeric(Cover))) %>%
     bind_rows(.id='Region') %>% 
     select(Region, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover)  
     
   
   dat <- left_join(meta, rda1)
      rar <- dat %>% 
        filter(!is.na(SpeciesName), Treatment %in% c('Warm', 'LocalControl')) %>%
        group_by(Region, Treatment) %>% 
        filter(Year==max(Year), Elevation==min(Elevation)) %>% 
        select(-Year, -destSiteID, -Elevation) %>%
        nest() %>%
        mutate(comm = map(data, ~spread(., SpeciesName, Cover, fill=0)),
             rda = map(comm, ~{
               com <- select(., -(destPlotID:Treatment))
               pred <- select(., destPlotID:Treatment)
               rda(com ~ Treatment, pred)}),
             rda_output = map2(.x=rda, .y=comm, ~fortify(.x, display = c("wa","cn"))))
   