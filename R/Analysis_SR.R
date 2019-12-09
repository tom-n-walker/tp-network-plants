##### Basic functions for analyses ####
#C&D 14.12.2018

# Species Richness per plot across sites
  dat %>% 
  group_by(Region) %>%
  mutate(Turf = case_when(Elevation == max(Elevation) & Treatment == "LocalControl" ~ "Alpine Control",
                               Elevation == min(Elevation) & Treatment == "LocalControl" ~ "Low Control",
                               Elevation == min(Elevation) & Treatment == "Warm" ~ "Warmed Turfs")) %>%
  filter(Year==max(Year), !is.na(Turf)) %>%
  group_by(Region, destSiteID, Turf, destPlotID) %>% 
  summarise(SR = n_distinct(SpeciesName)) %>%
  ggplot(aes(x=Turf, y=SR)) + geom_boxplot() + facet_wrap(~Region, ncol = 3) +
    theme_bw()


  dat %>% group_by(Region, Elevation) %>%
    distinct(Treatment) %>% arrange(Region, Elevation, Treatment) %>% View
# Average relative cover across sites
  dat %>% 
  group_by(Region, destSiteID, destPlotID) %>% 
  #summarise(SR = n_distinct(SpeciesName), ra=mean(Rel_Cover)) %>%
  ggplot(aes(x=Treatment, y=Rel_Cover)) + geom_boxplot() + facet_wrap(~Region, ncol = 3) +
    theme_bw()



  
  
  #GET SPECIES RICHNESS AT PLOT LEVEL PER TREATMENT*SITE
  #fix block issue and then bind
    
  comm <- alldat %>% map(~mutate(.$community, destPlotID=as.numeric(destPlotID), 
                                    Cover=as.numeric(Rel_Cover))) %>%
  bind_rows(.id='Region')
  
  SR <- alldat %>% map(~mutate(.$community, .id="Region")) %>%
    map_dfr(.$community, .id='Region') %>% 
    filter(!Cover==0, !Cover==is.na(Cover)) %>%
    select(Region, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    group_by(Region, destSiteID, Treatment, destPlotID) %>% 
    filter(Year==max(Year), !Treatment %in% c('NettedControl','Control')) %>%
    summarise(SR = n_distinct(SpeciesName)) 
  


  
  


 #### Code to produce RDA per site (for final year, low site treatments)
   library(vegan)

   rda1<- alldat %>% map(~mutate(.$community, destBlockID=as.character(destBlockID), Cover=as.numeric(Cover))) %>%
     bind_rows(.id='Region') #%>%
     select(Region, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover)  %>%
     filter(!Cover==0, !Cover==is.na(Cover)) %>%
     select(Region, destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>%
     group_by(Region, destSiteID, Treatment, destPlotID) %>%
     filter(Year==max(Year), !Treatment %in% c('NettedControl','Control'))

 #   
 #   dat <- left_join(meta, rda1)
 #   dat<- tibble::rowid_to_column(dat)
 #      rar <- dat %>% 
 #        filter(!is.na(SpeciesName), Treatment %in% c('Warm', 'LocalControl')) %>%
 #        group_by(Region) %>% 
 #        filter(Elevation==min(Elevation)) %>% 
 #        select(-Year, -destSiteID, -Elevation) %>%
 #        nest() %>%
 #        mutate(comm = map(data, ~spread(., SpeciesName, Cover, fill=0)))
 #      blah <- rar %>% mutate(rda = map(comm, ~{
 #               com <- select(., -(rowid:Treatment))
 #               pred <- select(., destPlotID:Treatment)
 #               rda(com ~ Treatment, pred)}),
 #             rda_output = map2(.x=rda, .y=comm, ~plot(.x, display = c("wa","cn")))) 
 #             
 #             
 #             
 #      blah %>% map(.x=rda, .y=comm, ~autoplot)
 #      #old option:   rda_output = map2(.x=rda, .y=comm, ~fortify(.x, display = c("wa","cn"))))
 #   
 #      
 #      #GET OVERLAP OF HIGH AND LOW SPECIES from control plots
 #      blerg1 <- dat %>% filter(Treatment == 'LocalControl') %>% 
 #        select(Region, Elevation, SpeciesName, Cover) %>% 
 #        group_by(Region, Elevation) %>% 
 #        nest() %>% 
 #        mutate(splist = map(data, ~select(., SpeciesName))) %>%
 #        select(-data) %>%
 #        arrange(Region, Elevation) %>%
 #        group_by(Region) %>%
 #        filter(Elevation == min(Elevation) | Elevation == max(Elevation)) %>%
 #        group_by(Region) %>%
 #        mutate(Elevation = ifelse(Elevation == min(Elevation), 'Low', 'High')) %>%
 #        spread(Elevation, splist) %>%
 #        mutate(overlap = map2(.x=High[[1]], .y=Low[[1]], intersect), 
 #               tot = map2(.x=High[[1]], .y=Low[[1]], union),
 #               prop=map2(.x=overlap, .y=tot, ~length(.x)/length(.y))) %>% 
 #        unnest(prop) 
 #      #GET SPECIES LIST FROM WARM TRANSPLANTED TURFS AT LOW ELEVATION
 #      dat %>% filter(Treatment == 'Warm') %>% #filter for warmed plots at low elevation to just get that species list, then join with above
 #        select(Region, Elevation, SpeciesName, Cover) %>% 
 #        group_by(Region) %>% 
 #        filter(Elevation==min(Elevation)) %>%
 #        nest() %>% 
 #        mutate(splist_warmed = map(data, ~select(., SpeciesName))) %>%
 #        select(-data) %>%
 #        left_join(., blerg1) %>%
 #        mutate(lowinhigh = map2(.x=splist_warmed[[1]], .y=Low[[1]], intersect),
 #               nooverlap = map2(.x=High[[1]], .y=Low[[1]], setdiff), 
 #               #lowinhigh = map2(.x=overlap, .y=Low, union),
 #               prop=map2(.x=lowinhigh, .y=splist_warmed, ~as.numeric(.x)/length(.y))) %>% 
 #        unnest(prop) 
 #      
 #      
 #      #species present in alpine at low elevation not present in alpine at high
 #        
 #               #old code, still use intersect but need to figure out how to use map2 with lists...
 #        #        mutate(overlap = map(splist, ~summarise(overlap = length(intersect(.[[1]]$SpeciesName, .[[2]]$SpeciesName))))) %>%
 #        # unnest(overlap)