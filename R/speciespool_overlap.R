#### SPECIES POOLS PER SITE/TREATMENT ####

#GET OVERLAP OF HIGH AND LOW SPECIES from control plots
     overlap <- dat %>% filter(Treatment == 'LocalControl') %>%
       select(Region, Elevation, SpeciesName, Cover) %>%
       group_by(Region, Elevation) %>%
       nest() %>%
       mutate(splist = map(data, ~select(., SpeciesName))) %>%
       select(-data) %>%
       arrange(Region, Elevation) %>%
       group_by(Region) %>%
       filter(Elevation == min(Elevation) | Elevation == max(Elevation)) %>%
       group_by(Region) %>%
       mutate(Elevation = ifelse(Elevation == min(Elevation), 'Low', 'High')) %>%
       spread(Elevation, splist) %>%
       mutate(High_n = map(High[[1]], length(unique(.))),
              Low_n = map(High[[1]], length(unique(.))),
              overlap = map2(.x=High[[1]], .y=Low[[1]], intersect),
              overlap_n = map(overlap, length),
              tot = map2(.x=High[[1]], .y=Low[[1]], union),
              prop=map2(.x=overlap, .y=tot, ~length(.x)/length(.y))) %>%
       unnest(prop, High_n, Low_n, overlap_n)
     
     #Check numbers, look at time since establishment.
     #Plot number of species in each pool + overlap
     overlap %>% select(Region, High_n, Low_n, overlap_n) %>% 
      gather(key = 'Total', value = 'Value', -Region) %>%
       ggplot(aes(x=Region, y=Value, fill=Total)) + 
       geom_bar(stat = "identity", position = position_dodge()) + xlab("Region") + ylab("Species Count")
     # ggsave("./figures/Speciespool_count.png",
     #        width = 40, height = 20, units = "cm")
     
     
#GET SPECIES LIST FROM WARM TRANSPLANTED TURFS AT LOW ELEVATION
     dat %>% filter(Treatment == 'Warm') %>% #filter for warmed plots at low elevation to just get that species list, then join with above
       select(Region, Elevation, SpeciesName, Cover) %>%
       group_by(Region) %>%
       filter(Elevation==min(Elevation)) %>%
       nest() %>%
       mutate(splist_warmed = map(data, ~select(., SpeciesName))) %>%
       select(-data) %>%
       left_join(., blerg1) %>%
       mutate(lowinhigh = map2(.x=splist_warmed[[1]], .y=Low[[1]], intersect),
              nooverlap = map2(.x=High[[1]], .y=Low[[1]], setdiff),
              #lowinhigh = map2(.x=overlap, .y=Low, union),
              prop=map2(.x=lowinhigh, .y=splist_warmed, ~as.numeric(.x)/length(.y))) %>%
       unnest(prop)
#
#
#      #species present in alpine at low elevation not present in alpine at high
#
#               #old code, still use intersect but need to figure out how to use map2 with lists...
#        #        mutate(overlap = map(splist, ~summarise(overlap = length(intersect(.[[1]]$SpeciesName, .[[2]]$SpeciesName))))) %>%
#        # unnest(overlap)