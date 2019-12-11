##### Basic functions for analyses ####
#C&D 14.12.2018


# Sanity checks for dataset
#fulldat %>% group_by(Region, Elevation, Treatment, destPlotID) %>% summarize(n=n()) %>% View

#### PLOT-LEVEL SUMMARY DATA (SR, ABUND, EVENNESS) for final year ####
  dat1 <- dat %>%
  group_by(Region) %>%
  mutate(Rel_Elevation = case_when(Elevation==min(Elevation) ~ 'Low',
                                   Elevation==max(Elevation) ~ 'High')) %>% 
  filter(!is.na(Rel_Elevation)) %>% 
  mutate(Turf = case_when(Rel_Elevation == 'High' & Treatment == "LocalControl" ~ "Alpine Control",
                               Rel_Elevation == 'Low' & Treatment == "LocalControl" ~ "Low Control",
                               Rel_Elevation == 'Low' & Treatment == "Warm" ~ "Warmed Turfs")) %>%
  ungroup() 
    
  SR <- dat1 %>% 
    group_by(Region, destSiteID, Turf, destPlotID) %>% 
  filter(Year==max(Year)) %>%
  summarise(SR = n_distinct(SpeciesName), ra=mean(Rel_Cover))
  
    # Average SR across sites  
  ggplot(SR, aes(x=Turf, y=SR)) + geom_boxplot() + facet_wrap(~Region, ncol = 3) +
    theme_classic() + xlab('Treatment') + ylab('Species Richness')
  
  # Average relative cover across sites
  ggplot(SR, aes(x=Turf, y=ra)) + geom_boxplot() + facet_wrap(~Region, ncol = 3) +
    theme_classic() + xlab('Treatment') + ylab('Ave. Relative Abundance')


 #### RDA PER SITE (for final year, low site treatments) ####
  library(vegan)
  
  #Base plot version
  #need to remove a duplicate for Lavey, for now added mean function in pivot 
  dat_wide <- dat1 %>%
  group_by(Region) %>%
  filter(Year==max(Year)) %>%
  select(-Year, -destSiteID, -Elevation, -Rel_Elevation) %>%
  nest() %>%
  mutate(wide = map(data, ~pivot_wider(., names_from = SpeciesName, values_from = Rel_Cover, values_fill=list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))))
  
  rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
                                    comm <- select(., -(originSiteID:Turf))
                                    comm1 <- comm %>% replace(is.na(.), 0)
                                    pred <- select(., originSiteID:Turf)
                                    rda(comm1 ~ Turf, pred)}),
                              rda_output = map2(.x=rda, .y=wide, ~{plot(.x, display = c("sites"), type = 'p')
                                                                  ordiellipse(.x, group = .y$Turf, scaling = "symmetric",
                                                                              kind = "ehull", col = 1:3, lwd=3)
                                                                  grouped = levels(.y$Turf)
                                                                  legend('topright', legend=unique(.y$Turf), col=1:3, lty = 1)
                                                                  }))

  #ggoplot version
  library(ggordiplots)
  par(mfrow = c(6,2))
  rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0)
    pred <- select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)}),
    rda_output = map2(.x=rda, .y=wide, ~{gg_ordiplot(.x,groups=.y$Turf, kind='sd', pt.size = 2)+
                                        theme_classic()}))
  
  # gg_ordiplot(ord, groups, scaling = 1, choices = c(1, 2),
  #             kind = c("sd", "se", "ehull"), conf = NULL, show.groups = "all",
  #             ellipse = TRUE, label = FALSE, hull = FALSE, spiders = FALSE,
  #             pt.size = 3, plot = TRUE)
  
