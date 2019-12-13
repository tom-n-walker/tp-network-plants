##### Basic functions for analyses ####
#C&D 14.12.2018
library(ggordiplots)
library(ggpubr)
library(vegan)

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
  ggplot(SR, aes(x=Turf, y=SR)) + geom_boxplot() + facet_wrap(~Region, ncol = 3, scales='free') +
    theme_classic() + xlab('Treatment') + ylab('Species Richness')  
  # ggsave("./figures/Speciesrichness.png")
  
  # Average relative cover across sites
  ggplot(SR, aes(x=Turf, y=ra)) + geom_boxplot() + facet_wrap(~Region, ncol = 3, scales='free') +
    theme_classic() + xlab('Treatment') + ylab('Ave. Relative Abundance')
  # ggsave("./figures/Rel_abundance.png")

 #### RDA PER SITE (for final year, low site treatments) ####
  
  #need to remove a duplicate for Lavey, for now added mean function in pivot 
  dat_wide <- dat1 %>%
  group_by(Region) %>%
  filter(Year==max(Year)) %>%
  select(-Year, -destSiteID, -Elevation, -Rel_Elevation) %>%
  nest() %>%
  mutate(wide = map(data, ~pivot_wider(., names_from = SpeciesName, values_from = Rel_Cover, values_fill=list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))))
                        
  rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- dplyr::select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0)
    pred <- dplyr::select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)}),
    rda_output = map2(.x=rda, .y=wide, ~{gg_ordiplot(.x ,groups=.y$Turf, kind='sd', pt.size = 1 )}))
  
  plots1 <- map2(rda1$rda_output, rda1$Region, ~(.x$plot +
                                                  theme(plot.margin = unit(c(1,1,1,1),"cm")) +
                                                  labs(title = .y) + 
                                                  theme_classic()))

  ggarrange(plots1[[1]], plots1[[2]],plots1[[3]], plots1[[4]],
            plots1[[5]], plots1[[6]],plots1[[7]], plots1[[8]],
            plots1[[9]], plots1[[10]],plots1[[11]], plots1[[12]],
            ncol=6, nrow=2, common.legend = TRUE, legend="bottom")

  
  #Excluding rare species
  dat1 <- dat %>%
    group_by(Region) %>%
    mutate(Rel_Elevation = case_when(Elevation==min(Elevation) ~ 'Low',
                                     Elevation==max(Elevation) ~ 'High')) %>% 
    filter(!is.na(Rel_Elevation)) %>% 
    mutate(Turf = case_when(Rel_Elevation == 'High' & Treatment == "LocalControl" ~ "Alpine Control",
                            Rel_Elevation == 'Low' & Treatment == "LocalControl" ~ "Low Control",
                            Rel_Elevation == 'Low' & Treatment == "Warm" ~ "Warmed Turfs")) %>%
    group_by(Region, Year, UniqueID) %>%
    filter(Rel_Cover >= quantile(Rel_Cover, 0.5)) %>% #filter by percentile
    ungroup()
  
  dat1 %>% group_by(Region) %>% summarize(m=min(Rel_Cover))
    
  dat_wide <- dat1 %>%
    group_by(Region) %>%
    filter(Year==max(Year)) %>%
    select(-Year, -destSiteID, -Elevation, -Rel_Elevation) %>%
    nest() %>%
    mutate(wide = map(data, ~pivot_wider(., names_from = SpeciesName, values_from = Rel_Cover, values_fill=list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))))
  
  rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- dplyr::select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0) %>% decostand(method='hellinger')
    pred <- dplyr::select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)}),
    rda_output = map2(.x=rda, .y=wide, ~{gg_ordiplot(.x ,groups=.y$Turf, kind='sd', pt.size = 1 )}))
  
  plots1 <- map2(rda1$rda_output, rda1$Region, ~(.x$plot +
                                                   theme(plot.margin = unit(c(1,1,1,1),"cm")) +
                                                   labs(title = .y) + 
                                                   scale_color_manual(values=c('Alpine Control'='navyblue', 'Low Control'='goldenrod', 'Warmed Turfs'='red')) +
                                                   theme_classic()))
  
  ggarrange(plots1[[1]], plots1[[2]],plots1[[3]], plots1[[4]],
            plots1[[5]], plots1[[6]],plots1[[7]], plots1[[8]],
            plots1[[9]], plots1[[10]],plots1[[11]], plots1[[12]],
            ncol=6, nrow=2, common.legend = TRUE, legend="bottom")
   ggsave("./figures/RDA.png",
                  width = 40, height = 20, units = "cm")
  
  #Find centroid per turf per year per site
  dat1 <- dat %>%
    group_by(Region) %>%
    mutate(Rel_Elevation = case_when(Elevation==min(Elevation) ~ 'Low',
                                     Elevation==max(Elevation) ~ 'High')) %>% 
    filter(!is.na(Rel_Elevation)) %>% 
    mutate(Turf = case_when(Rel_Elevation == 'High' & Treatment == "LocalControl" ~ "Alpine Control",
                            Rel_Elevation == 'Low' & Treatment == "LocalControl" ~ "Low Control",
                            Rel_Elevation == 'Low' & Treatment == "Warm" ~ "Warmed Turfs")) %>%
    group_by(Region, Year, UniqueID) %>%
    filter(Rel_Cover >= quantile(Rel_Cover, 0.5)) %>% #filter by percentile
    ungroup()
  
  dat_wide <- dat1 %>%
    group_by(Region, Year) %>%
    select(-destSiteID, -Elevation, -Rel_Elevation) %>%
    nest() %>%
    mutate(wide = map(data, ~pivot_wider(., names_from = SpeciesName, values_from = Rel_Cover, values_fill=list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))))
  
  rda2 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- dplyr::select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0) %>% decostand(method='hellinger')
    pred <- dplyr::select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)}),
    rda_centroids = map(rda, ~{summary(.)$centroids}))  #,
    rda_dist1_warm_low = map(rda, ~{summary(.)$centroids[3,1]-summary(.)$centroids[1][2,1]}), #first axis
    rda_dist2_warm_low= map(rda, ~{summary(.)$centroids[1][3,2]-summary(.)$centroids[1][2,2]})) %>% #second axis
  #alpine is first, low second and warmed 3rd in rda output
    unnest(rda_dist1_warm_low, rda_dist2_warm_low)
  
  rda1 %>% ggplot(aes(x=Year, y=rda_dist1_warm_low))
  #For species pool, colonisers at low elevation should be species at high abundance, but what if relatively rare, seed bank, 
  #Centroid distance? Maybe some summary of variation in the groups
  #Are warmed turfs becoming more similar to low because of selective loss vs. colonisation?
    #extinction debt vs colonisation lag? Increasing SR/decreasing SR and what causes it.
  
  #For species which are completely new (seed bank?) check with site owners, maybe we send a list of the full species and then get them to key.
    #Highlight issues species.
  #Through loss or gain of species?

  
  
#### EXTRA CODE ####
  
  #For plotting rdas
  #library(cowplot)
  #cowplot::plot_grid(plotlist = plots1) #includes all plots on one, but no common legend
  # gg_ordiplot(ord, groups, scaling = 1, choices = c(1, 2),
  #             kind = c("sd", "se", "ehull"), conf = NULL, show.groups = "all",
  #             ellipse = TRUE, label = FALSE, hull = FALSE, spiders = FALSE,
  #             pt.size = 3, plot = TRUE)
  
  #Base plot version
  # rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
  #                                   comm <- select(., -(originSiteID:Turf))
  #                                   comm1 <- comm %>% replace(is.na(.), 0)
  #                                   pred <- select(., originSiteID:Turf)
  #                                   rda(comm1 ~ Turf, pred)}),
  #                             rda_output = map2(.x=rda, .y=wide, ~{plot(.x, display = c("sites"), type = 'p')
  #                                                                       ordiellipse(.x, group = .y$Turf, scaling = "symmetric",
  #                                                                             kind = "ehull", col = 1:3, lwd=3)
  #                                                                       grouped = levels(.y$Turf)
  #                                                                       legend('topright', legend=unique(.y$Turf), col=1:3, lty = 1)
  #                                                                       }),
  #                              plots = map2(.x=rda_output, .y=Region, ~{.x
  #                                                                      title(main = .y)}))
  
