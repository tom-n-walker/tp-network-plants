##### Basic functions for analyses ####
#C&D 14.12.2018
library(ggordiplots)
library(ggpubr)
library(vegan)
library(wesanderson)


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
  
  
#### RDA PER SITE (for final year) ####
### Exclude rare species ###
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
  
  
  
### Make wide ###
  dat_wide <- dat1 %>%
    group_by(Region, destSiteID) %>%
    filter(Year==max(Year)) %>%
    group_by(Region) %>%
    select(originSiteID, destBlockID, destPlotID, Elevation, Turf, SpeciesName, Rel_Cover) %>%
    nest() %>%
    mutate(wide = map(data, ~pivot_wider(., names_from = SpeciesName, values_from = Rel_Cover, values_fill=list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))))
  

  
  

### Do RDA ###
  rda1 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- dplyr::select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0)
    pred <- dplyr::select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)})) 
  #    rda_output = map2(.x=rda, .y=wide, ~{gg_ordiplot(.x ,groups=.y$Turf, kind='sd', pt.size = 1 )}))
  
  # plots1 <- map2(rda1$rda_output, rda1$Region, ~(.x$plot +
  #                                                   theme(plot.margin = unit(c(1,1,1,1),"cm")) +
  #                                                   labs(title = .y) + 
  #                                                   theme_classic()))
  # 
  # ggarrange(plots1[[1]], plots1[[2]],plots1[[3]], plots1[[4]],
  #           plots1[[5]], plots1[[6]],plots1[[7]], plots1[[8]],
  #           plots1[[9]], plots1[[10]],plots1[[11]], plots1[[12]],
  #           ncol=6, nrow=2, common.legend = TRUE, legend="bottom")
  
### Make RDA2
  rda2 <- dat_wide %>% mutate(rda = map(wide, ~{
    comm <- dplyr::select(., -(originSiteID:Turf))
    comm1 <- comm %>% replace(is.na(.), 0) %>% decostand(method='hellinger')
    pred <- dplyr::select(., originSiteID:Turf)
    rda(comm1 ~ Turf, pred)}),
    d_centroid = map(rda, ~{dist(summary(.)$centroids)})) %>% 
    select(-data, -wide, -rda) %>%
    mutate(low_high = map(d_centroid, ~{.[1]}),
           warm_high = map(d_centroid, ~{.[2]}),
           warm_low = map(d_centroid, ~{.[3]})) %>%
    select(-d_centroid) %>%                  
    unnest() %>%
    gather(key='Treatment', value='Distance', -Region, -Year)  
  
### Divergence / Convergence graph
  divcon<- rda2 %>% 
    group_by(Region) %>% 
    filter(Year==max(Year) | Year == min(Year)) %>% 
    mutate(EndStart = case_when(Year==max(Year) ~ "End",
                                Year==min(Year) ~ "Start")) %>%
    select(-Year) %>% 
    pivot_wider(names_from = c(Treatment, EndStart), values_from = Distance) %>% 
    mutate(Controls = (low_high_End) - (low_high_Start),
           TP_Alpine = (warm_high_End) - (warm_high_Start),
           TP_Lowland = (warm_low_End) - (warm_low_Start)) %>% 
    select(-c( low_high_Start:warm_low_End)) %>% 
    pivot_longer(cols = -Region, names_to = "Compare", values_to = "Difference") %>% 
    mutate(Compare = factor(Compare, levels = c("Controls", "TP_Lowland", "TP_Alpine")))
  

# PLOTS
  ggplot(divcon, aes(x = Difference, y = Compare, color = Compare, size = 5))+     
    facet_wrap(~Region)+
    geom_point()+
    geom_vline(xintercept = 0)
  
  # left_join with year / duration info!
  # Then plot in horizontal bar graphs, sort by duration.
  # Next: 
  
    
##################################
#          OLD CODE BELOW        # 
##################################
  
#### Sanity checks for dataset #####
#dat1 %>% group_by(Region, Elevation, Turf, destPlotID) %>% summarize(n=n(), max=max(Rel_Cover)) %>% View
    
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
  dat1 %>% group_by(Region) %>% summarize(years=max(Year)-min(Year))
  dat_wide <- dat1 %>%
    group_by(Region) %>%
    filter(Year==max(Year)) %>%
    select(originSiteID, destBlockID, destPlotID, Turf, SpeciesName, Rel_Cover) %>%
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
  
  ggarrange(plots1[[3]], plots1[[12]],plots1[[1]], plots1[[7]], plots1[[4]], plots1[[5]], plots1[[11]], 
            plots1[[13]], plots1[[10]], plots1[[9]],plots1[[8]], plots1[[6]], plots1[[2]],
            ncol=7, nrow=2, common.legend = TRUE, legend="bottom")
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
                                        d_centroid = map(rda, ~{dist(summary(.)$centroids)})) %>% 
                      select(-data, -wide, -rda) %>%
                      mutate(low_high = map(d_centroid, ~{.[1]}),
                             warm_high = map(d_centroid, ~{.[2]}),
                             warm_low = map(d_centroid, ~{.[3]})) %>%
    select(-d_centroid) %>%                  
    unnest() %>%
    gather(key='Treatment', value='Distance', -Region, -Year)
  
  gd <- rda2 %>% group_by(Year, Treatment) %>% summarize(Distance=mean(Distance))
  rda2 <-rda2 %>% mutate(Treatment=as.factor(Treatment))
  levels(rda2$Treatment)
  levels(rda2$Treatment) <- c("Low_High", "Warmed_High", "Warmed_Low")
  
  rda2 %>% ggplot(aes(x=Year, y=Distance, col=Treatment, shape=Region), alpha = .3) + 
    geom_point() + 
    scale_color_manual(values = c("darkblue", "lightblue", "darkred")) +
    geom_line(stat="smooth",method = "lm", se=F, aes(group = Region), alpha = .3) +
    #stat_summary(aes(group=Treatment), fun.y=mean, geom='pointrange', se=F, alpha=0.8, size=2) +
    #geom_line(stat="smooth",method = "lm",data = gd, se=F, alpha = .8, size = 2) +
    labs(colour="Treatment Comparisons",
      x = "Year",
      y = "Distance between centroids") +
    theme_classic() + 
    facet_wrap(~Treatment)
  
  ggsave("./figures/RDA_centroid_distances.png")
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
  
  
  ####### Making divergence - convergence overview ########
  
 divcon<- rda2 %>% 
    group_by(Region) %>% 
    filter(Year==max(Year) | Year == min(Year)) %>% 
    mutate(EndStart = case_when(Year==max(Year) ~ "End",
                                Year==min(Year) ~ "Start")) %>%
    select(-Year) %>% 
    pivot_wider(names_from = c(Treatment, EndStart), values_from = Distance) %>% 
    mutate(Controls = (low_high_End) - (low_high_Start),
           TP_Alpine = (warm_high_End) - (warm_high_Start),
           TP_Lowland = (warm_low_End) - (warm_low_Start)) %>% 
    select(-c( low_high_Start:warm_low_End)) %>% 
    pivot_longer(cols = -Region, names_to = "Compare", values_to = "Difference") %>% 
    mutate(Compare = factor(Compare, levels = c("Controls", "TP_Lowland", "TP_Alpine")))
v