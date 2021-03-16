library(FD)
#use traitdat_f from communitytraits.R
traitdat_all <- traitdat %>% mutate(SpeciesName=species) %>% 
  filter(!rowSums(is.na(.[,9:15]))==7) %>%
  select(-species)

dd <- traitdat_all %>%
  group_by(Region, originSiteID, destSiteID) %>%
  nest() %>%
  mutate(comm_meta  = map(data, select, ODT, destPlotID, Year), 
         comm_abund = map(data, select, Rel_Cover),
         comm_traits = map(data, select, leaf_area:SLA), 
         trait_scaled = map(comm_traits, ~scale(.x)),
         trait_dist = map(trait_scaled, ~dist(.x))) %>% 
  mutate(nmds = map(trait_dist, ~monoMDS(.x))) %>%
  mutate(scores = map(nmds, ~scores(.x, display='sites'))) %>% 
  mutate(scores_all = map(scores, ~data.frame(MDS1=.x[,1], MDS2=.x[,2]))) %>%
  mutate(scores_all = map2(.x = comm_meta, .y = scores_all, bind_cols)) 

dd <- traitdat_all %>%
  group_by(Region, originSiteID, destSiteID) %>%
  nest() %>%
  mutate(comm_wide = map(data, ~{
    .x %>% select(ODT, Year, Rel_Cover, destPlotID, leaf_area:SLA) %>% 
      group_by(ODT, Year, destPlotID) %>%
      summarize_at(vars(leaf_area:SLA), ~weighted.mean(., w = Rel_Cover, na.rm=T)) %>%
      ungroup() %>%
      select(ODT, Year, destPlotID, leaf_area:SLA)  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID, Year), 
         comm_traits = map(comm_wide, select, leaf_area:SLA), 
         trait_scaled = map(comm_traits, ~log(.x)),
         trait_dist = map(trait_scaled, gowdis)) %>% 
  mutate(nmds = map(trait_dist, ~monoMDS(.x))) %>%
  mutate(scores = map(nmds, ~scores(.x, display='sites'))) %>% 
  mutate(scores_all = map(scores, ~data.frame(MDS1=.x[,1], MDS2=.x[,2]))) %>%
  mutate(scores_all = map2(.x = comm_meta, .y = scores_all, bind_cols)) 


dd2 <- dd %>% 
  select(-(data:scores)) %>% 
  unnest(scores_all) %>%
  group_by(Region, ODT, Year) %>% 
  summarise_at(vars(matches("MDS")), .funs = mean) %>% 
  group_by(Region, Year) %>% 
  nest() %>% 
  mutate(distances = map(data, ~dist(select(.x, matches("MDS"))))) %>% 
  mutate(distances = map(distances, ~tibble(what = c("Low_High", "Low_TP", "High_TP"), dist = as.vector(.x)))) %>% 
  unnest(distances)

colour_cd <- c("#A92420", "darkgrey", "#016367")

dd2 %>%  #filter(!Region %in% c("FR_Lautaret", "US_Colorado", "IN_Kashmir")) %>%
  #filter(Region %in% c("CH_Calanda", "US_Montana", "CN_Damxung", "CN_Gongga", "NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "DE_Grainau", "SE_Abisko", "DE_Susalps", "FR_Lautaret", "IN_Kashmir", "US_Colorado", "IT_MatschMazia", "US_Arizona", "CN_Heibei", "FR_AlpeHuez"))%>%
  filter(what != "Low_High") %>%
  ggplot(aes(x = Year, y = dist, color = what)) + 
  TP_theme() +
  geom_point() +
  scale_colour_manual(values = colour_cd) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Region) + 
  labs(color = "Treatment Comparisons", y="Distance between centroids", x='Year') 


## Plot by duration of time on one graph
colour_cd <- c("#49BEB7", "black", "black")
ddcent <- dd2 %>%  filter(!Region %in% c("FR_Lautaret", "US_Colorado", "IN_Kashmir")) %>%
  filter(Region %in% c("CH_Calanda", "US_Montana", "CN_Damxung", "CN_Gongga", "NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "DE_Grainau", "SE_Abisko", "DE_Susalps", "FR_Lautaret", "IN_Kashmir", "US_Colorado", "IT_MatschMazia", "US_Arizona", "CN_Heibei", "FR_AlpeHuez"))%>% 
  group_by(Region) %>%
  mutate(Year_0 = Year-min(Year)) 

ddcent %>% filter(what != "Low_High") %>%
  ggplot(aes(x = Year_0, y = dist, color = what, group=interaction(Region, what))) + 
  TP_theme() +
  geom_point() +
  scale_colour_manual(values = colour_cd) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~what) + 
  labs(color = "Treatment Comparisons", y="Distance between centroids", x='Duration (years)') 

library(nlme)
library(emmeans)
#Controls
dd_C <- ddcent %>% filter(what=='Low_High')
m1<-lme(dist ~ Year_0*Region, random = ~1|destSiteID, method = "ML", data=dd_C) 
summary(m1)
anova(m1) #***, 6.37

##To low
dd_C <- ddcent %>% filter(what=='Low_TP')
m1<-lme(dist ~ Year_0, random = ~1|Region, data=dd_C)
summary(m1)
m1<-lm(dist ~ Year_0*Region, data=dd_C)
summary(m1)

##To alpine
dd_C <- ddcent %>% filter(what=='High_TP')
m1<-lme(dist ~ Year_0, random = ~1|Region, data=dd_C)
summary(m1)
m1<-lm(dist ~ Year_0*Region, data=dd_C)
summary(m1)


