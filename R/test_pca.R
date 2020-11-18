dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
        bind_rows(
          originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
          destControls = dat %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
          warmed =  dat %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
          .id = "ODT")
      })) %>% 
  mutate(comm_wide = map(comm, ~{
    .x %>% select(ODT, Year, SpeciesName, Rel_Cover, destPlotID) %>% 
      pivot_wider(names_from = SpeciesName, values_from = Rel_Cover, values_fill = list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))
  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID, Year), 
         comm_spp = map(comm_wide, select, -ODT, -destPlotID, -Year)) %>% 
  select(-comm_wide) %>% 
  mutate(PCA = map(comm_spp, ~rda(sqrt(.x)))) %>% 
  mutate(scores = map(.x = PCA, .f = fortify, display = "wa", axes = 1:2)) %>% 
  mutate(scores = map2(.x = comm_meta, .y = scores, bind_cols))
  
dd2 <- dd %>% 
  select(-(comm:PCA)) %>% 
  unnest(scores) %>% 
  group_by(Region, originSiteID, destSiteID, ODT, Year) %>% 
  summarise_at(vars(matches("PC")), .funs = mean) %>% 
  group_by(Region, originSiteID, destSiteID, Year) %>% 
  nest() %>% 
  mutate(distances = map(data, ~dist(select(.x, matches("PC"))))) %>% 
  mutate(distances = map(distances, ~tibble(what = c("Low_High", "Low_TP", "High_TP"), dist = as.vector(.x)))) %>% 
  unnest(distances)

#### PLOT PCA ####
source('R/theme_ggplot.R')
library("patchwork")
colour_otd <- c("orange", "blue","green3")
 
pmap(dd, function(scores, Region, originSiteID, ...){
  scores %>% arrange(Year) %>% 
    ggplot(aes(x = PC1, y = PC2, colour  = ODT, scale_fill_manual(values = colour_otd), group = destPlotID)) +
    geom_point(mapping = aes(shape = Year == min(Year))) +
    scale_colour_manual(values = colour_otd) + 
    geom_path() +
    coord_equal() +
    TP_theme()+
    labs(title = paste(Region, originSiteID), shape = "First Year")
  })%>%
    wrap_plots() +
  plot_layout(guides = 'collect')
  
## Individual region PCA
#dest control, origin control, warmed
colour_odt <- c("#A92420", "#016367", "#FBC00E")
#shape_odt <- c(16,16,25)

p1 <- dd %>% filter(Region == "DE_Susalps") %>% #Insert desired region name
  pmap(function(scores, Region, originSiteID, ...){
    scores %>% arrange(Year) %>% 
    ggplot(aes(x = PC1, y = PC2, colour  = ODT, scale_fill_manual(values = colour_otd), group = destPlotID)) +
      geom_point(mapping = aes(size = Year == min(Year))) +
      scale_colour_manual(values = colour_odt) + 
      scale_fill_manual(values = colour_odt) +
      geom_path() +
      coord_equal() +
      TP_theme() +
      labs(title = "IT_MatschMazia", size = "First Year", color = "Treatment") 
  })

# Plotting 3x plots for display
# p2a <- p2[[2]]
# p1a <- p1[[3]]
# p3a <- p3[[2]]
# p1a + p2a + p3a
# library(ggpubr)
# ggarrange(p1a, p2a, p3a, ncol=3, nrow=1, common.legend = TRUE, legend="bottom", align = 'hv')
#Next!: remove rare species by region, max cover e.g. 1% or n occur ==1


### Plot centroid distance over time ####
colour_cd <- c("#A92420", "darkgrey", "#016367")
colour_cd <- c("#49BEB7", "black", "black")
 
dd2 %>%  filter(!Region %in% c("FR_Lautaret", "US_Colorado", "IN_Kashmir")) %>%
  filter(Region %in% c("CH_Calanda", "US_Montana", "CN_Damxung", "CN_Gongga", "NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "DE_Grainau", "SE_Abisko", "DE_Susalps", "FR_Lautaret", "IN_Kashmir", "US_Colorado", "IT_MatschMazia", "US_Arizona", "CN_Heibei", "FR_AlpeHuez"))%>%
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
m1<-lme(dist ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1)
anova(m1) #*, -0.006

##To low
dd_C <- ddcent %>% filter(what=='Low_TP')
m1<-lme(dist ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1)
anova(m1) #***, -0.034
mnull<-lme(dist ~ Year_0+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)
mnull1<-lme(dist ~ Year_0, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1)
mnull2<-lme(dist ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2)


##To alpine
dd_C <- ddcent %>% filter(what=='High_TP')
m1<-lme(dist ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1) 
anova(m1) #***, 0.028
mnull<-lme(dist ~ Year_0+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)
mnull1<-lme(dist ~ Year_0, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1)
mnull2<-lme(dist ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2)



#TRY TO DO: 
#Order by duration
#change shape to up and down triangle

### Plot centroid distance over time lags...is this necessary? ####
#Gives an indication of directionality and rate of change


