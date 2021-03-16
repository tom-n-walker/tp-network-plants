library("codyn")
source('R/theme_ggplot.R')

#### COLONISATION PATTERNS ACROSS SITES ####
#turnover function from codyn

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
  filter(Region %in% c("NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "CN_Damxung", "CN_Gongga", "US_Arizona", "DE_Susalps", "SE_Abisko", "IT_MatschMazia")) %>%
  mutate(specrich = map(comm, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 

#### Plot SR patterns ####
#dest control, origin control, warmed
colour_odt <- c("#A92420", "#016367", "#FBC00E")

dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(specrich, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = SR)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~Region, nrow=2) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(color = "Treatment", y = 'Species Richness') +
  labs(title = 'Species Richness over time', color = "Treatment") 

#### Plot colonisation patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(colonisation, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
    ggplot(aes(x = Year, y = appearance)) + 
    scale_colour_manual(values = colour_odt) + 
    geom_line(aes(group=destPlotID, color=ODT), alpha=0.2) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~Region, nrow=2) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Colonisation over time', color = "Treatment") 

#### Plot extinction patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(extinction, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = disappearance)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~Region, nrow=2) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(title = 'Extinction over time', color = "Treatment") 

#### Plot turnover patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = total)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~Region, nrow=2) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(title = 'Turnover over time', color = "Treatment") 


#### Plot C, E and T for only transplanted turfs ####
colour_comdyn <- c("#fe875d", "#356288", "black")
colour_comdyn <- c("#49BEB7", "black", "black")
#"#7496D2", "#CECD7B"
#c("#fe875d", "#49BEB7", "black")

dd %>%
  filter(Region != "IT_MatschMazia") %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>% 
  filter(ODT == "warmed", comdyn %in% c('C', 'E')) %>% 
  ggplot(aes(x=Year, y=value, color=comdyn, group = interaction(destPlotID, comdyn))) + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  facet_wrap(~Region, nrow=2) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2011,2013,2015, 2017)) + 
  labs(color="Process", y="Proportional change", x='Year') 

#On one graph, with years scaled to 0 +

dd_T <- dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Year_0 = Year-min(Year)))) %>%
  unnest(dat) %>% 
  filter(ODT == "warmed") %>%
  filter(comdyn%in% c('C', 'E', 'T'))  

dd_T %>% 
  ggplot(aes(x=Year_0, y=value, color=comdyn, group = interaction(Region, destPlotID, comdyn))) + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=interaction(Region, comdyn), color=comdyn), method='lm', se=F) +
  #geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  TP_theme() + 
  facet_grid(~comdyn) +
  labs(color="Process", y="Proportional change in communities", x='Duration (years)') 

library(nlme)
library(emmeans)
#Colonisation
dd_C <- dd_T %>% filter(comdyn=='C')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
mnull<-lme(value ~ Year_0+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  #***, 99.582
mnull1<-lme(value ~ Year_0, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 42.692
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) #**, 8.674

##Extinction
dd_C <- dd_T %>% filter(comdyn=='E')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
mnull<-lme(value ~ Year_0+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  #***, 100.610
mnull1<-lme(value ~ Year_0, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 59.958
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) 

##Turnover
dd_C <- dd_T %>% filter(comdyn=='T')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
mnull<-lme(value ~ Year_0+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  #***, 50.267
mnull1<-lme(value ~ Year_0, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 68.218
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) 

#With region as fixed effect
#Colonisation
dd_C <- dd_T %>% filter(comdyn=='C')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1)
anova(m1)
mnull<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)

##Extinction
dd_C <- dd_T %>% filter(comdyn=='E')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1)
anova(m1)

##Turnover
dd_C <- dd_T %>% filter(comdyn=='T')
m1<-lme(value ~ Year_0*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1) 
anova(m1)


#### Extract elevation and compare ####
library(broom)
colour_comdyn <- c("#fe875d", "#356288", "black")
dd_E <- dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(Elevation, ODT, destPlotID) %>% 
                          distinct() %>%
                          mutate(Elev_diff=max(Elevation - min(Elevation))))) %>% 
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  unnest(dat) %>% 
  group_by(Region, destSiteID, Elevation, Elev_diff, comdyn) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(value ~ Year, data = .x))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>%
  filter(term == 'Year') 

dd_E %>%
  ggplot(aes(x=Elev_diff, y=abs(estimate), color=comdyn)) + 
  geom_point() + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  TP_theme() + 
  labs(color="Process", y="Absolute slope over time", x=expression(Delta*Elevation)) 

#Colonisation
dd_C <- dd_E %>% filter(comdyn=='C')
m1<-lme(abs(estimate) ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1)
anova(m1)

##Extinction
dd_C <- dd_E %>% filter(comdyn=='E')
m1<-lme(abs(estimate) ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1)
anova(m1)

##Turnover
dd_C <- dd_E %>% filter(comdyn=='T')
m1<-lme(abs(estimate) ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1) 
anova(m1)

## Elevation but not slope estimates, just to raw values
dd_E <- dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(Elevation, ODT, destPlotID) %>% 
                          distinct() %>%
                          mutate(Elev_diff=max(Elevation - min(Elevation))))) %>% 
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  unnest(dat) 

dd_E %>%
  ggplot(aes(x=Elev_diff, y=value, color=comdyn)) + 
  geom_point() + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  TP_theme() + 
  labs(color="Process", y="Proportional change", x=expression(Delta*Elevation)) 


#Colonisation
dd_C <- dd_E %>% filter(comdyn=='C')
m1<-lme(value ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1)
anova(m1)

dd_C <- dd_E %>% filter(comdyn=='C')
m1<-lm(value ~ Elev_diff, data=dd_C) 
summary(m1) 

##Extinction
dd_C <- dd_E %>% filter(comdyn=='E')
m1<-lme(value ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1)
anova(m1)

dd_C <- dd_E %>% filter(comdyn=='E')
m1<-lm(value ~ Elev_diff, data=dd_C) 
summary(m1) 

##Turnover
dd_C <- dd_E %>% filter(comdyn=='T')
m1<-lme(value ~ Elev_diff, random = ~1|Region, method = "ML", data=dd_C) 
summary(m1) 
anova(m1)

dd_C <- dd_E %>% filter(comdyn=='T')
m1<-lm(value ~ Elev_diff, data=dd_C) 
summary(m1) 



#### Extract year since establishment and compare ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% mutate(Year_diff=max(Year - min(Year))) %>%
                                select(Elevation, Year_diff, ODT, destPlotID) %>% 
                                distinct())) %>% 
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  unnest(dat) %>% 
  group_by(Region, Year_diff, comdyn) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(value ~ Year, data = .x))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>%
  filter(term == 'Year') %>%
  ggplot(aes(x=Year_diff, y=estimate, color=comdyn)) + 
  geom_point() + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_smooth(method = "nls", method.args = list(formula = "y~(a/(b+x))", algorithm = 'plinear', start=list(a=1,b=0.5)), aes(group=comdyn, color=comdyn), se=F) +
  TP_theme() + 
  labs(y="Absolute slope over time", x=expression(Delta*Time)) +
  labs(color="Process") 

## Compare distributions of change in these three

dd %>%
  mutate(comm_sim = map(comm, ~.x %>% mutate(Year_diff=max(Year - min(Year))) %>%
                          select(Elevation, Year_diff, ODT, destPlotID) %>% 
                          distinct())) %>% 
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  unnest(dat) %>% 
  group_by(Region, Year_diff, comdyn) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(value ~ Year, data = .x))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>%
  filter(term == 'Year') %>%
  ggplot(aes(x=estimate, color=comdyn)) + 
  geom_density() + 
  scale_colour_manual(values = colour_comdyn) +
  TP_theme() + 
  labs(y="Number of sites", x=expression(Delta*Slope)) +
  labs(color="Process") 


#ISSUES WITH MULTIPLE SPECIES: "DE_Grainau", "US_Montana", "CN_Heibei", "FR_AlpeHuez"
#ISSUES WITH DESTPLOT ID: "CH_Calanda", "US_Colorado", "FR_Lautaret"
#Keep in mind, Colorado, Lautaret will have only one year of data, so Calanda is really the only issue here

#### Extra code ####
# ##Code with only points for plotting
# dd %>%
#   mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
#   mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
#   unnest(dat) %>%
#   ggplot(aes(x = Year, y = total, color = ODT)) + 
#   geom_point() +
#   scale_colour_manual(values = colour_odt) + 
#   geom_smooth(method = "lm", se = FALSE) +
#   facet_wrap(~ Region) +
#   TP_theme() + 
#   labs(title = 'Turnover over time', color = "Treatment Comparisons") 
# 
# ##Code for using intersect manually
# dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
#   distinct() %>% 
#   filter(Treatment == "Warm") %>% 
#   select(-Treatment) %>% 
#   mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
#     bind_rows(
#       originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
#       destControls = dat %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
#       warmed =  dat %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
#       .id = "ODT") %>%
#       arrange(Region, originSiteID, turfID, Year)
#   })) %>% 
#   mutate(ID = map(comm, function(x) x %>% group_by(ODT, destPlotID) %>% 
#                     distinct(SpeciesName) %>% nest())) %>%
#   unnest(ID) #%>%
# group_by(Region, Year, ODT, destPlotID) %>%
#   mutate(overlap = map(data, ~for (i in 1:length(.x)) {intersect(.x[i], .x[i+1])})) 
# 
# 
