library("codyn")
source('R/theme_ggplot.R')
temp <- read.csv('./climate/worlclim2_processedtemp.csv')

#replace incorrect site names in temp
temp$site <- as.character(temp$site)
temp$site[26:44] <- c("High", "Low", "Gudmedalen", "Arhelleren", "Rambera", 
                      "Lavisdalen", "Vikesland", "Hogsete", 
                      "Skjellingahaugen", "Ovstedal", "Veskre", 
                      "Ulvhaugen", "Fauske", "Alrust", "High", "Low", "Mid", "MC", "PP")
#[26:44] replaced
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


#Join temp data
Temp <- temp %>% select(gradient, site, T_ann_cor)

dd_temp <- left_join(dd, Temp, by=c("Region"="gradient", "originSiteID"="site")) %>% 
  mutate(T_ann_origin = T_ann_cor) %>% select(-T_ann_cor)
dd_Temp <- left_join(dd_temp, Temp, by=c("Region"="gradient", "destSiteID"="site")) %>% 
  mutate(T_ann_dest = T_ann_cor, T_warm = T_ann_dest-T_ann_origin) %>% 
  select(-T_ann_cor)

#No data for DE_Susalps, US_Arizona is looking weird (I think sites reversed, making absolute for now)
#also three data points (18-20 missing NA)

# Calculate per plot
dd_T <- dd_Temp %>%
  filter(!Region %in% c("DE_Susalps", "CN_Damxung")) %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Year_0 = Year-min(Year)))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Temp_cum = Year_0*abs(T_warm)))) %>%
  unnest(dat) %>% 
  filter(comdyn %in% c('C', 'E', 'T'))  

colour_comdyn <- c("#fe875d", "#356288", "black")

dd_T %>% 
  ggplot(aes(x=Temp_cum, y=value, color=comdyn, group = interaction(Region, destPlotID, comdyn))) + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=interaction(Region, comdyn), color=comdyn), method='lm', se=F) +
  #geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  TP_theme() + 
  facet_grid(~comdyn) +
  labs(color="Process", y="Proportional change in communities", x=expression('Cumulative Warmed Temperature ('~degree*C~')'))

#Average rate ~ max cumulative temperature <- should I standardize the rate? Or maybe just make it between year 1 and year max
dd_T %>% group_by(Region, destSiteID, destPlotID, comdyn) %>%
  summarize(value=mean(value), Temp_cum = max(Temp_cum)) %>%
  ggplot(aes(x=Temp_cum, y=value, color=comdyn)) + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_point() +
  geom_smooth(aes(group=interaction(Region, comdyn), color=comdyn), method='lm', se=F) +
  #geom_smooth(aes(group=comdyn, color=comdyn), method='lm', se=F) +
  TP_theme() + 
  labs(color="Process", y="Proportional change in communities", x=expression('Cumulative Warmed Temperature ('~degree*C~')'))

#With region as fixed effect
#Colonisation
dd_C <- dd_T %>% filter(comdyn=='C')
m1<-lme(value ~ Temp_cum*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
summary(m1)
mnull<-lme(value ~ Temp_cum+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  #***, 26.7
mnull1<-lme(value ~ Temp_cum, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 30.4
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) #**, 7.7

##Extinction
dd_C <- dd_T %>% filter(comdyn=='E')
m1<-lme(value ~ Temp_cum*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
mnull<-lme(value ~ Temp_cum+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  #***, 57.3
mnull1<-lme(value ~ Temp_cum, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 39.4
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) 

##Turnover
dd_C <- dd_T %>% filter(comdyn=='T')
m1<-lme(value ~ Temp_cum*Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
mnull<-lme(value ~ Temp_cum+ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(m1, mnull)  
mnull1<-lme(value ~ Temp_cum, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull1) #***, 46.3
mnull2<-lme(value ~ Region, random = ~1|originSiteID, method = "ML", data=dd_C) 
anova(mnull, mnull2) 


#Now calculate per slope value

library(broom)
colour_comdyn <- c("#fe875d", "#356288", "black")
dd_E <- dd_Temp %>%
  mutate(comm_sim = map(comm, ~.x %>% select(Year, ODT, destPlotID) %>% 
                          distinct() %>%
                          mutate(Year_diff=max(Year) - min(Year)) %>%
                          select(-Year))) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Year_0 = Year-min(Year)))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Temp_cum = Year_0*abs(T_warm)))) %>%
  unnest(dat) %>% 
  filter(comdyn %in% c('C', 'E', 'T')) %>% 
  group_by(Region, destSiteID, destPlotID, T_warm, Year_diff, comdyn) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(value ~ Year, data = .x))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>%
  filter(term == 'Year')

dd_E %>%
  ggplot(aes(x=T_warm*Year_diff, y=abs(estimate), color=comdyn)) + 
  geom_point() + 
  scale_colour_manual(values = colour_comdyn) +
  stat_smooth(method = "nls", formula = y ~ a * exp(-S * x), 
              method.args = list(start = list(a = 78, S = 0.02)), aes(group=comdyn, color=comdyn), se=F) +
  TP_theme() + 
  labs(color="Process", y="Absolute slope over time", x=expression(Delta*'Temperature ('~degree*C~')')) 
