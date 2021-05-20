library("codyn")
library(broom)
source('R/theme_ggplot.R')
source('R/DynamicsAnalysis/utilityfuns_codyn.R')

#### COLONISATION PATTERNS ACROSS SITES ####
#turnover function from codyn

# Create ODT and nest communities
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
  })) 

# Create lists of invading species per plot, merge invaders and community data
invaders <- dd %>% select(Region, originSiteID, destSiteID, comm) %>%
    mutate(low = map(comm, ~{.} %>% #species pool at low site controls across all years (could do only in first year)
                       select(ODT, SpeciesName) %>% 
                       filter(ODT == "destControls") %>%
                       distinct(.$SpeciesName) %>% 
                       flatten_chr(.)),
           high = map(comm, ~{.} %>% #species pool of high site controls across all years
                        select(ODT, SpeciesName) %>% 
                        filter(ODT =="origControls") %>%
                        distinct(.$SpeciesName) %>% 
                        flatten_chr(.)),
           warmed = map(comm, ~{.} %>% #species pool of transplanted turfs across all years
                          select(ODT, SpeciesName) %>% 
                          filter(ODT =="warmed") %>%
                          distinct(.$SpeciesName) %>% 
                          flatten_chr(.)),
           overlap = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #overlap of species pool in transplanted turfs in year 1 in warmed vs. lowland species
                        select(ODT, SpeciesName) %>% 
                        filter(ODT %in% c("warmed", "destControls")) %>%
                        group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                        flatten_chr(.)),
           high_unique = map2(high, low, ~setdiff(.x, intersect(.x,.y))), #only species in high species pool
           resident = map2(high_unique, overlap, ~c(.x,.y)), #high species pool and overlap for transplanted turfs
           invader = map2(warmed, resident, ~setdiff(.x, intersect(.x,.y)))) %>% #those species in low species pool that are not in the high or overlap pool
      mutate(comm_inv = map2(comm, invader, .f=function(.x, .y) { .x %>% 
            filter(ODT == "warmed") %>%
            mutate(Pool = ifelse(SpeciesName %in% unique(.y), "invader", "resident"))}))



# Separate by group and look at colonisation or extinction per invaders or residents:
# Right now it is relative turnover (total num species observed across two time points). I need proportion gained or lost relative to total for two groups (invaders and residents).
test <- invaders %>% 
  select(Region, originSiteID, destSiteID, comm_inv) %>%
  mutate(specrich_inv = map(comm_inv, ~ {.} %>% filter(Pool=="invader") %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total")),
         specrich = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total")),
         specrich_res = map(comm_inv, ~ {.} %>% filter(Pool=="resident") %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 


#### Plot SR patterns (NOT WORKING WTF)####
#dest control, origin control, warmed
colour_odt <- c("#A92420", "#016367", "#FBC00E")
#colour_odt <- c("#A92420", "#016367")

SR <- test %>%
  select(Region, originSiteID, destSiteID, specrich, specrich_inv, specrich_res) %>%
  mutate(dat = pmap(.l=list(a=specrich, b=specrich_inv, c=specrich_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident"))

SR %>%
  ggplot(aes(x = Year, y = SR)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(color = "Treatment", y = 'Species Richness') +
  labs(title = 'Species Richness over time', color = "Treatment") 

SR %>% group_by(Region, originSiteID, destSiteID, destPlotID, type) %>%
  do(fitSR = tidy(lm(SR ~ Year, data = .))) %>% 
  unnest(fitSR) %>%
  filter(term=="Year") %>%
  ggplot(aes(x = type, y = estimate)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_boxplot(aes(color=type), alpha=0.2) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Species Richness') +
  labs(title = 'Species Richness over time', color = "Treatment") 
  

#Need to remove IN_Kashmir, FR_Lautaret and US_Colorado as only 2 years of data
#### Plot colonisation patterns ####

CO <- test %>% filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  select(Region, originSiteID, destSiteID, comm_inv, colonisation, colonisation_inv, colonisation_res) %>%
  mutate(comm_sim = map(comm_inv, ~.x %>% select(destPlotID) %>% distinct())) %>%
  mutate(dat = map2(colonisation, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat2 = pmap(.l=list(a=dat, b=colonisation_inv, c=colonisation_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat2) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident")) 

CO %>% 
  ggplot(aes(x = Year, y = appearance)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_point(aes(group=destPlotID, color=type), alpha=0.05) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Colonisation over time', color = "Treatment") 

CO %>% group_by(Region, originSiteID, destSiteID, destPlotID, type) %>%
  do(fitCO = tidy(lm(appearance ~ Year, data = .))) %>% 
  unnest(fitCO) %>%
  filter(term=="Year") %>%
  group_by(Region, type) %>%
  summarize(mid = mean(estimate), sd=sd(estimate, na.rm=T)) %>%
  ggplot() + 
  scale_colour_manual(values = colour_odt) + 
  scale_size_manual(values = c(4,2,2)) +
  geom_point(aes(y=mid, x=type, color=type, size=type)) +
  geom_linerange(aes(ymin=mid-sd, ymax=mid+sd, 
                     x=type, color=type)) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Species Richness') +
  labs(title = 'Colonisation over time', color = "Treatment") 

#### Plot exctinction patterns ####
test %>% filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  select(Region, originSiteID, destSiteID, comm_inv, extinction, extinction_inv, extinction_res) %>%
  mutate(comm_sim = map(comm_inv, ~.x %>% select(destPlotID) %>% distinct())) %>%
  mutate(dat = map2(extinction, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat2 = pmap(.l=list(a=dat, b=extinction_inv, c=extinction_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat2) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident")) %>%
  ggplot(aes(x = Year, y = disappearance)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Extinction over time', color = "Treatment") 

#### Plot turnover patterns ####
test %>% filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  select(Region, originSiteID, destSiteID, comm_inv, turnover, turnover_inv, turnover_res) %>%
  mutate(comm_sim = map(comm_inv, ~.x %>% select(destPlotID) %>% distinct())) %>%
  mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat2 = pmap(.l=list(a=dat, b=turnover_inv, c=turnover_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat2) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident")) %>%
  ggplot(aes(x = Year, y = total)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Turnover over time', color = "Treatment") 


#### Plot colonisaiton patterns ordered by temperature ####


# Read in temperature and compute cumulative temp for ordering graph

temp <- read.csv('./climate/worlclim2_processedtemp.csv')

#replace incorrect site names in temp
temp$site <- as.character(temp$site)
temp$site[1:3] <- c('Cal', 'Nes', 'Pea')
temp$site[14:17] <- c('3200', '3400', '3600', '3800')
temp$site[22:23] <- c('G', 'L')
temp$site[26:44] <- c("High", "Low", "Gudmedalen", "Arhelleren", "Rambera", 
                      "Lavisdalen", "Vikesland", "Hogsete", 
                      "Skjellingahaugen", "Ovstedal", "Veskre", 
                      "Ulvhaugen", "Fauske", "Alrust", "High", "Low", "Mid", "MC", "PP")

#Calculate annual cumulative warming
Temp <- temp %>% select(gradient, site, T_ann, year_range) 

dd_temp <- left_join(CO, Temp, by=c("Region"="gradient", "originSiteID"="site")) %>% 
  mutate(T_ann_origin = T_ann) %>% select(-T_ann, -year_range)
CO_Temp <- left_join(dd_temp, Temp, by=c("Region"="gradient", "destSiteID"="site")) %>% 
  mutate(T_ann_dest = T_ann, T_warm = T_ann_dest-T_ann_origin, T_warm_cum = T_warm*year_range) %>% 
  select(-T_ann)

CO_Temp %>% filter(!Region %in% c("CH_Lavey", "US_Montana")) %>%
                     group_by(Region, originSiteID, destSiteID, destPlotID, T_warm_cum, type) %>%
  do(fitCO = tidy(lm(appearance ~ Year, data = .))) %>% 
  unnest(fitCO) %>%
  filter(term=="Year") %>%
  group_by(Region, type) %>%
  summarize(mid = mean(estimate), sd=sd(estimate, na.rm=T), T_warm_cum = mean(T_warm_cum, na.rm=T)) %>%
  arrange(T_warm_cum) %>% 
  ggplot() + 
  scale_colour_manual(values = colour_odt) + 
  scale_size_manual(values = c(4,2,2)) +
  geom_point(aes(y=mid, x=type, color=type, size=type)) +
  geom_linerange(aes(ymin=mid-sd, ymax=mid+sd, 
                     x=type, color=type)) +
  facet_wrap(~Region, nrow=1) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Colonisation Rate') +
  labs(title = 'Colonisation over time', color = "Treatment") 

# #order by T_warm_cum
# dd_Temp_ave <- dd_Temp_ave %>% arrange(T_warm_cum)
# dd_Temp_ave$SiteID <- c('COL', 'SWE', 'MON', 'ARI', 'FRA1', 'SWI1', 'NOR1', 'FRA2', 'GER1', 'CHI1', 'CHI2', 'IND', 'CHI3', 'NOR2', 'SWI2', 'NOR3', 'NOR4', 'GER2', 'ITA')
# colfunc <- colorRampPalette(c("blue4", "#6A38B3", "#FE433C"))
# cols <- colfunc(19)
# cols <- data.frame(SiteID=dd_Temp_ave$SiteID, cols=cols)

