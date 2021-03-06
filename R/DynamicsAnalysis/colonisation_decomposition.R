library(codyn)
library(broom)
library(ggalt)
library(brms)
library(modelr)
library(tidybayes)
source('R/theme_ggplot.R')
source('R/DynamicsAnalysis/utilityfuns_codyn.R')

#### Make plant community cover relative abundance (at the moment includes categories like other, rock, moss, etc.)
dat2 <- dat %>%  
  select(Region, Year, originSiteID, destSiteID, destBlockID, Elevation, Treatment, destPlotID, SpeciesName, Rel_Cover) %>%
  filter(!is.na(Rel_Cover)) %>% #not creating other cover as biomass, doesn't exist
  mutate(Cover = Rel_Cover) %>%
  select(-Rel_Cover) %>%
  group_by(Region, Year, originSiteID, destSiteID, destBlockID, Elevation, Treatment, destPlotID) %>%
  mutate(Total_Cover = sum(Cover, na.rm=T), Rel_Cover = Cover / Total_Cover) %>%
  ungroup()

# Create ODT and nest communities
dd <- dat2 %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
    bind_rows(
      originControls = dat2 %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
      destControls = dat2 %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
      warmed =  dat2 %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
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
  mutate(specrich_inv = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% filter(Pool=="invader") %>%  summarize(SR=n_distinct(SpeciesName))), 
         relabund_inv = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% filter(Pool=="invader") %>%  summarize(RA=sum(Rel_Cover))), 
         colonisation_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover_inv = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="invader", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total")),
         specrich = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         relabund = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(RA=sum(Rel_Cover))), 
         colonisation = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover = map(comm_inv, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total")),
         specrich_res = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% filter(Pool=="resident") %>% summarize(SR=n_distinct(SpeciesName))), 
         relabund_res = map(comm_inv, ~ {.} %>% group_by(Year, destPlotID) %>% filter(Pool=="resident") %>% summarize(RA=sum(Rel_Cover))), 
         colonisation_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover_res = map(comm_inv, ~turnover_inv(.x, time.var= "Year", species.var= "SpeciesName", pool.var="resident", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 

#### Plot RA patterns ####
#dest control, origin control, warmed
colour_odt <- c("#A92420", "#016367", "#FBC00E")
#colour_odt <- c("#A92420", "#016367")

RA <- test %>%
  select(Region, originSiteID, destSiteID, relabund, relabund_inv, relabund_res) %>%
  mutate(dat = pmap(.l=list(a=relabund, b=relabund_inv, c=relabund_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident"))

RA %>%
  ggplot(aes(x = Year, y = RA)) + 
  scale_colour_manual(values = colour_odt) + 
  #geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_point(aes(group=destPlotID, color=type), alpha=0.05) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(color = "Treatment", y = 'Relative Abundance') +
  labs(title = 'Relative abundance over time', color = "Treatment") 

RA %>%
  group_by(Region, originSiteID, destSiteID, destPlotID) %>%
  filter(Year == max(Year)) %>%
  filter(type != "all") %>%
  ggplot(aes(x=RA, y=Region, fill=type)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values =c("darkred", "darkblue")) + 
  TP_theme()

RA %>% 
  group_by(Region, originSiteID, destSiteID, destPlotID) %>%
  filter(Year %in% range(Year)) %>%
  mutate(Range = ifelse(Year==min(Year), "Ymin", "Ymax")) %>%
  group_by(Region, Range, type) %>%
  summarize(RA = mean(RA)) %>%
  ungroup() %>%
  pivot_wider(values_from=RA, names_from=c(Range, type)) %>%
  ggplot(aes(x=Ymin_invader, xend=Ymax_invader, y=Region, group=Region)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=3,
                colour_xend = 'darkblue',
                colour_x = 'blue') + 
  scale_x_continuous() + 
  TP_theme()

RA_m <- RA %>% mutate(Gradient = paste(originSiteID, destSiteID, sep="_")) %>%
  mutate(rela = 0.00001+(1-2*0.00001)*(RA-min(RA))/(max(RA)-min(RA)))  %>%
  mutate(Years = scale(Year)) %>% 
  filter(type != "all")

mR = brm(
  rela ~ 0 + type + Years:type + (Years|Region/Gradient), 
  data = RA_m,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)


#### Plot SR patterns ####
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
  #geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_point(aes(group=destPlotID, color=type), alpha=0.05) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  labs(color = "Treatment", y = 'Species Richness') +
  labs(title = 'Species Richness over time', color = "Treatment") 


SR %>%
  group_by(Region, originSiteID, destSiteID, destPlotID) %>%
  filter(Year == max(Year)) %>%
  filter(type != "all") %>%
  ggplot(aes(x=SR, y=Region, fill=type)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values =c("darkred", "darkblue")) + 
  TP_theme()

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
  geom_point(aes(y=type, x=mid, color=type, size=type)) +
  geom_linerange(aes(xmin=mid-sd, xmax=mid+sd, 
                     y=type, color=type)) +
  geom_vline(aes(xintercept=0)) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Colonisation rate') +
  labs(title = 'Colonisation rate', color = "Treatment") 

CO_m <- CO %>% mutate(Gradient = paste(originSiteID, destSiteID, sep="_")) %>%
  mutate(col = 0.00001+(1-2*0.00001)*(appearance-min(appearance))/(max(appearance)-min(appearance)))  %>%
  mutate(Years = scale(Year))

mC = brm(
  col ~ Years*type + (1|Region/Gradient), 
  data = CO_m,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

#### Plot exctinction patterns ####
EX <- test %>% filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  select(Region, originSiteID, destSiteID, comm_inv, extinction, extinction_inv, extinction_res) %>%
  mutate(comm_sim = map(comm_inv, ~.x %>% select(destPlotID) %>% distinct())) %>%
  mutate(dat = map2(extinction, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat2 = pmap(.l=list(a=dat, b=extinction_inv, c=extinction_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat2) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident")) 

EX %>%
  ggplot(aes(x = Year, y = disappearance)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Extinction over time', color = "Treatment") 

EX %>% group_by(Region, originSiteID, destSiteID, destPlotID, type) %>%
  do(fitEX = tidy(lm(disappearance ~ Year, data = .))) %>% 
  unnest(fitEX) %>%
  filter(term=="Year") %>%
  group_by(Region, type) %>%
  summarize(mid = mean(estimate), sd=sd(estimate, na.rm=T)) %>%
  ggplot() + 
  scale_colour_manual(values = colour_odt) + 
  scale_size_manual(values = c(4,2,2)) +
  geom_point(aes(y=type, x=mid, color=type, size=type)) +
  geom_linerange(aes(xmin=mid-sd, xmax=mid+sd, 
                     y=type, color=type)) +
  geom_vline(aes(xintercept=0)) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Colonisation rate') +
  labs(title = 'Extinction rate', color = "Treatment") 

EX_m <- EX %>% mutate(Gradient = paste(originSiteID, destSiteID, sep="_")) %>%
  mutate(ext = 0.00001+(1-2*0.00001)*(disappearance-min(disappearance))/(max(disappearance)-min(disappearance)))  %>%
  mutate(Years = scale(Year))

mE = brm(
  ext ~  Years*type + (1|Region/Gradient), 
  data = EX_m,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

#### Plot turnover patterns ####
TU <- test %>% filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  select(Region, originSiteID, destSiteID, comm_inv, turnover, turnover_inv, turnover_res) %>%
  mutate(comm_sim = map(comm_inv, ~.x %>% select(destPlotID) %>% distinct())) %>%
  mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat2 = pmap(.l=list(a=dat, b=turnover_inv, c=turnover_res), function(a,b,c) bind_rows(list(a, b,c), .id="sp_pool"))) %>%
  unnest(dat2) %>%
  mutate(type = recode(sp_pool, "1"="all", "2"="invader", "3"="resident")) 

TU %>%
  ggplot(aes(x = Year, y = total)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=type), alpha=0.2) +
  geom_smooth(aes(group = type, color=type), method = "lm", se = F) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  scale_x_continuous(breaks=c(2010,2013,2016)) + 
  labs(title = 'Turnover over time', color = "Treatment") 

TU %>% group_by(Region, originSiteID, destSiteID, destPlotID, type) %>%
  do(fitTU = tidy(lm(total ~ Year, data = .))) %>% 
  unnest(fitTU) %>%
  filter(term=="Year") %>%
  group_by(Region, type) %>%
  summarize(mid = mean(estimate), sd=sd(estimate, na.rm=T)) %>%
  ggplot() + 
  scale_colour_manual(values = colour_odt) + 
  scale_size_manual(values = c(4,2,2)) +
  geom_point(aes(y=type, x=mid, color=type, size=type)) +
  geom_linerange(aes(xmin=mid-sd, xmax=mid+sd, 
                     y=type, color=type)) +
  geom_vline(aes(xintercept=0)) +
  facet_wrap(~Region, nrow=3) +
  TP_theme() + 
  labs(color = "Treatment", y = 'Colonisation rate') +
  labs(title = 'Turnover rate', color = "Treatment") 

TU_m <- TU %>% mutate(Gradient = paste(originSiteID, destSiteID, sep="_")) %>%
  mutate(tur = 0.00001+(1-2*0.00001)*(total-min(total))/(max(total)-min(total)))  %>%
  mutate(Years = scale(Year))

mT = brm(
  tur ~ Years*type + (1|Region/Gradient), 
  data = TU_m,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

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

