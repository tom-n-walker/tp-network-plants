library("codyn")

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
  filter(Region %in% c("NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "CN_Damxung", "CN_Gongga", "US_Arizona", "DE_Susalps", "SE_Abisko")) %>%
  mutate(specrich = map(comm, . %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
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
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2, method = "lm", se = FALSE) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~ Region) +
  TP_theme() + 
  labs(title = 'SR over time', color = "Treatment Comparisons") 

#### Plot colonisation patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(colonisation, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
    ggplot(aes(x = Year, y = appearance)) + 
    scale_colour_manual(values = colour_odt) + 
    geom_line(aes(group=destPlotID, color=ODT), alpha=0.2, method = "lm", se = FALSE) +
    geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
    facet_wrap(~ Region) +
    TP_theme() + 
  labs(title = 'Colonisation over time', color = "Treatment Comparisons") 

#### Plot extinction patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(extinction, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = disappearance)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2, method = "lm", se = FALSE) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~ Region) +
  TP_theme() + 
  labs(title = 'Extinction over time', color = "Treatment Comparisons") 

#### Plot turnover patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = total)) + 
  scale_colour_manual(values = colour_odt) + 
  geom_line(aes(group=destPlotID, color=ODT), alpha=0.2, method = "lm", se = FALSE) +
  geom_smooth(aes(group = ODT, color=ODT), method = "lm", se = F) +
  facet_wrap(~ Region) +
  TP_theme() + 
  labs(title = 'Turnover over time', color = "Treatment Comparisons") 

#### Plot C, E and T for only transplanted turfs ####
colour_comdyn <- c("darkblue", "darkred", "black")
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>% 
  filter(ODT == "warmed") %>% 
  ggplot(aes(x=Year, y=value, color=comdyn, group = interaction(destPlotID, comdyn))) + 
  scale_colour_manual(values = colour_comdyn) + 
  geom_line(alpha=0.2) +
  geom_smooth(aes(group=comdyn, color=comdyn), method='lm', alpha=0.2, se=F) +
  facet_wrap(~ Region) +
  TP_theme() + 
  labs(title = 'Community dynamics') 


#### Extract elevation and compare ####


#ISSUES WITH MULTIPLE SPECIES: "DE_Grainau", "US_Montana", "IT_MatschMazia", "CN_Heibei", "FR_AlpeHuez"
#ISSUES WITH DESTPLOT ID: "CH_Calanda", "US_Colorado", "FR_Lautaret"
#Keep in mind, Colorado, Lautaret will have only one year of data

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
