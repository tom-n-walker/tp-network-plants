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
  mutate(colonisation = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance"))) %>%
  mutate(extinction = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance"))) %>%
  mutate(turnover = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 

#### Plot colonisation patterns ####
colour_odt <- c("darkred", "darkblue", "darkorange")
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(colonisation, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
    ggplot(aes(x = Year, y = appearance, color = ODT)) + 
    geom_point() +
    scale_colour_manual(values = colour_odt) + 
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Region) +
    TP_theme()

#### Plot extinction patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(extinction, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = disappearance, color = ODT)) + 
  geom_point() +
  scale_colour_manual(values = colour_odt) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Region) +
  TP_theme()

#### Plot turnover patterns ####
dd %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = map2(turnover, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  unnest(dat) %>%
  ggplot(aes(x = Year, y = total, color = ODT)) + 
  geom_point() +
  scale_colour_manual(values = colour_odt) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Region) +
  TP_theme()


#ISSUES WITH MULTIPLE SPECIES: "DE_Grainau", "US_Montana", "IT_MatschMazia", "CN_Heibei", "FR_AlpeHuez"
#ISSUES WITH DESTPLOT ID: "CH_Calanda", "US_Colorado", "FR_Lautaret"
#Keep in mind, Colorado, Lautaret will have only one year of data

#### extra code (not sure where to go with intersect...) ####
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
