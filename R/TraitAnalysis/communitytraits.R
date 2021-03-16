#### COMMUNITY TRAITS ####
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

# species <- dd %>% 
#   mutate(species = map(comm, ~{.} %>% 
#                          select(Year, ODT, SpeciesName) %>% 
#                          group_by(Year, ODT) %>%
#                           distinct(.$SpeciesName) %>% 
#                           flatten_chr(.)))

# Unnest and lengthen dataframe
speclist <- dd %>% select(comm) %>% unnest(comm) %>% left_join(., sp_codes, by=c('SpeciesName'='code')) %>%
  mutate(species = ifelse(!is.na(taxa), taxa, SpeciesName)) %>%
  filter(!is.na(species), !is.na(SpeciesName)) %>%
  select(Region, Year, originSiteID, destSiteID, ODT, destPlotID, SpeciesName, Rel_Cover, taxa, species)


# Match with try trait data
trait_g <-traits$species_only
traitdat <- left_join(speclist, trait_g, by = c("species" = "original_name"))

# Scaling traits
traitdat <- traitdat %>% select(Region, Year, originSiteID, destSiteID, ODT, destPlotID, species, Rel_Cover, leaf_area:SLA) %>% tibble()
traitdat_f <- traitdat %>% filter(!rowSums(is.na(.[,9:15]))==7)

# Calculate CWMs
CWM <- traitdat_f %>% group_by(Region, Year, destSiteID, ODT, destPlotID) %>%
  summarise(across(.cols= leaf_area:SLA, .fns = list(CWM = ~weighted.mean({.}, Rel_Cover)))) %>%
  pivot_longer(leaf_area_CWM:SLA_CWM, names_to="CWM") 

# Plot change in CWMs over time
colour_odt <- c("#A92420", "#016367", "#FBC00E")

CWM %>%
  group_by(Region) %>% mutate(Year_diff=max(Year - min(Year))) %>%
  ungroup() %>%
  filter(Year_diff > 0, !is.na(value)) %>%
  group_by(Region, destSiteID, Year_diff, destPlotID, ODT, CWM) %>%
  nest() %>% 
  mutate(model = map(data, ~lm(log(value) ~ Year, data = .x))) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  unnest(tidied) %>%
  filter(term == 'Year', CWM != "leaf_C_CWM") %>% #ODT == "warmed"
  ggplot(aes(x=abs(estimate), fill=ODT)) + 
  geom_density(adjust = 3, alpha=0.2) + 
  scale_fill_manual(values = colour_odt) + 
  xlim(-0.5, 0.5) +
  TP_theme() + 
  labs(x=expression(Delta*Slope)) +
  labs(color="Treatment", y="Number of communities") +
  facet_wrap(~CWM) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1) 

# Plot CWm of last year per treatment
colour_odt <- c("#016367", "#FBC00E", "#A92420")

CWM %>%
  group_by(Region) %>% mutate(Year=max(Year)) %>%
  ungroup() %>%
  mutate(ODT = factor(ODT, levels=c("originControls","warmed", "destControls"))) %>%
  filter(CWM=="SLA_CWM", !is.na(value)) %>%
  ggplot(aes(y=log(value), x=ODT, fill=ODT)) + 
  geom_boxplot(alpha=0.6) + 
  scale_fill_manual(values = colour_odt) + 
  TP_theme() + 
  labs(x="Treatment") +
  labs(color="Treatment", y="log(SLA)") +
  facet_wrap(~Region) 

CWM %>%
  group_by(Region) %>% mutate(Year=max(Year)) %>%
  ungroup() %>%
  mutate(ODT = factor(ODT, levels=c("originControls","warmed", "destControls"))) %>%
  filter(CWM=="plant_height_CWM", !is.na(value)) %>%
  ggplot(aes(y=log(value), x=ODT, fill=ODT)) + 
  geom_boxplot(alpha=0.6) + 
  scale_fill_manual(values = colour_odt) + 
  TP_theme() + 
  labs(x="Treatment") +
  labs(color="Treatment", y="log(Plant Height)") +
  facet_wrap(~Region) 


