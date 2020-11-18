#### INVADER TRAITS ####
invaders <- dd %>% select(Region, originSiteID, destSiteID, comm) %>%
  mutate(low = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #first year
                     select(ODT, SpeciesName) %>% 
                     filter(ODT == "destControls") %>%
                     distinct(.$SpeciesName) %>% flatten_chr(.)),
         high = map(comm, ~{.} %>% filter(Year == max(Year)) %>% #last year
                      select(ODT, SpeciesName) %>% 
                      filter(ODT == "warmed") %>%
                      distinct(.$SpeciesName) %>% flatten_chr(.)),
         overlap = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #first year
                         select(ODT, SpeciesName) %>% 
                         filter(ODT %in% c("warmed", "destControls")) %>%
                         group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                         flatten_chr(.)),
         high_unique = map2(high, low, ~setdiff(.x, intersect(.x,.y))),
         resident = map2(high_unique, overlap, ~c(.x,.y)),
         invader = map2(high, resident, ~setdiff(.x, intersect(.x,.y))))

# Unnest and lengthen dataframe
inv_long <- invaders %>% pivot_longer(invader, names_to="Community", values_to="SpeciesName") %>% 
  unnest(SpeciesName) %>%
  nest(SpeciesPool = c(Community, SpeciesName))  %>%
  select(-low, -high, -overlap, -high_unique, - resident)

inv <- inv_long %>% group_by(Region, originSiteID, destSiteID) %>% 
  mutate(comm2 = map2(comm, SpeciesPool, ~left_join(.x, .y, by = "SpeciesName"))) %>%
  mutate(comm_3 = map(comm2, ~{.} %>% filter(ODT=='warmed') %>%
                        mutate(invader = ifelse(is.na(Community), 'resident', 'invader')))) 

inv_spfixed <- inv %>%
  mutate(comm_4 = map(comm_3, ~left_join(.,sp_codes, by=c('SpeciesName'='code')))) %>%
  mutate(comm_5 = map(comm_4, ~ {.} %>% mutate(species = ifelse(!is.na(taxa), taxa, SpeciesName)) %>%
                        filter(!is.na(species), !is.na(SpeciesName)))) %>%
  mutate(comm_6 = map(comm_5, ~select(., ODT, destPlotID, SpeciesName, Rel_Cover, Community, taxa, species, invader)))

inv_fixed <- inv_spfixed %>% unnest(comm_6) 

# Match with try trait data
trait_g <-traits$species_only
traitdat <- left_join(inv_fixed, trait_g, by = c("species" = "original_name"))

# Scaling traits
traitdat <- traitdat %>% select(originSiteID, destSiteID, Region, ODT, destPlotID, invader, species, leaf_area:SLA) %>% tibble()
traitdat_f <- traitdat %>% filter(!rowSums(is.na(.[,8:14]))==7)

# Distances between invaders and low community (residents)
invord <- traitdat_f %>% group_by(originSiteID, destSiteID, Region, ODT, destPlotID) %>%
  nest() %>%
  mutate(meta = map(data, select, invader, species),
         trait = map(data, select, -invader, -species),
         trait_scaled = map(trait, ~scale(.x)),
         trait_dist = map(trait_scaled, ~dist(.x))) %>% 
  mutate(nmds = map(trait_dist, ~monoMDS(.x))) %>% 
  mutate(scores = map(nmds, ~scores(.x))) %>% 
  mutate(scores_all = map(scores, ~data.frame(MDS1=.x[,1], MDS2=.x[,2]))) %>%
  mutate(scores_all = map2(.x = meta, .y = scores_all, bind_cols)) 

invord_dist <- invord %>% 
  select(-(data:scores)) %>% 
  unnest(scores_all) %>%
  group_by(Region, originSiteID, destSiteID, ODT, destPlotID, invader) %>% 
  summarise_at(vars(matches("MDS")), .funs = mean) %>% 
  group_by(Region, originSiteID, destSiteID, ODT, destPlotID) %>% 
  nest() %>% 
  mutate(distances = map(data, ~c(dist(select(.x, matches("MDS"))))[1])) %>%  
  unnest(distances) %>%
  mutate(invaded = ifelse(is.na(distances), 'no', 'yes')) %>%
  select(-data)

invord_sp <- invord %>% 
  select(-(data:scores)) %>% 
  unnest(scores_all) %>%
  select(-MDS1, -MDS2)

invord_dist %>% filter(invaded == 'yes') %>%
  ggplot(aes(x = Region, y = distances)) + 
  TP_theme() +
  geom_boxplot() +
  labs(color = "Treatment Comparisons", y="Distance between centroids", x='Year') 

#### ODT ORDINATIONS ####

trait_odt <- dd %>% select(Region, originSiteID, destSiteID, comm) %>%
  mutate(SpeciesPool = map(comm, ~{.} %>% select(Year, ODT, destPlotID, SpeciesName) %>% 
                             filter(Year == max(Year)) %>%
                             select(-Year) %>%
                             group_by(ODT, destPlotID) %>%
                             distinct(SpeciesName)))

# Unnest and lengthen dataframe
inv_long <- trait_odt %>% unnest(SpeciesPool) 

inv<- inv_long %>% left_join(.,sp_codes, by=c('SpeciesName'='code')) %>% 
  mutate(species = ifelse(!is.na(taxa), taxa, SpeciesName)) %>%
  filter(!is.na(species), !is.na(SpeciesName)) %>%
  select(originSiteID, destSiteID, Region, ODT, destPlotID, SpeciesName, taxa, species)

# Match with try trait data
trait_g <-traits$species_only
traitdat <- left_join(inv, trait_g, by = c("species" = "original_name"))

# Ordination of all traits
traitdat <- traitdat %>% select(originSiteID, destSiteID, Region, ODT, destPlotID, species, leaf_area:SLA) %>% tibble()
traitdat_f <- traitdat %>% filter(!rowSums(is.na(.[,7:13]))==7)

# Distances between invaders and low community (residents)
traitord <- traitdat_f %>% group_by(Region, originSiteID, destSiteID) %>%
  nest() %>%
  mutate(meta = map(data, select, ODT, destPlotID, species),
         trait = map(data, select, -ODT, -destPlotID, -species),
         trait_scaled = map(trait, ~scale(.x)),
         trait_dist = map(trait_scaled, ~dist(.x))) %>% 
  mutate(nmds = map(trait_dist, ~monoMDS(.x))) %>% 
  mutate(scores = map(nmds, ~scores(.x))) %>% 
  mutate(scores_all = map(scores, ~data.frame(MDS1=.x[,1], MDS2=.x[,2]))) %>%
  mutate(scores_all = map2(.x = meta, .y = scores_all, bind_cols)) 

traitord_dist <- traitord %>% 
  select(-(data:scores)) %>% 
  unnest(scores_all) %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  group_by(Region, originSiteID, destSiteID, ODT, destPlotID, invader) %>% 
  summarise_at(vars(matches("MDS")), .funs = mean) #%>% 
  # group_by(Region, originSiteID, destSiteID, ODT, destPlotID, invader) %>% 
  # nest() %>%
  # mutate(distances = map(data, ~c(dist(select(.x, matches("MDS"))))[1])) %>%  
  # unnest(distances) %>%
  # mutate(invaded = ifelse(is.na(distances), 'no', 'yes'))


traitord_dist %>% #filter(invaded == 'yes') %>%
  ggplot(aes(x = MDS1, y = MDS2, col=invader)) + 
  TP_theme() +
  geom_point() +
  #facet_wrap(~Region) +
  labs(color = "Treatment Comparisons", y="Distance between centroids", x='Year')

#Individual plots
colour_odt <- c("#A92420", "#016367", "#FBC00E")
colour_odt <- c("#FBC00E")
#shape_odt <- c(16,16,25)

traitord_dist %>% filter(Region == "CH_Lavey") %>% #Insert desired region name
  #filter(ODT=='warmed') %>%
      ggplot(aes(x = MDS1, y = MDS2, colour  = ODT, scale_fill_manual(values = colour_otd), shape=invader, siz=1.5)) +
      geom_point() +
      scale_colour_manual(values = colour_odt) + 
      scale_fill_manual(values = colour_odt) +
      scale_linetype_manual(values=c(2,1)) +
      coord_equal() +
      stat_ellipse(aes(x=MDS1, y=MDS2,color=ODT, linetype=invader),type = "norm") +
      TP_theme() +
      labs(title = "CH_Lavey", color = "Treatment", shape = "Species") 

#### TRYING TO USE ADONIS TO COMPARE DIFFERENCES ####
trdat <- traitdat %>% filter(Region=='CH_Lavey') %>% 
  filter(ODT !='originControls')  %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  filter(!rowSums(is.na(.[,7:13]))==7)

tr <- dist(scale(trdat[,7:13]))
adonis(tr~invader, data=trdat)

#### TRYING TO USE ADONIS TO COMPARE DIFFERENCES ####
colour_odt <- c("#F4631E", "black")
#("#A92420", "#016367", "#FBC00E")
#SLA
traitdat %>% filter(Region=='CH_Lavey') %>% 
  filter(ODT !='originControls')  %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  filter(!rowSums(is.na(.[,7:13]))==7) %>%
  ggplot(aes(x = ODT, y = SLA, colour  = invader, scale_fill_manual(values = colour_otd))) +
  geom_boxplot() +
  scale_colour_manual(values = colour_odt) + 
  scale_fill_manual(values = colour_odt) +
  TP_theme() +
  labs(title = "CH_Lavey", color = "Treatment", shape = "Species") 

#Plant height
traitdat %>% filter(Region=='CH_Lavey') %>% 
  filter(ODT !='originControls')  %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  filter(!rowSums(is.na(.[,7:13]))==7) %>%
  ggplot(aes(x = ODT, y = plant_height, colour  = invader, scale_fill_manual(values = colour_otd))) +
  geom_boxplot() +
  scale_colour_manual(values = colour_odt) + 
  scale_fill_manual(values = colour_odt) +
  TP_theme() +
  labs(title = "CH_Lavey", color = "Treatment", shape = "Species") 

rar <- traitdat %>% filter(Region=='CH_Lavey') %>% 
  filter(ODT !='originControls')  %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  filter(!rowSums(is.na(.[,7:13]))==7) %>%
  mutate(treat = paste0(ODT, invader))

mt <- aov(rar$SLA ~ rar$treat)
emmeans(mt, pairwise~ treat)$contrasts
summary(mt)
mt <- aov(rar$plant_height ~ rar$treat)
emmeans(mt, pairwise~ treat)$contrasts
summary(mt)

#For all plots
traitdat %>% 
  filter(ODT !='originControls')  %>%
  left_join(., invord_sp, by=c("originSiteID", "destSiteID", "Region", "ODT", "destPlotID", "species")) %>%
  filter(Region %in% c("NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "CN_Damxung", "CN_Gongga", "US_Arizona", "DE_Susalps", "SE_Abisko")) %>%
  mutate(invader = ifelse(is.na(invader), 'resident', invader)) %>%
  filter(!rowSums(is.na(.[,7:13]))==7) %>%
  ggplot(aes(x = ODT, y = plant_height, colour  = invader, scale_fill_manual(values = colour_otd))) +
  geom_boxplot() +
  scale_colour_manual(values = colour_odt) + 
  scale_fill_manual(values = colour_odt) +
  TP_theme() +
  facet_wrap(~Region, nrow=2) +
  labs(title = "CH_Lavey", color = "Treatment", shape = "Species") 
                                                     