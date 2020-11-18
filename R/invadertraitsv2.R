#All years
invaders <- dd %>% select(Region, originSiteID, destSiteID, comm) %>%
    mutate(overlap = map(comm, ~{.} %>% select(Year, ODT, destPlotID, SpeciesName) %>% 
                        filter(ODT %in% c("warmed", "destControls")) %>%
                        group_by(Year) %>%
                        group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                        set_names(group_vars(.))
                        ))

dd$comm[[1]] %>% select(Year, ODT, destPlotID, SpeciesName) %>% 
                         filter(ODT %in% c("warmed", "destControls")) %>%
                         group_by(Year) %>%
                         group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                         set_names(unique(dd$comm[[1]]$Year))

#For final year
invaders <- dd %>% select(Region, originSiteID, destSiteID, comm) %>%
  mutate(low = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #first year
                     select(ODT, SpeciesName) %>% 
                     filter(ODT == "destControls") %>%
                     distinct(.$SpeciesName) %>% flatten_chr(.)),
         high = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #first year
                     select(ODT, SpeciesName) %>% 
                     filter(ODT == "warmed") %>%
                     distinct(.$SpeciesName) %>% flatten_chr(.)),
         overlap = map(comm, ~{.} %>% filter(Year == min(Year)) %>% #first year
                      select(ODT, SpeciesName) %>% 
                      filter(ODT %in% c("warmed", "destControls")) %>%
                      group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                      flatten_chr(.)),
         resident = map(comm, ~{.} %>% filter(Year == max(Year)) %>%
                         select(ODT, SpeciesName) %>% 
                         filter(ODT %in% c("warmed", "destControls")) %>%
                         group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                         flatten_chr(.)))

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

# testing setdiff        
# a <- c(1:5)
# b <- c(3:8)
# c <- c(2:8)
# overlap <- intersect(a,b) #overlap
# setdiff(union(a,b), intersect(a,b)) #everything but overlap
# high <- setdiff(b, intersect(a,b)) #unique to b 
# low <- setdiff(a, intersect(a,b)) #unique to a 
# setdiff(c, intersect(c, c(overlap, high)))

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

  # Community == NA & ODT == 'originControls' ~ 'resident',
  # Community == NA & ODT == 'destControls' ~ 'resident',
  # Community == 'invader' & ODT == 'originControls' ~ 'resident',
  # Community == 'invader' & ODT == 'destControls' ~ 'resident'))
  
  
inv_fixed <- inv_spfixed %>% unnest(comm_6) 


# Match with try trait data
trait_g <-traits$species_only

traitdat <- left_join(inv_fixed, trait_g, by = c("species" = "original_name"))

# Ordination of all traits
traitdat <- traitdat %>% select(originSiteID, destSiteID, Region, ODT, destPlotID, invader, species, leaf_area:SLA) %>% tibble()
traitdat_f <- traitdat %>% filter(!rowSums(is.na(.[,8:14]))==7)
trait<-scale(traitdat_f[,8:ncol(traitdat_f)]) #standardize trait data to mean = 0 and var = 1

trait.dist <- dist(trait)
traits.mds<-monoMDS(trait.dist, wa=FALSE, auto=FALSE)
rownames(traits.mds$points)<-traitdat_f$species
mds.fig <- ordiplot(traits.mds, type = "none")
points(mds.fig, "sites", pch = 19, col = "green", select = traitdat_f$invader == 
         "resident")
points(mds.fig, "sites", pch = 19, col = "blue", select = traitdat_f$invader == 
         "invader")
# add confidence ellipses around habitat types
ordiellipse(traits.mds, traitdat_f$Region, conf = 0.95, label = TRUE)

# Distances between invaders and low community (residents)
dd <- traitdat_f %>% group_by(originSiteID, destSiteID, Region, ODT, destPlotID) %>%
  nest() %>%
  mutate(meta = map(data, select, invader, species),
         trait = map(data, select, -invader, -species),
         trait_scaled = map(trait, ~scale(.x)),
         trait_dist = map(trait_scaled, ~dist(.x))) %>% 
  mutate(nmds = map(trait_dist, ~monoMDS(.x))) %>% 
  mutate(scores = map(nmds, ~scores(.x))) %>% 
  mutate(scores_all = map(scores, ~data.frame(MDS1=.x[,1], MDS2=.x[,2]))) %>%
  mutate(scores_all = map2(.x = meta, .y = scores_all, bind_cols)) 

dd1 <- dd %>% 
  select(-(data:scores)) %>% 
  unnest(scores_all) %>%
  group_by(Region, originSiteID, destSiteID, ODT, destPlotID, invader) %>% 
  summarise_at(vars(matches("MDS")), .funs = mean) %>% 
  group_by(Region, originSiteID, destSiteID, destPlotID) %>% 
  nest() %>% 
  mutate(distances = map(data, ~c(dist(select(.x, matches("MDS"))))[1])) %>%  
  unnest(distances) %>%
  mutate(invaded = ifelse(is.na(distances), 'no', 'yes'))


dd1 %>% filter(invaded == 'yes') %>%
  ggplot(aes(x = Region, y = distances)) + 
  TP_theme() +
  geom_boxplot() +
  labs(color = "Treatment Comparisons", y="Distance between centroids", x='Year') 


# Map out distances from invaders to warmed communities and plot
dd %>% 
  ggplot(aes(x = PC1, y = PC2, colour  = ODT, scale_fill_manual(values = colour_otd), group = destPlotID)) +
  geom_point(mapping = aes(size = Year == min(Year))) +
  scale_colour_manual(values = colour_odt) + 
  scale_fill_manual(values = colour_odt) +
  geom_path() +
  coord_equal() +
  TP_theme() +
  labs(title = "IT_MatschMazia", size = "First Year", color = "Treatment") 





#older attempts
# dd %>%
#   mutate(overlap = map(comm, ~. %>% select(Year, ODT, destPlotID, SpeciesName) %>%
#                          group_by(Year) %>%
#                          summarise(high = unique(originControls[!originControls%in%destControls]),
#                                    low = unique(destControls[!destControls%in%originControls]),
#                                    invaders = unique(warmed[low%in%warmed]))))
#                                    
# 
# dd$comm[[1]] %>% select(Year, ODT, destPlotID, SpeciesName) %>% 
#   group_by(Year) %>%
#   mutate(comm = pmap(.l = list(ODT=ODT, SPN = SpeciesName), .f = function(ODT, SPN){
#     bind_rows(
#       originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
#   summarise(high = unique(originControls[!originControls%in%destControls]),
#             low = unique(destControls[!destControls%in%originControls]),
#             invaders = unique(warmed[low%in%warmed]))
