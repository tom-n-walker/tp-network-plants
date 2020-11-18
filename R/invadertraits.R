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
  mutate(low = map(comm, ~{.} %>% filter(Year == min(Year)) %>%
                     select(ODT, SpeciesName) %>% 
                     filter(ODT == "destControls") %>%
                     distinct(.$SpeciesName)) %>% flatten(.),
         high = map(comm, ~{.} %>% filter(Year == min(Year)) %>%
                     select(ODT, SpeciesName) %>% 
                     filter(ODT == "warmed") %>%
                     distinct(.$SpeciesName)) %>% flatten(.),
         overlap = map(comm, ~{.} %>% filter(Year == max(Year)) %>%
                         select(ODT, SpeciesName) %>% 
                         filter(ODT %in% c("warmed", "destControls")) %>%
                         group_map(~Reduce(intersect, split(.$SpeciesName, .$ODT))) %>%
                         flatten_chr(.)))
                         
# Unnest and lengthen dataframe
inv_long <- invaders %>% pivot_longer(cols=low:overlap, names_to="Community", values_to="SpeciesName") %>%
  unnest(SpeciesName)

inv_long <- inv_long %>% inner_join(.,sp_codes, by=c('SpeciesName'='code')) %>%
  mutate(species = ifelse(!is.na(taxa), taxa, SpeciesName))


# Match with try trait data (add columns as nested dataframe?)
trait_g <-traits$species_only
trait_g[trait_g$original_name,]

traitdat <- left_join(inv_long, trait_g, by = c("species" = "original_name"))

# Create ordinations of traits
traitdat <- traitdat %>% select(Region, Community, species, leaf_area:SLA) 
traitdat_f <- traitdat %>% filter(!rowSums(is.na(.[,4:10]))==9)
trait<-scale(traitdat_f[,4:ncol(traitdat_f)]) #standardize trait data to mean = 0 and var = 1

trait.dist <- dist(trait)
traits.mds<-monoMDS(trait.dist, wa=FALSE, auto=FALSE)
rownames(traits.mds$points)<-traitdat_f$species
mds.fig <- ordiplot(traits.mds, type = "none")
points(mds.fig, "sites", pch = 19, col = "green", select = traitdat_f$Community == 
         "low")
points(mds.fig, "sites", pch = 19, col = "blue", select = traitdat_f$Community == 
         "overlap")
# add confidence ellipses around habitat types
ordiellipse(traits.mds, traitdat_f$Community, conf = 0.95, label = TRUE)



dd <- traitdat %>% 
  mutate(comm_wide = map(comm, ~{
    .x %>% select(Region, Community, species, leaf_area:stem_density) %>% 
      pivot_wider(names_from = species, values_from = Rel_Cover, values_fill = list(Rel_Cover = 1), values_fn = list(Rel_Cover = sum))
  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID, Year), 
         comm_spp = map(comm_wide, select, -ODT, -destPlotID, -Year)) %>% 
  select(-comm_wide) %>% 
  mutate(PCA = map(comm_spp, ~rda(sqrt(.x)))) %>% 
  mutate(scores = map(.x = PCA, .f = fortify, display = "wa", axes = 1:2)) %>% 
  mutate(scores = map2(.x = comm_meta, .y = scores, bind_cols))

# Map out distances from invaders to warmed communities and plot






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
