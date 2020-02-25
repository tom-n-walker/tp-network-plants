dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = map2(.x = originSiteID, .y = destSiteID, .f = ~{
        bind_rows(
          originControls = dat %>% filter(destSiteID == .x, Treatment == "LocalControl"),
          destControls = dat %>% filter(destSiteID == .y, Treatment == "LocalControl"),
          warmed =  dat %>% filter(destSiteID == .y, Treatment == "Warm"),
          .id = "ODT")
      })) %>% 
  mutate(comm_wide = map(comm, ~{
    .x %>% select(ODT, Year, SpeciesName, Rel_Cover, turfID) %>% 
      pivot_wider(names_from = SpeciesName, values_from = Rel_Cover, values_fill = list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))
  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, turfID, Year), 
         comm_spp = map(comm_wide, select, -ODT, -turfID, -Year)) %>% 
  select(-comm_wide) %>% 
  mutate(PCA = map(comm_spp, rda)) %>% 
  mutate(scores = map(.x = PCA, .f = fortify, display = "wa", axes = 1:2)) %>% 
  mutate(scores = map2(.x = comm_meta, .y = scores, bind_cols))
  
dd2 <- dd %>% 
  select(-(comm:PCA)) %>% 
  unnest(scores) %>% 
  group_by(Region, originSiteID, destSiteID, ODT, Year) %>% 
  summarise_at(vars(matches("PC")), .funs = mean) %>% 
  group_by(Region, originSiteID, destSiteID, Year) %>% 
  nest() %>% 
  mutate(distances = map(data, ~dist(select(.x, matches("PC"))))) %>% 
  mutate(distances = map(distances, ~tibble(what = c("Controls", "dest_TP", "orig_TP"), dist = as.vector(.x)))) %>% 
  unnest(distances)

#remove rare species by region, max cover e.g. 1% or n occur ==1
#originSiteID and destSiteID are not distinct across regions! FIX

