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
  mutate(comm_wide = map(comm, ~{
    .x %>% select(ODT, Year, SpeciesName, Rel_Cover, destPlotID) %>% 
      pivot_wider(names_from = SpeciesName, values_from = Rel_Cover, values_fill = list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))
  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID, Year), 
         comm_spp = map(comm_wide, select, -ODT, -destPlotID, -Year)) %>% 
  select(-comm_wide) %>% 
  mutate(PCA = map(comm_spp, ~rda(sqrt(.x)))) %>% 
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

#### PLOT PCA ####

library("patchwork")
colour_otd <- c("orange", "blue","green3")
 
  pmap(dd, function(scores, Region, originSiteID, ...){
    ggplot(scores, aes(x = PC1, y = PC2, colour  = ODT, group = destPlotID)) +
      geom_point() +
      scale_colour_manual(values = colour_otd) +
      geom_path() +
      coord_equal() +
      labs(title = paste(Region, originSiteID))
  }) %>%
    wrap_plots() +
  plot_layout(guides = 'collect')
  
## Individual region PCA
  
colour_otd <- c("orange", "blue","green3")

dd %>% filter(Region == "US_Arizona") %>% #Insert desired region name
  pmap(function(scores, Region, originSiteID, ...){
    ggplot(scores, aes(x = PC1, y = PC2, colour  = ODT, scale_fill_manual(values = colour_otd), group = destPlotID)) +
      geom_point() +
      scale_colour_manual(values = colour_otd) + 
      geom_path() +
      coord_equal() +
      labs(title = paste(Region, originSiteID))
  })



#remove rare species by region, max cover e.g. 1% or n occur ==1
#

