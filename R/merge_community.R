#### Code to merge all community data (with metadata) together ####

merge_comm_data <- function(alldat) {
  
  
  #fix up community dat
  dat <- alldat %>% 
    map_df("community", .id='Region') %>%
    ungroup() %>%
    filter(!Treatment %in% c('NettedControl', 'Cold', 'Control')) #%>%
    #select(-notbad, -Gradient, -collector, -Collector, -originPlotID, -Individuals, -Date) #This was for norway
  
  #add metadata to organize by elevations
  meta <- alldat %>% 
    map("meta") %>% 
    map(ungroup) %>% 
    map_df(mutate, Gradient = as.character(Gradient), .id='Region') %>% 
    mutate(Gradient=recode(Gradient, '1'='NO_Ulvhaugen', '2'='NO_Lavisdalen', '3'='NO_Gudmedalen', '4'='NO_Skjellingahaugen')) %>%
    select(Region, destSiteID, Elevation) %>% 
    distinct()
  
  #bind
  fulldat <- left_join(dat, meta, by=c('Region', 'destSiteID'))
  
  #sanity checks:
  #unique(dat$destSiteID) %in% unique(meta$destSiteID) #all true
  #unique(meta$destSiteID) %in% unique(dat$destSiteID) #all true
  # fulldat[is.na(fulldat$Rel_Cover),] #no NA Rel_covers (cover yes, arizona only has rel_cover)
  # dat[is.na(dat$Treatment),] #no NA treatments
  #fulldat %>% group_by(Region, destSiteID, Treatment) %>% summarize(n=n()) %>% View
  
  return(fulldat) 
  
}
