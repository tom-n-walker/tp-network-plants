#### Code to merge all community data (with metadata) together ####

merge_trait_data <- function(alldat) {
  
  
  #merge trait data from 7 sites
  dat <- alldat %>% 
    map_df("trait", .id='Region') %>%
    ungroup() 
  
  #sanity checks:
  #unique(dat$destSiteID) %in% unique(meta$destSiteID) #all true
  #unique(meta$destSiteID) %in% unique(dat$destSiteID) #all true
  # fulldat[is.na(fulldat$Rel_Cover),] #no NA Rel_covers (cover yes, arizona only has rel_cover)
  # dat[is.na(dat$Treatment),] #no NA treatments
  #fulldat %>% group_by(Region, destSiteID, Treatment) %>% summarize(n=n()) %>% View
  
  return(dat) 
  
}
