#### Code to merge all community data (with metadata) together ####

merge_comm_data <- function(x) {
  
  loadd()
  alldat = list(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
                CH_Lavey, CH_Calanda, #CH_Calanda2, 
                US_Colorado, US_Montana, US_Arizona,
                CN_Damxung, IN_Kashmir, CN_Gongga, 
                DE_Grainau, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia) 
  names(alldat) = c("NO_Ulvhaugen", "NO_Lavisdalen", "NO_Gudmedalen", "NO_Skjellingahaugen",
                    'CH_Lavey', 'CH_Calanda', #'CH_Calanda2', 
                    'US_Colorado', 'US_Montana', 'US_Arizona',
                    "CN_Damxung", 'IN_Kashmir', "CN_Gongga", 
                    'DE_Grainau', 'FR_AlpeHuez', 'SE_Abisko', 'FR_Lautaret', 'IT_MatschMazia')
  
  #fix up community dat
  dat <- alldat %>% 
    map(~.$community) %>% 
    bind_rows(.id='Region') %>%
    ungroup() %>%
    filter(!Treatment %in% c('NettedControl', 'Cold', 'Control')) #%>%
    #select(-notbad, -Gradient, -collector, -originPlotID, -Individuals, -Date)
  
  #add metadata to organize by elevations
  meta <- alldat %>% map(~ungroup(.$meta) %>% mutate(Gradient=as.character(Gradient))) %>%
    bind_rows(.id='Region') %>% 
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
