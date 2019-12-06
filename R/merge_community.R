#### Code to merge all community data (with metadata) together ####

merge_comm_data <- function(x) {
  
  loadd()
  alldat = list(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
                CH_Lavey, CH_Calanda, CH_Calanda2, 
                US_Colorado, US_Montana, US_Arizona,
                CN_Gongga, CN_Damxung, IN_Kashmir, 
                DE_Grainau, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia) 
  names(alldat) = c("NO_Ulvhaugen", "NO_Lavisdalen", "NO_Gudmedalen", "NO_Skjellingahaugen",
                    'CH_Lavey', 'CH_Calanda', 'CH_Calanda2', 
                    'US_Colorado', 'US_Montana', 'US_Arizona',
                    "CN_Gongga", "CN_Damxung", 'IN_Kashmir', 
                    'DE_Grainau', 'FR_AlpeHuez', 'SE_Abisko', 'FR_Lautaret', 'IT_MatschMazia')
  
  dat <- alldat %>% 
    map(~.$community) %>% 
    bind_rows(.id='Region') 
  
  #add metadata to organize elevations
  meta <- alldat %>% map(~mutate(.$meta, Gradient = as.character(Gradient))) %>%
    bind_rows(.id='Region') %>% 
    select(Region, destSiteID, Elevation) %>% 
    distinct()
  
  dat <- left_join(meta, SR)
  
  return(dat) 
  
}
