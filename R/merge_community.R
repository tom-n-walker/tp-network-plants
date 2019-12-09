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
  meta <- alldat %>% map(~ungroup(.$meta) %>% mutate(Gradient=as.character(Gradient))) %>%
    bind_rows(.id='Region') %>% 
    mutate(Gradient=recode(Gradient, '1'='NO_Ulvhaugen', '2'='NO_Lavisdalen', '3'='NO_Gudmedalen', '4'='NO_Skjellingahaugen')) %>%
    select(Region, destSiteID, Elevation) %>% 
    distinct()
  
  fulldat <- left_join(dat, meta, by=c('Region', 'destSiteID'))
  
  return(fulldat) 
  
}
