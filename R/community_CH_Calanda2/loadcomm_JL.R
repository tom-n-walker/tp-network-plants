###### Calanda2 community data functions ####
#These are all taken from Jacob's ladder
#Chelsea Chisholm, 03.11.2019

load_cover_CH_Calanda <- function(){
  #import data
    dat <- read.csv('./data/CH_JacobsLadder/CH_JacobsLadder_commdata/covmat.csv', stringsAsFactors = FALSE)
  cover <- dat %>% 
    gather(plot, cover, -Species) %>%
    mutate(Site= gsub('\\..*', '', plot), turfID= paste0('T', gsub('[^0-9]', '', plot)), year= 2017, SpeciesName=Species) %>%
    select(Site, year, turfID, SpeciesName, cover, -Species, -plot)
  
  return(cover)
}
