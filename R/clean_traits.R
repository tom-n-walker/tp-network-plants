library(tidyverse)


### FUNCTION TO IMPORT TRAITS -------------------------------------------------

get_species <- function(){
  # load drake environment
  loadd()
  alldat <- mget(ls(pattern = "_"))
  # map all taxa to one unique vector
  taxa <- alldat %>% 
    map(~.$taxa) %>%
    do.call(c, .) %>%
    unique
  # remove missing cases
  taxa <- taxa[!is.na(taxa)]
  # return
  return(taxa)
}
