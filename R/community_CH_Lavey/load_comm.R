###### Lavey community data ####
#Chelsea Chisholm, 19.02.2018

setwd("./data/CH_Lavey/2017/cover")
#load_comm <- function(cover) {
  require("tidyverse")
  
  collector <- function (x) {
    cover <- excel_sheets(x) %>% 
      set_names() %>% 
      map_df(
        ~ read_xlsx(path = x, sheet = .x),
        .id = "turfID") %>% 
      group_by(turfID) %>% 
      summarise_if(is.numeric, sum)
    return(cover)
  }
 
  
  #import data
  path <- list.files('.')
  cover <- data_frame(siteID = path) %>% 
    mutate(file_contents = map(siteID, collector)) %>%
    mutate(siteID = gsub(pattern = 'turf.xlsx', replacement='', siteID)) %>%
    unnest() %>% select(-Dead, -contains('FOCAL'), -`NA`) %>%
    mutate(year=2017)
  
  #relabel sites and format column names
dat <- cover %>% separate(siteID, c('siteID', 'destsiteID'), sep='_')
cover.meta <- cover %>% select(siteID:turfID, year) #%>% 
    #     mutate(TTtreat = factor(TTtreat, levels = c("TTC","TT1", "TT2", "TT3", "TT4")))
cover <- dat %>% select(-(siteID:turfID), year)  
taxa <- colnames(cover)
#need to create treatment identifiers
  #and clean taxa list

    
#clear up
rm(dat, path, collector)
  

  # table(cover.meta$turfID, cover.meta$year)
  # table(cover.meta$year, cover.meta$siteID, cover.meta$TTtreat)
