###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

load_cover_CH_Lavey <- function(){
  
  collector <- function (x) {
    dat <- excel_sheets(paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/',x)) %>% 
      set_names() %>% 
      map_df(
        ~ read_xlsx(path = paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/',x), sheet = .x),
         .id = "turfID") #%>% 
      # group_by(turfID) %>% 
      # summarise_if(is.numeric, sum)
    return(dat)
  }
  
  #import data
  files <- list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/')
  cover <- tibble(siteID = files) %>% 
    mutate(file_contents = map(siteID, collector)) #%>%
    mutate(siteID = gsub(pattern = 'turf.xlsx', replacement='', siteID)) %>%
    unnest() %>% select(-contains('FOCAL'), -`NA`) %>% #-Dead?
    mutate(year=2017)

  return(cover)
}
