###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x, 
                     col_types = 'numeric', .name_repair = make.names),
         .id = "turfID") %>% 
      mutate(turfID = paste0('T', turfID)) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum)
    return(dat)
  }
  
  #import data
  files <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/'))
  cover17 <- tibble(siteID = files) %>% 
    mutate(file_contents = map(siteID, collector))%>%
    mutate(siteID = gsub(pattern = one_of('turf.xlsx', 'turfV2.xlsx'), replacement='', siteID)) %>%
    unnest() %>% #change mutate call to just take one of various options (CRE_CRE, etc.) 
    select(-one_of('X','Remarque', 'Dead', 'NA.', 'Bare.ground'), -contains('FOCAL')) %>%
    mutate(Year=2017)

  return(cover)
}
