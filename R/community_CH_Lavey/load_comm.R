###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

#### STILL MAJOR ISSUE WITH MULTIPLE SAME SPECIES IN SAME SUBSHEET--> CHECK WITH LOIC!!!
load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector17 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
         .id = "turfID") %>% 
      select(-contains('Bare'), -contains("...")) %>% #check with loic, is this a real speacies. And are all NA species the same?
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum)
    return(dat)
  }
  
  collector18 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
        .id = "turfID") %>%
      select(-sp_2017) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID)), cover=rowSums(select_if(., is.numeric), na.rm=T)) %>%
      select(-starts_with('CRE')) %>%
      filter(!is.na(cover))
      
    return(dat)
  }
  
  #import data
  files17 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/'))
  files18 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2018/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2018/'))

  sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')
  cover17 <- tibble(siteID=files17) %>% 
    mutate(file_contents = map(siteID, collector17)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2017) %>%
    unnest() %>% 
    select(-contains('FOCAL'))
  
  cover18 <- tibble(siteID=files18) %>% 
    mutate(file_contents = map(siteID, collector18)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2018) %>%
    unnest() %>% 
    select(-contains('FOCAL'))

  return(bind_rows(cover17, cover18))
}
