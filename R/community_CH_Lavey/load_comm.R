###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector17 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x, .name_repair = make.names),
         .id = "turfID") %>% 
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum)
    return(dat)
  }
  
  collector18 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x, .name_repair = make.names),
        .id = "turfID") %>%
      select(-sp_2017) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      gather(sp_2018, 'cover')
      group_by(turfID, sp_2018) %>%
      summarise_all(funs(sum)) 
      
    return(dat)
  }
  
  #import data
  files17 <- data.frame(siteID=paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/')))
  files18 <- data.frame(siteID=paste0('./data/CH_Lavey/CH_Lavey_commdata/2018/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2018/')))

  sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')
  cover <-   bind_rows(list('2017' = files17, '2018' = files18), .id='year') %>% 
    mutate(file_contents = map(siteID, collector)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=is.numeric(year)) %>%
    unnest() %>% 
    select(-one_of('X','Remarque', 'Dead', 'NA.', 'Bare.ground'), -contains('FOCAL'))

  return(cover)
}
