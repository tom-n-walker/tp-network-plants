###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

#### STILL MAJOR ISSUE WITH MULTIPLE SAME SPECIES IN SAME SUBSHEET--> Loic says it's just an input mistake, they are the same species.
load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector17 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x), #has to add name repair because adding unique names. Just combine them I think...
         .id = "turfID") #no idea why but needed to break pipeline here 
    dat1 <- dat %>% dplyr::select(-'NA', -contains('FOCAL')) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum) %>%
      gather(key='SpeciesName', value='cover', -turfID ) %>% 
      mutate(SpeciesName=str_replace(SpeciesName, "\\...+[\\d]$", "")) %>% #remove unique IDS and sum across groups
      group_by(turfID, SpeciesName) %>%
      summarise(cover=sum(cover)) %>%
      filter(!is.na(cover)) #remove non essential rows
    return(dat1)
  }
  
  
  
  collector18 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
        .id = "turfID") 
    dat1 <- dat %>% dplyr::select(-sp_2017) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID)), cover=rowSums(select_if(., is.numeric), na.rm=T)) %>%
      dplyr::select(-starts_with('CRE')) %>% #remove all rows and keep sums
      filter(!is.na(cover)) %>% #remove non essential rows
      filter(!grepl('FOCAL', sp_2018)) %>% #remove focals from counts
      mutate(SpeciesName = if_else(sp_2018=='NA', paste0('sp', turfID), sp_2018)) %>% #appears to only be one NA per turf, so this should be ok. Is it the same species everytime?
      select(turfID, SpeciesName, cover, -sp_2018) %>%
      group_by(turfID, SpeciesName) %>% #again make sure unique IDs are accounted for
      summarise(cover=sum(cover))
    return(dat1)
  }
  
  #import data
  files17 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/'))
  files18 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2018/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2018/'))

  sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')
  #Cover17 is site x species
  cover17 <- tibble(siteID=files17) %>% 
    mutate(file_contents = map(siteID, collector17)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2017) %>%
    unnest(cols = c(file_contents)) 
  
  #Cover18 is species x site
  cover18 <- tibble(siteID=files18) %>% 
    mutate(file_contents = map(siteID, collector18)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2018) %>%
    unnest(cols = c(file_contents)) %>%
    dplyr::select(siteID, year, turfID, SpeciesName, cover) #%>%
    #pivot_wider(names_from =SpeciesName, values_from=cover) 
  
  alldat <- bind_rows(cover17, cover18) 

  return(alldat)
}
