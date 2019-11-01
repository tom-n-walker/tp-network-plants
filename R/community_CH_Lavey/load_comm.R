###### Lavey community data functions ####
#Chelsea Chisholm, 19.02.2019

#### STILL MAJOR ISSUE WITH MULTIPLE SAME SPECIES IN SAME SUBSHEET--> CHECK WITH LOIC!!!
load_cover_CH_Lavey <- function(){
  
#Function to load sheets and sum abundance across pin drop points per turf
  collector17 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x), #has to add name repair because adding unique names. Just combine them I think...
         .id = "turfID") #no idea why but needed to break pipeline here 
    dat1 <- dat %>% dplyr::select(-'NA', -contains('Bare'), -contains('FOCAL'), -Dead) %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum) %>%
      gather(key='SpeciesName', value='cover', -turfID ) %>% 
      mutate(SpeciesName=str_replace(SpeciesName, "\\...+[\\d]$", "")) %>% #remove unique IDS and sum across groups
      group_by(turfID, SpeciesName) %>%
      summarise(cover=sum(cover))
    return(dat1)
  }
  
  
  
  collector18 <- function (y) {
    dat <- excel_sheets(y) %>%
      str_subset("^\\d|^\\d.") %>%
      map_df(
        ~ read_excel(path = y, sheet = .x),
        .id = "turfID") 
    dat1 <- dat %>% dplyr::select(-'sp2017') %>%
      mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID))) %>%
      group_by(turfID) %>%
      summarise_if(is.numeric, sum) %>%
      gather(key='SpeciesName', value='cover', -turfID ) %>% 
      mutate(SpeciesName=str_replace(SpeciesName, "\\...+[\\d]$", "")) %>% #remove unique IDS and sum across groups
      group_by(turfID, SpeciesName) %>%
      summarise(cover=sum(cover))
    #This is the older text, need to harmonize these! I think the 2018 data is in a different format (transposed)  
    # select(-sp_2017) %>%
      # mutate(turfID = paste0('T', gsub("[^0-9.-]", "", turfID)), cover=rowSums(select_if(., is.numeric), na.rm=T)) %>%
      # select(-starts_with('CRE')) %>%
      # filter(!is.na(cover))
      
    return(dat)
  }
  
  #import data
  files17 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2017/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2017/'))
  files18 <- paste0('./data/CH_Lavey/CH_Lavey_commdata/2018/', list.files('./data/CH_Lavey/CH_Lavey_commdata/2018/'))

  sitenames <-c('CRE_CRE','CRE_RIO','MAR_MAR','MAR_RIO','PRA_PRA','PRA_RIO','RIO_RIO')
  #Cover17 is site x species
  cover17 <- tibble(siteID=files17) %>% 
    mutate(file_contents = map(siteID, collector17)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2017) %>%
    unnest(cols = c(file_contents)) %>% 
    select(-contains('FOCAL'), -'NA')
  
  #Cover18 is species x site
  cover18 <- tibble(siteID=files18) %>% 
    mutate(file_contents = map(siteID, collector18)) %>%
    mutate(siteID = str_extract(siteID, paste(sitenames, collapse="|")), year=2018) %>%
    unnest(cols = c(file_contents)) %>%
    dplyr::select(siteID, year, turfID, sp_2018, cover) %>%
    spread(key=sp_2018, cover, -siteID, -year, -turfID) %>%
    filter(!is.na(sp_2018), -contains('FOCAL', sp_2018)) %>%
    #filter(!sp2018 %in% grepl('FOCAL', sp2018))

  return(bind_rows(cover17, cover18))
}
