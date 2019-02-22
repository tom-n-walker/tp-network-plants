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
        .id = "plotID") %>% 
      group_by(plotID) %>% 
      summarise_if(is.numeric, sum)
    return(cover)
  }
 
  path <- list.files('.')
  cover <- data_frame(siteID = path) %>% 
    mutate(file_contents = map(siteID, collector)) #%>%
    mutate(siteID = gsub(pattern = 'turf.xlsx', replacement='', filename)) %>%
    unnest() %>% select(-filename, -Dead, -contains('FOCAL'), -`NA`) #%>%
    
  cover %>% arrange() 
    
  

  
  
  #to do: remove Feuile sheet, create site ID to destination ID (and I guess metadata)
  #and then clean.
  
  
  
#   
#   ##extra stuff from gonga/norway
#   # make fat table
#   cover <- cover.thin %>% spread(key = species, value = cover)
#   
#   #make meta data
#   cover.meta <- cover %>% select(siteID:destSiteID) %>% 
#     mutate(TTtreat = factor(TTtreat, levels = c("TTC","TT1", "TT2", "TT3", "TT4")))
#   
#   
#   turfs <- cover.meta[!duplicated(cover.meta$turfID),]
#   
#   cover <- cover %>% select(-(siteID:destSiteID))
#   
#   #clear up
#   rm(siri.fix, siriLOW, siri)
#   
#   table(cover.meta$turfID, cover.meta$year)   
#   table(cover.meta$year, cover.meta$siteID, cover.meta$TTtreat)         
#   
#   alltaxa <- TRUE
#   propertaxa <- !names(cover) %in% c("NID.seedling", "Car.sp", "Hie.sp", "Luz.sp",  "NID.gram", "NID.herb", "NID.rosett", "Pyr.sp")
#   noNIDseedlings <- !names(cover) %in% c("NID.seedling")
#   
#   turfs$newTT <- turfs$TTtreat  #alternative TTtreat with combined controls
#   levels(turfs$newTT)[1:2] <- "control"
#   
#   # save(cover, cover.thin, cover.meta, turfs, file = "cover.Rdata")
#   return(cover.thin)
# }
# )
#   