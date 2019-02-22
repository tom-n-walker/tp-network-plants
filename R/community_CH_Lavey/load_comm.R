###### Lavey community data ####
#Chelsea Chisholm, 19.02.2018

load_comm <- function(cover) {
  require("tidyverse")
  require("readxl")
  
path <- "PRA_PRAturf.xlsx"
cover_PRA <- lapply(excel_sheets(path), read_excel, path = path)
cover_PRA <- cover_PRA[-1]
lapply(cover_PRA, sumcov)
sumcov(cover_PRA)
sumcov <- function(x) {
  x %>% select(-1) %>% 
    colSums() 
}
cover_PRA %>% 
cover_PRA <- path %>% 
    excel_sheets() %>%
    filter(!1)
    set_names() %>% 
    map(read_excel, path = path) 

rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}
all.data <- Reduce(rbind.all.columns, cover_PRA )

#to do: remove Feuile sheet, merge (not bind_rows, cols not identical across sheets)
#check that not duplicating columns (likely...)
#now iterate this over all files, make a column for each file name
#and then clean.




##extra stuff from gonga/norway
# make fat table
cover <- cover.thin %>% spread(key = species, value = cover)

#make meta data
cover.meta <- cover %>% select(siteID:destSiteID) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("TTC","TT1", "TT2", "TT3", "TT4")))


turfs <- cover.meta[!duplicated(cover.meta$turfID),]

cover <- cover %>% select(-(siteID:destSiteID))

#clear up
rm(siri.fix, siriLOW, siri)

table(cover.meta$turfID, cover.meta$year)   
table(cover.meta$year, cover.meta$siteID, cover.meta$TTtreat)         

alltaxa <- TRUE
propertaxa <- !names(cover) %in% c("NID.seedling", "Car.sp", "Hie.sp", "Luz.sp",  "NID.gram", "NID.herb", "NID.rosett", "Pyr.sp")
noNIDseedlings <- !names(cover) %in% c("NID.seedling")

turfs$newTT <- turfs$TTtreat  #alternative TTtreat with combined controls
levels(turfs$newTT)[1:2] <- "control"

# save(cover, cover.thin, cover.meta, turfs, file = "cover.Rdata")
return(cover.thin)
}