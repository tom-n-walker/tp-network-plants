###### IT community data functions ####
#Chelsea Chisholm, 3.11.2019

load_cover_IT_MatschMazia <- function(){
  #import data
  low <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13.csv.xlsx", sheet=1)
  colnames(low)[3] <- 'Treatment'
  high <- read_excel("data/IT_MatschMazia/IT_MatschMazia_commdata/VegData10-13.csv.xlsx", sheet=2)
  colnames(high)[2] <- 'Treatment'
  
  low <- low %>% mutate(destSiteID='Low', destPlotID=`internal ID`) %>% select(-Bezeichnung, -`Gradient Name`, -`internal ID`) 
  high <- high %>% mutate(destSiteID='High', destPlotID=`internal ID`) %>% select(-`Gradient Name`, -`internal ID`) 
  
  cover <- bind_rows(low, high)
  
  return(cover)
}

# Check if this is correct.. DestSiteID are named confusingly here, doesn't translate correclty to what they are doing in the treatments. 