##### Simple Test Analysis ####
#C&D 14.12.2018

#summarize data and analyze Species richness ~treatment by Year and Site
AnalyzeSR <- function(x) {

lm_Site<-x$community %>% group_by(Year, destSiteID, Treatment, destPlotID) %>% 
  filter(Cover>0, Treatment %in% c('Warm', 'Control', 'LocalControl', destSiteID == 'L')) %>% 
  summarize(SR=n_distinct(species)) %>%
  group_by(Year) %>%
  do(lmSR = lm(SR~Treatment, data=.)) %>%
  tidy(lmSR)

return(lm_Site) }

           