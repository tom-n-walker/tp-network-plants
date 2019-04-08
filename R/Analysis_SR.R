##### Simple Test Analysis ####
#C&D 14.12.2018

#summarize data and analyze Species richness ~treatment by Year and Site
NMDS <- function(x) {
  
  x %>% metaMDS(.) 
}

library(vegan)
library(ggord) #use pack for github repos
AnalyzeSR <- function(p) {

  rar<- x$community %>% select(destSiteID, destPlotID, Treatment, Year, SpeciesName, Cover) %>% 
    filter(Year==max(Year)) %>%
    spread(SpeciesName, Cover) %>% replace_na(0)
    ggord(metaMDS(rar[,5:99]), rar$Treatment, poly = FALSE, polylntyp = rar$Treatment)

}
lm_Site<-x$community %>% group_by(Year, destSiteID, Treatment, destPlotID) %>% 
  filter(Cover>0) %>% 
  summarize(SR=n_distinct(SpeciesName)) %>%
  group_by(Year) %>%
  do(lmSR = lm(SR~Treatment, data=.)) %>%
  tidy(lmSR)

return(lm_Site) }

           