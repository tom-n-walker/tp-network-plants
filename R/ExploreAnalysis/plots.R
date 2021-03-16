### Plot results from Analyse_SR

#PLOT SR
PlotSR <- function(x) {

p1 <- x %>% group_by(Region) %>% filter(Elevation==min(Elevation) & Treatment=='Warm'|Elevation==max(Elevation) & Treatment=='LocalControl') %>%
  ggplot(aes(x=Treatment, y=SR)) + geom_boxplot() + facet_wrap(~Region) + theme_bw() + scale_x_discrete(labels=c("Alpine Control", "Transplanted Turf")) +
  theme(panel.grid = element_blank()) + ylab('Species Richness') + ggtitle('Species Richness of alpine turfs')

return(p1)

}

