#using Richard's function

# install.packages("devtools")
devtools::install_github("Between-the-Fjords/turfmapper", force=TRUE)

library(turfmapper)
library("tidyverse")

dat <- CH_Lavey$community

#set up subturf grid
grid <- make_grid(ncol = 1)

dat %>%
  filter(destPlotID=='CRE_CRE_T1') %>%
  mutate(subturf=1) %>%
  make_turf_plot(
    year = Year, species = SpeciesName, cover = Cover, subturf = subturf, 
    title = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}"), 
    grid_long = grid)

x <- dat %>% 
  # sort
  arrange(destSiteID, destPlotID) %>% 
  mutate(subturf = 1) %>%
  group_by(destSiteID, destPlotID) %>% 
  nest() %>% 
  {map2(
    .x = .$data, 
    .y = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}"),
    .f = ~make_turf_plot(
      data = .x, year = Year, species = SpeciesName, cover = Cover, subturf = subturf, title = .y, grid_long = grid)
  )} %>% 
  walk(print) #print all maps


pdf(file = "~/Desktop/lavey_checks.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches

# Step 2: Create the plot with R code
x

# Step 3: Run dev.off() to create the file!
dev.off()