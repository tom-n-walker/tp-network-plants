#### MAKE A CLIMATE FIGURE ####
#10.04.2019
library(tidyverse)
#devtools::install_github('MirzaCengic/climatedata')
#devtools::install_github("jimhester/archive")
library(climatedata)
library(archive)

metadata <- read_excel(path = '~/Dropbox/projects/SNF Experiment/Transplant_Incline/data/overview/MetadataOverview_Gradient_Site.xlsx', 
                       sheet='Site level', skip = c(2))
metadat <- data.frame(gradient = metadata[,1], site = metadata[,2], lat =  metadata[,5], long = metadata[,6], elev=metadata[4], year1=metadata[7], yearn=metadata[8])
colnames(metadat) <- c('gradient', 'site', 'lat', 'long', 'elev', 'year1', 'yearn') 
metadat <- metadat %>% group_by(Country, gradient) %>% summarize(nSites=n(), lat=mean(as.numeric(lat)), long=mean(as.numeric(long)),
                                                        yearn=mean(yearn), year1=mean(year1), 
                                                        Elevrange=(max(elev)-min(elev)), Yearrange=yearn-year1)



#chelsa_bioclim <- get_chelsa(output_dir = "./climate", period = "current")

library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=10)

r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")

lats <- metadat$lat
lons <- metadat$long

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)
df <-df %>% mutate(Temp=Temp/10, Prec=Prec/10)
df <- cbind(df, metadat)
df <- df %>% mutate(Continent=case_when(gradient %in% c('CH_Calanda', 'CH_Lavey', 'DE_Grainau',
                                                   'FR_AlpeHuez', 'FR_Lautaret','IT_MatschMazia') ~ 'Alps',
                               gradient %in% c('NO_Gudmedalen', 'NO_Lavisdalen', 'NO_Skjellingahaugen',
                                                'NO_Ulvhaugen', 'SE_Abisko') ~'Scandinavia',
                               gradient %in% c('US_Arizona', 'US_Colorado', 'US_Montana') ~'USA',
                               gradient %in% c('IN_Kashmir', 'CN_Heibei', 'CN_Gongga', 'CN_Damxung') ~'Asia'))


devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)

plot1 <- whittaker_base_plot() +
  geom_point(data = df,
             aes(x = Temp,
                 y = Prec),
             shape  = 21,
             stroke = 1, # acts as the thickness of the boundary line
             colour = "gray95", # acts as the color of the boundary line
             size   = 3.5) +
  geom_point(data = df,
             aes(x = Temp,
                 y = Prec),
             shape = 16,
             size  = 3,
             alpha = 0.5) 

my_plot <- plot1 +
  # Optional - Overwrite axis ranges (the scale warning is expected):
  # - set range on OY axes and adjust the distance (gap) from OX axes
  scale_y_continuous(name = 'Precipitation (cm)',
                     limits = c(min = -5, max = ceiling(max(460, df$Prec)/10)*10) ,
                     expand = c(0, 0)) +
  # - set range on OX axes and adjust the distance (gap) from OY axes
  scale_x_continuous(name = expression("Temperature " ( degree*C)),
                     limits = c(min = floor(min(-20, df$Temp)/5)*5, max = 30.5),
                     expand = c(0, 0)) +
  coord_fixed(ratio = 1/10) + # aspect ratio, expressed as y / x
  theme_bw() +
  theme(
    legend.justification = c(0, 1), # pick the upper left corner of the legend box and
    legend.position = c(0, 1), # adjust the position of the corner as relative to axis
    legend.background = element_rect(fill = NA), # transparent legend background
    legend.box = "horizontal", # horizontal arrangement of multiple legends
    legend.spacing.x = unit(0.5, units = "cm"), # horizontal spacing between legends
    panel.grid = element_blank() # eliminate grids
  )

my_plot

color_palette <- c("#C1E1DD", "#D16E3F", "#97B669",  "#FCD57A")
ggplot() +
  geom_point(data = df,
             aes(x = Temp,
                 y = Prec, 
                 fill=Continent),
             shape  = 21,
             size   = 3.5) +
  theme_bw() + 
  xlab(expression("Temperature " ( degree*C))) + 
  ylab('Precipitation (cm)') +
  scale_fill_manual(values = color_palette) +
  theme(
    legend.justification = c(0, 1), # pick the upper left corner of the legend box and
    legend.position = c(0, 1), # adjust the position of the corner as relative to axis
    legend.background = element_rect(fill = NA), # transparent legend background
    legend.box = "horizontal", # horizontal arrangement of multiple legends
    legend.spacing.x = unit(0.5, units = "cm"), # horizontal spacing between legends
    panel.grid = element_blank() # eliminate grids
  )
