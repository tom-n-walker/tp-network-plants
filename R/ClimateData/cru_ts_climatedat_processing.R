#### Read in climate data for all sites ####

install.packages('GSODR')
library(GSODR)

devtools::install_github("adamhsparks/GSODRdata")
library(GSOD)

nearest_stations(46.20299633094538, 7.062330594198718, 5)
#1 within 5 km for Lavey, 1 within 75 km for Heibei...

#not many stations, start with CRU data as more recent grids

# Create lat lon metadata

metadata <- read_excel(path = '~/Dropbox/projects/SNF Experiment/Transplant_Incline/data/overview/MetadataOverview_Gradient_Site.xlsx', 
                       sheet='Site level', skip = c(2))
metadat <- data.frame(gradient = metadata[,1], site = metadata[,2], lat =  metadata[,5], long = metadata[,6], elev=metadata[4], year1=metadata[7], yearn=metadata[8])
colnames(metadat) <- c('gradient', 'site', 'lat', 'long', 'elev', 'year1', 'yearn') 
metadat <- metadat %>% group_by(gradient,site) %>% summarize(lat=mean(as.numeric(lat)), long=mean(as.numeric(long)),
                                                             yearn=mean(yearn), year1=mean(year1), 
                                                             elev=elev, Yearrange=yearn-year1)
coords <- data.frame(x=metadat$long,y=metadat$lat)
points <- SpatialPoints(coords, proj4string = pre@crs)

# Read in CRU TS data

library(raster)
library(ncdf4)

setwd('./climate/CRU_TS/')

pre <- brick("./cru_ts4.04.1901.2019.pre.dat.nc", varname="pre") #Precipitation
tmp <- brick("./cru_ts4.04.1901.2019.tmp.dat.nc", varname="tmp") # Mean monthly temperature
pre.sites <- extract(pre, points)
tmp.sites <- extract(tmp, points)

# Change column names
years <- 1901:2019
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(pre.sites) <- c(paste(rep(years, each=12), rep(month, times=119), sep="_"))
colnames(tmp.sites) <- c(paste(rep(years, eachhe=12), rep(month, times=119), sep="_"))

# Join and create long-form data
prec <- cbind.data.frame(metadat, pre.sites)
rar <- prec[,c(1:2, 9:10)]
temp <- cbind.data.frame(metadat, tmp.sites)

prec <- prec %>% pivot_longer(cols=9:1436) %>% separate(name, sep='_', into = c("year", "month")) %>% mutate(year = as.numeric(year)) %>% filter(year>2000)
temp <- temp %>% pivot_longer(cols=9:1436)%>% separate(name, sep='_', into = c("year", "month")) %>% mutate(year = as.numeric(year)) %>% filter(year>2000)

# Read in elevation data
pre <- brick("halfdesg.elv", varname="elv") #Precipitation
#adiabatic lapse rate 9.8 deg C / km (dry)
