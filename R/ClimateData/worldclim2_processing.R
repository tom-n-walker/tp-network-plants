#### Read in worldclim data (downloaded to /climate/Worldclim2)
library(rgdal)
library(raster)
library(sp)

# Read in lat lon metadata for each site
metadata <- read_excel(path = '~/Dropbox/projects/SNF Experiment/Transplant_Incline/data/overview/MetadataOverview_Gradient_Site.xlsx', 
                       sheet='Site level', skip = c(2))
metadat <- data.frame(gradient = metadata[,1], site = metadata[,2], lat =  metadata[,5], long = metadata[,6], elev=metadata[4], year1=metadata[7], yearn=metadata[8])
colnames(metadat) <- c('gradient', 'site', 'lat', 'long', 'elev', 'year1', 'yearn') 
metadat2 <- metadat %>% group_by(gradient,site, lat, long) %>% mutate(yearrange=yearn-year1)
coords <- data.frame(x=as.numeric(metadat2$long),y=as.numeric(metadat2$lat))
points <- SpatialPoints(coords, proj4string = CRS("+proj=longlat"))

#### Get elevation data ####
#  Generate a stack object with elevation used for worldclim2
E_filenames<- "./climate/Worldclim2/wc2.1_30s_elev.tif"
elev<- stack(E_filenames)
values <- extract(elev, points)
colnames(values) <- "wc2.1_30s_elev"

## Get temperature data 
T_filenames<- paste("./climate/Worldclim2/wc2.1_30s_tavg/wc2.1_30s_tavg_", c(paste(0,1:9, sep=""), 10,11,12),
                    ".tif",sep="")
temp30<- stack(T_filenames)
temps <- extract(temp30, coords)

## Get vapor pressure deficit data 
V_filenames<- paste("./climate/Worldclim2/wc2.1_30s_vapr/wc2.1_30s_vapr_", c(paste(0,1:9, sep=""), 10,11,12),
                    ".tif",sep="")
vapr30<- stack(V_filenames)
vapr <- extract(vpr30, coords)

df <- cbind.data.frame(coordinates(points), values, temps, vapr)
colnames(df)[1:2] <- c('x','y')
dat <- cbind.data.frame(metadat2, df)

# Calculate lapse rate for each site for temp and include average vapr (how to make this cumulative? does that even make sense?)
dat2 <- dat %>% rowwise() %>%
  mutate(T_ann = mean(c(wc2.1_30s_tavg_01:wc2.1_30s_tavg_12))) %>%
  mutate(T_sum = mean(c(wc2.1_30s_tavg_05:wc2.1_30s_tavg_09))) %>%
  mutate(T_win = mean(c(wc2.1_30s_tavg_10, wc2.1_30s_tavg_11, wc2.1_30s_tavg_12, wc2.1_30s_tavg_01, wc2.1_30s_tavg_02, wc2.1_30s_tavg_03))) %>%
  mutate(T_ann_cor = T_ann+(0.0098*(wc2.1_30s_elev-elev)),
         T_sum_cor = T_sum+(0.0098*(wc2.1_30s_elev-elev)),
         T_win_cor = T_win+(0.0098*(wc2.1_30s_elev-elev))) %>%
  mutate(V_ann = mean(c(wc2.1_30s_vapr_01:wc2.1_30s_vapr_12))) %>%
  mutate(V_sum = mean(c(wc2.1_30s_vapr_04:wc2.1_30s_vapr_09))) %>%
  mutate(V_win = mean(c(wc2.1_30s_vapr_10, wc2.1_30s_vapr_11, wc2.1_30s_vapr_12, wc2.1_30s_vapr_01, wc2.1_30s_vapr_02, wc2.1_30s_vapr_03))) %>%
  dplyr::select(gradient, site, lat, lon=long, yearn, year1, year_range = yearrange, elev, T_ann, T_sum, T_win, T_ann_cor, T_sum_cor, T_win_cor, V_ann, V_sum, V_win)

dat2$site <- as.character(dat2$site)
dat2$site[5:7] <- c('Nes', 'Cal', 'Pea') # fixing Calanda
dat2$site[10:11] <- c('G', 'L') #fixing FR_Laut
dat2$site[12:13] <- c('High', 'Low') #fixing IT_Matsch <- NEED TO ADD SECOND ITALIAN SITE!!!
dat2$site[16:27] <- c("Ulvhaugen", "Fauske", "Alrust", #fixing Norway. gradients are in order, check sites
                      "Lavisdalen", "Vikesland", "Hogsete",
                      "Gudmedalen", "Arhelleren", "Rambera", 
                      "Skjellingahaugen", "Ovstedal", "Veskre")

dat2$site[30:33] <- c('3200', '3400', '3600', '3800') #fixing Heibei <- this mysterious mid2 at 3600... what to do about that
dat2$site[43:44]  <- c("MC", "PP")
dat2$site[48:50]  <- c("High", "Mid", "Low")

# for DE_Susalps, totally confused as to which is Bavaria and which is DE. Right now sites are EB, GW and FE... wtf

write.csv(dat2, './climate/worlclim2_processedclimate.csv')
