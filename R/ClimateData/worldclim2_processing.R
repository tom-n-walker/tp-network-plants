
library(rgdal)
library(raster)
library(sp)

# Read in lat lon metadata
metadata <- read_excel(path = '~/Dropbox/projects/SNF Experiment/Transplant_Incline/data/overview/MetadataOverview_Gradient_Site.xlsx', 
                       sheet='Site level', skip = c(2))
metadat <- data.frame(gradient = metadata[,1], site = metadata[,2], lat =  metadata[,5], long = metadata[,6], elev=metadata[4], year1=metadata[7], yearn=metadata[8])
colnames(metadat) <- c('gradient', 'site', 'lat', 'long', 'elev', 'year1', 'yearn') 
metadat <- metadat %>% group_by(gradient,site) %>% summarize(lat=mean(as.numeric(lat)), long=mean(as.numeric(long)),
                                                             yearn=mean(yearn), year1=mean(year1), 
                                                             elev=elev, Yearrange=yearn-year1)
coords <- data.frame(x=metadat$long,y=metadat$lat)


#  Generate a stack object with elevation used for worldclim2
T_filenames<- "../Worldclim2/wc2.1_30s_elev.tif"

elev<- stack(T_filenames)

values <- extract(elev, coords)

# Generate temp data (using worldclim1, fill in 2 later)

# r <- getData("worldclim",var="bio", res=10)
# 
# r <- r[[1]]
# names(r) <- c("Temp")
# 
# temps <- extract(r,coords)

temps <- read.csv('../Worldclim2/wordclim2_temp_extracted.csv')

df <- cbind.data.frame(coordinates(points),temps, values)
colnames(df)[1:2] <- c('x','y')
df <- cbind.data.frame(df, metadat)

# Calculate lapse rate for each site
#df1 <- df %>% group_by(gradient) %>% summarize(elev_ave = mean(WC_elev)) %>% left_join(df)
df2 <- df %>% rowwise() %>% mutate(T_ann = mean(c(wc2.1_30s_tavg_01:wc2.1_30s_tavg_12))) %>%
  mutate(T_sum = mean(c(wc2.1_30s_tavg_06:wc2.1_30s_tavg_08))) %>%
  mutate(T_win = mean(c(wc2.1_30s_tavg_11, wc2.1_30s_tavg_12, wc2.1_30s_tavg_01, wc2.1_30s_tavg_02))) %>%
  mutate(T_ann_cor = T_ann+(0.0098*(wc2.1_30s_elev-elev)),
         T_sum_cor = T_sum+(0.0098*(wc2.1_30s_elev-elev)),
         T_win_cor = T_win+(0.0098*(wc2.1_30s_elev-elev))) %>%
  select(siteID =loc, gradient, site, lat, lon=long, yearn, year1, year_range = Yearrange, elev, T_ann, T_sum, T_win, T_ann_cor, T_sum_cor, T_win_cor) %>%
  mutate(T_ann_cum = T_ann_cor*(year_range),
         T_sum_cum = T_sum_cor*(year_range),
         T_win_cum = T_win_cor*(year_range),
         T_ann_Scum = T_ann_cor*(2018-year1),
         T_sum_Scum = T_sum_cor*(2018-year1),
         T_win_Scum = T_win_cor*(2018-year1))

write.csv(df2, '../worlclim2_processedtemp.csv')
