dat_fil <- dat %>% filter(!is.na(Rel_Cover)) %>% #4 NAs in DE_Susalps, correct that in cleaning file
  filter(!(Region=="DE_Susalps" & Year==2016))

source('R/theme_ggplot.R')
library(LearnGeom) #for trig functions

#### RUN PCA AND EXTRACT DISTANCES ####
dd <- dat_fil %>% 
  select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
    bind_rows(
      originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
      destControls = dat %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
      warmed =  dat %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
      .id = "ODT")
  })) %>% 
  mutate(comm_wide = map(comm, ~{
    .x %>% select(ODT, Year, SpeciesName, Rel_Cover, destPlotID) %>% 
      pivot_wider(names_from = SpeciesName, values_from = Rel_Cover, values_fill = list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))
  })) %>% 
  mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID, Year), 
         comm_spp = map(comm_wide, select, -ODT, -destPlotID, -Year)) %>% 
  select(-comm_wide) %>% 
  mutate(PCA = map(comm_spp, ~rda(sqrt(.x)))) %>% 
  mutate(scores = map(.x = PCA, ~scores(.x, choices=c(1,2), display='sites'))) %>%
  mutate(scores = map(.x = scores, ~data.frame(PCA1=.x[,1], PCA2=.x[,2]))) %>%
  mutate(scores = map2(.x = comm_meta, .y = scores, bind_cols))

#### RUN PCA AND EXTRACT STANDARDIZED DISTANCES ####
# Get single point per region

dd2 <- dd %>% 
  select(-(comm:PCA)) %>% 
  unnest(scores) %>% 
  group_by(Region, originSiteID, destSiteID, ODT, Year) %>%
  summarise_at(vars(matches("PC")), .funs = mean) %>% 
  group_by(Region, originSiteID, destSiteID) %>% 
  filter(Year == max(Year)) %>%
  select(-Year) %>%
  nest() %>% 
  #mutate(distances = map(data, ~dist(select(.x, matches("PC"))))) %>% 
  #mutate(distances = map(distances, ~tibble(what = c("Low_High", "Low_TP", "High_TP"), dist = as.vector(.x)))) #%>% 
  mutate(point = map(data, function(x) {
    a=as.numeric(x[2,2:3]) # a vertice (origin)
    b=as.numeric(x[1,2:3]) # b vertice (dest)
    c=as.numeric(x[3,2:3]) # c vertice (warmed)
    dis <- DistancePoints(a, b) # distance between origin and dest
    # Line <- CreateLinePoints(a, b)
    # pro <- ProjectPoint(c, Line)
    c <- abs(c/dis) #the length of intersecting line between c and the a-b segment
    # pro <- abs(pro/dis)
    ppro <- c[1] # the projected intersection point of c along that a-b segment
    return(tibble(Treatment = c("origin", "destination", "warmed", "origin", "destination", "proj_warmed"), value = c(1,1,1,2,2,2), Xcoord = c(0, 1, c[1], 0, c[1], c[1]), Ycoord = c(0, 0, c[2], 0, 0, c[2])))
  })) %>% 
  unnest(point)

# Plot triangles for each site
dd2 %>%
  ggplot(aes(x = Xcoord, y = Ycoord)) + 
  TP_theme() +
  geom_polygon(aes(fill = as.factor(value), group = as.factor(value))) +
  scale_fill_manual(values = c("light grey", "blue")) +
  facet_wrap(~Region)  +
  xlim(0,1) 

# Get point per plot

dd2 <- dd %>% 
  select(-(comm:PCA)) %>% 
  mutate(odt_ave = map(scores, function(x) x %>% 
                         group_by(ODT, Year) %>%
                         summarise_at(vars(matches("PC")), .funs = mean) %>% 
                         filter(Year == max(Year)) %>%
                         select(-Year))) %>% 
  mutate(dis = map(odt_ave, function(x) {
    a=as.numeric(x[2,2:3]) #origin
    b=as.numeric(x[1,2:3]) #destination
    c=as.numeric(x[3,2:3]) #warmed plots
    dis = DistancePoints(a, b)
    return(dis)}),
  avepoints = map(odt_ave, function(x) {
      a=as.numeric(x[2,2:3]) #origin
      b=as.numeric(x[1,2:3]) #destination
      c=as.numeric(x[3,2:3]) #warmed plots
      dis = DistancePoints(a, b)
      c <- abs(c/dis) #the length of intersecting line between c and the a-b segment
      return(tibble(Treatment = c("origin", "destination", "warmed"), PCA1 = c(0, 1, c[1]), PCA2 = c(0, 0, c[2])))
    })) %>%
  unnest(dis) %>%
  mutate(odt_s = map2(scores, dis, function(.x,.y) {
    newpoint = .x %>% 
      mutate(PCA1 = abs(as.numeric(PCA1)/.y), PCA2=abs(as.numeric(PCA2)/.y)) %>%
      filter(ODT == "warmed") %>%
      select(destPlotID, PCA1, PCA2)
    return(newpoint)
  })) 

dd_ind <- dd2 %>% unnest(odt_s) #individual obs, however we may have >1 as the scaling is between average values per site, not plot-level (because this is impossible)

# Get average per Site to overplot
dd_ind_ave <- dd_ind %>% group_by(Region, originSiteID, destSiteID) %>% summarize(pca1 = mean(PCA1), pca2=mean(PCA2), se1 = sd(PCA1)/sqrt(n()), se2 = sd(PCA2)/sqrt(n()))

dd_group <- dd2 %>% unnest(avepoints) # group obs

ggplot(dd_ind, aes(x = PCA1, y = PCA2, color = Region)) +
  geom_point(alpha = .4) +
  geom_point(data = dd_group, size = 4) +
  theme_bw() +
  guides(color = guide_legend("Region")) +
  labs(
    x = "Distance from origin to destination",
    y = "Distance to point"
  )

#### ADD CLIMATE DATA ####

# Read in temperature and compute cumulative temp for ordering graph

temp <- read.csv('./climate/worlclim2_processedclimate.csv')

#Calculate annual cumulative warming
Temp <- temp %>% select(Gradient, destSiteID, T_ann_cor, T_sum_cor, V_ann, P_ann, YearRange, PlotSize_m2) 

#Join temp data to originSiteID
df1 <- left_join(dd_ind, Temp, by=c("Region"="Gradient", "originSiteID"="destSiteID")) %>% 
  mutate(T_ann_origin = T_ann_cor, T_sum_origin = T_sum_cor, V_ann_origin = V_ann, P_ann_origin = P_ann) %>% 
  select(-T_ann_cor, -T_sum_cor, -V_ann, -P_ann, -YearRange, -PlotSize_m2)

#Join temp data to destSiteID
df2 <- left_join(df1, Temp, by=c("Region"="Gradient", "destSiteID"="destSiteID")) %>% 
  mutate(T_ann_dest = T_ann_cor, T_sum_dest = T_sum_cor, V_ann_dest = V_ann, P_ann_dest = P_ann) %>%
  select(-T_ann_cor, -T_sum_cor, -V_ann, -P_ann)

#Calculate origin - destSiteID temps and *Year for cumulative 
df3 <- df2 %>% select(Region,  originSiteID, destSiteID, destPlotID, PCA1, PCA2, T_ann_dest, T_ann_origin,  P_ann_dest, P_ann_origin, YearRange, PlotSize_m2) %>% 
  mutate(T_warm = ifelse(Region == "US_Arizona", T_ann_origin-T_ann_dest, T_ann_dest-T_ann_origin), P_warm = ifelse(Region =="US_Arizona", P_ann_origin-P_ann_dest, P_ann_dest-P_ann_origin)) %>%#US_Arizona reversed. Swap sites for now, need to chat to Bruce and Rachel.
  mutate(T_warm_cum = T_warm*YearRange, P_warm_cum = P_warm*YearRange)

# Standardize response (PCA1) between 0 and 1 (0,1) for beta distribution
dd_Temp_sum <- df3 %>% 
  select(Region, originSiteID, destSiteID, destPlotID, PCA1, PCA2, T_warm, T_warm_cum, P_warm, P_warm_cum, YearRange, PlotSize_m2) %>%
  mutate(Gradient = paste(originSiteID, destSiteID, sep='_')) %>%
  mutate(PCA1 = 0.00001+(1-2*0.00001)*(PCA1-min(PCA1))/(max(PCA1)-min(PCA1))) %>% 
  mutate(T_warm = abs(T_warm), T_warm_cum=abs(T_warm_cum)) %>% # to fix issue of GW and FE giving - values to BT (issue with BT coords?)
  mutate(T_warms = scale(T_warm), T_warm_cums = scale(T_warm_cum), P_warms = scale(P_warm), P_warm_cums = scale(P_warm_cum), YearRanges = scale(YearRange), PlotSize_m2s = scale(PlotSize_m2))

dd_clim_ave <- dd_Temp_sum %>% 
  group_by(Region, Gradient) %>%
  summarize_each(PCA1:PlotSize_m2s, funs=c(mean, sd)) 
  

#### RUNNING BRMS MODEL AGAINST ALL PLOTS (REGION RANDOM EFFECT) ####
library(brms)
library(modelr)
library(tidybayes)

m1 = brm(
  PCA1 ~ T_warm_cums + P_warm_cums + (1|Gradient), 
  data = dd_Temp_sum,
  iter = 4000,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

m2 = brm(
  PCA1 ~ YearRange + T_warms + P_warms + (1|Gradient), 
  data = dd_Temp_sum,
  iter = 4000,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

m3 = brm(
  PCA1 ~ YearRange + (1|Gradient), 
  data = dd_Temp_sum,
  iter = 4000,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

m4 = brm(
  PCA1 ~ T_warm_cums + (1|Gradient), 
  data = dd_Temp_sum,
  iter = 4000,
  family=Beta(link = "logit", link_phi = "log") # data scaled between 0 and 1
)

dd_Temp_sum %>%
  group_by(Gradient) %>%
  data_grid(T_warm_cums = seq_range(T_warm_cums, n = 51)) %>%
  add_fitted_draws(m4) %>%
  ggplot(aes(x = T_warm_cums, y = PCA1)) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = dd_Temp_sum) +
  scale_fill_brewer(palette = "Greys") +
  #scale_color_brewer(palette = "Set2") +
  TP_theme() 

#### BRMS MODEL PCA1 ~ ALL CLIMATE DATA
# for gaussian use: PCA1_fn1|resp_se(PCA1_fn2, sigma = TRUE)

m1 = brm(
  PCA1_fn1|mi(PCA1_fn2) ~ PlotSize_m2s_fn1 + YearRange_fn1 + (1|Region:Gradient), # plot size and year range no effect
  data = dd_clim_ave,
  iter = 10000,
  cores=3, 
  chains=3,
  control = list(adapt_delta=0.99, max_treedepth = 20), 
  family=Beta(link = "logit", link_phi = "log") 
)

m2 = brm(
  PCA1_fn1|mi(PCA1_fn2) ~ YearRanges_fn1 + T_warms_fn1 + P_warms_fn1 + (1|Gradient), # none significant
  data = dd_clim_ave,
  iter = 10000,
  control = list(adapt_delta=0.99, max_treedepth = 20), 
  family=Beta(link = "logit", link_phi = "log") 
)

m3 = brm(
  PCA1_fn1|mi(PCA1_fn2) ~ T_warm_cums_fn1 + P_warm_cums_fn1 + (1|Gradient), # t-warm sig, but negative !?
  data = dd_clim_ave,
  iter = 10000,
  control = list(adapt_delta=0.99, max_treedepth = 20), 
  family=Beta(link = "logit", link_phi = "log") 
)

m4 = brm(
  PCA1_fn1|mi(PCA1_fn2) ~  T_warm_fn1 + P_warm_fn1 + T_warm_cum_fn1 + P_warm_cum_fn1 + (1|Gradient), # use 0+ if unscaled
  data = dd_clim_ave,
  iter = 10000,
  cores=3, 
  chains=3,
  control = list(adapt_delta=0.99, max_treedepth = 20), 
  family=Beta(link = "logit", link_phi = "log") 
)


m5 = brm(
  PCA1_fn1|mi(PCA1_fn2) ~  PlotSize_m2s_fn1*T_warm_cums_fn1 + (1|Gradient), # no interaction of plotsize, but negative tcum
  data = dd_clim_ave,
  iter=8000,
  control = list(adapt_delta=0.99, max_treedepth = 15), 
  family=Beta(link = "logit", link_phi = "log") 
)

# Plotting posterior predictions over range of new values

newdf <- dd_clim_ave %>%
  distinct(T_warm_cums_fn1, PCA1_fn2) %>%
  mutate(Region = "fake")  %>%
  add_fitted_draws(m5, allow_new_levels = TRUE)

ggplot(newdf) +
  aes(x = T_warm_cums_fn1, y = .value) +
  stat_lineribbon(
    .width = c(.25, .5, .75, .95)
  ) +
  geom_point(data=dd_clim_ave, aes(x=T_warm_cums_fn1, y = PCA1_fn1)) +
  scale_color_viridis_d(aesthetics = "fill") +
  guides(fill = FALSE) +
  theme_grey(base_size = 14) +
  TP_theme()

  #Example model from CC's climate change paper
  
  # m7x<-bf(estimate|resp_se(std.error, sigma = TRUE)~ treatment*siteT + treatment*siteyear_deltaT + (treatment|site_name)+ (treatment|site_name:year) + (treatment|spp)+ (treatment|site_name:subsite)) 
  # fit_m7x_fruit<- brm(m7x, data = regmodsx, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian)
  # save(fit_m7x_fruit, file="Data/brms_output/fit_m7x_fruit.Rdata")
  
  # Predictor of those species that colonise and those that don't
  # Predict those species that establish and grow (subset those that have colonised and lasted for >1 year) (increase in rel abundance?)
  # Two step model, in Stan if you have a log link function eventually the values go so low that the model freaks out, so add +1 to the - something, can do the same for the amplitude of it
  
  # response - min + min-max, 
  
#### PLOT AVERAGE SITE VALUES BY TEMPERATURE ####
  
  #order by Temp (black to red) and P_warm (black to blue) <- VPD doesn't vary much
  dd_clim_ave$SiteID <- dd_clim_ave$Gradient
  TS <- dd_clim_ave$T_warm_cums_fn1
  TSA <- dd_clim_ave$T_warms_fn1
  PR <- dd_clim_ave$P_warm_cums_fn1
  PRA <- dd_clim_ave$P_warms_fn1
  
  TS <- (TS-min(TS)) / (max(TS)-min(TS))
  TSA <- (TSA-min(TSA)) / (max(TSA)-min(TSA))
  PR <- (PR-min(PR)) / (max(PR)-min(PR))
  PRA <- (PRA-min(PRA)) / (max(PRA)-min(PRA))
  
  TScol <- rgb(TS, 0, 1-TS) #R, G, B colors, between 0 to 1
  TSAcol <- rgb(TSA, 0, 0)
  PRcol <- rgb(0, 0, PR) 
  PRAcol <- rgb(0, 0, PRA) 

  names(TScol) <- as.character(dd_clim_ave$SiteID)
  names(TSAcol) <- as.character(dd_clim_ave$SiteID)
  names(PRcol) <- as.character(dd_clim_ave$SiteID)
  names(PRAcol) <- as.character(dd_clim_ave$SiteID)

  
  #### For change in summer temp (dest-origin) 
  p1 <-  dd_clim_ave %>% arrange(T_warm_cums_fn1) %>%    
    mutate(SiteID=factor(SiteID, levels=SiteID)) %>%
    ggplot(aes(x = PCA1_fn1, y = PCA2_fn2, color = SiteID)) +
    geom_point(alpha = 3, size=3) +    
    geom_errorbar(aes(ymin=PCA2_fn1-PCA2_fn2, ymax=PCA2_fn1+PCA2_fn2), width=.01) +
    geom_errorbar(aes(xmin=PCA1_fn1-PCA1_fn2, xmax=PCA1_fn1+PCA1_fn2), width=.01) +
    geom_line() +
    scale_colour_manual(values=TScol) +
    TP_theme() +
    xlim(0,1) +
    ylim(0,1) +
    guides(color = guide_legend("Region")) +
    labs(
      x = "",
      y = "Distance to point"
    )
  
  p2 <-  dd_clim_ave %>% arrange(T_warm_cums_fn1) %>%    
    mutate(SiteID=factor(SiteID, levels=SiteID)) %>%
    ggplot(aes(x = SiteID, y=PCA2_fn1, color = SiteID)) +
    geom_point(alpha = 3, size=3) +   
    geom_errorbar(aes(ymin=PCA2_fn1-PCA2_fn2, ymax=PCA2_fn1+PCA2_fn2), width=.01) +
    geom_line() +
    scale_colour_manual(values=TScol) +
    TP_theme() +
    ylim(0,1) +
    labs(
      x = "Region",
      y = ""
    )
  
  p3 <- dd_clim_ave %>% arrange(T_warm_cums_fn1) %>%    
    mutate(SiteID=factor(SiteID, levels=SiteID)) %>%
    ggplot(aes(x = PCA1_fn1, y=SiteID, color = SiteID)) +
    geom_point(alpha = 3, size=3) +   
    geom_errorbar(aes(xmin=PCA1_fn1-PCA1_fn2, xmax=PCA1_fn1+PCA1_fn2), width=.01) +
    geom_line() +
    scale_colour_manual(values=PRcol) +
    TP_theme() +
    xlim(0,1) +
    labs(
      x = "Distance from origin to destination",
      y = "Region"
    )
  
  library(cowplot)
  
  legend <- get_legend(
    # create some space to the left of the legend
    p1 + theme(legend.position = "right")
  )
  
  
  plot_grid(p1 + theme(legend.position="none"), 
            p2 + theme(legend.position="none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)), 
            p3 +  theme(legend.position="none"), 
            align = "vh", nrow = 2, 
            rel_heights = c(1/2, 1/2, 1/2),
            rel_widths = c(1/2, 1/2, 1/2),
            labels = c('A', 'B', 'C'))
  