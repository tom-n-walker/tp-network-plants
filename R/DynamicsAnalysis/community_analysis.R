# COLONISATION PATTERNS ACROSS SITES 
#C.Chisholm, April 16, 2021

library(tidyverse)
library(brms)
library(tidybayes)
library(codyn)
source('R/theme_ggplot.R')


#### COLONISATION ACROSS SITES BY ODT ####
#turnover function from codyn

dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
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
  mutate(specrich = map(comm, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 

ddd <- dd %>%
  filter(!Region %in% c("FR_Lautaret", "IN_Kashmir", "US_Colorado")) %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Year_0 = Year-min(Year)))) %>%
  unnest(dat) %>% 
  filter(ODT=='warmed') %>%
  filter(comdyn%in% c('C', 'E', 'T')) 

Col <- ddd %>% filter(comdyn=='C') %>% select(Region, originSiteID, destSiteID, destPlotID, ODT, Year, Year_0, value)
Ext <- ddd %>% filter(comdyn=='E') %>% select(Region, originSiteID, destSiteID, destPlotID, ODT, Year, Year_0, value)
Tur <- ddd %>% filter(comdyn=='T') %>% select(Region, originSiteID, destSiteID, destPlotID, ODT, Year, Year_0, value)

#### Analyze turnover by treatment

mC_odt <- brm(formula = bf(value ~ Year_0*Region + (1 | originSiteID) +(1 | originSiteID:Region)),
    family = zero_inflated_beta,
    data = Col,
    # prior = c(prior(normal(14.5, 20), class = b, coef = "intercept"),
    #           prior(normal(0, 10),    class = b),
    #           prior(student_t(3, 0, 10), class = sd),
    #           prior(student_t(3, 0, 10), class = sigma),
    #           prior(lkj(4), class = cor)),
    iter = 2000, warmup = 1000, chains = 3, cores = 3,
    seed = 5)
summary(mC_odt)

mE_odt <- brm(formula = bf(value ~ Year_0*Region + (1 | originSiteID) +(1 | originSiteID:Region)),
              family = zero_inflated_beta,
              data = Ext,
              # prior = c(prior(normal(14.5, 20), class = b, coef = "intercept"),
              #           prior(normal(0, 10),    class = b),
              #           prior(student_t(3, 0, 10), class = sd),
              #           prior(student_t(3, 0, 10), class = sigma),
              #           prior(lkj(4), class = cor)),
              iter = 2000, warmup = 1000, chains = 3, cores = 3,
              seed = 5)
summary(mE_odt)

Tur$value[Tur$value==1] <- 0.99999
mT_odt <- brm(formula = bf(value ~ Year_0*Region + (1 | originSiteID) +(1 | originSiteID:Region)),
              family = zero_inflated_beta,
              data = Tur,
              # prior = c(prior(normal(14.5, 20), class = b, coef = "intercept"),
              #           prior(normal(0, 10),    class = b),
              #           prior(student_t(3, 0, 10), class = sd),
              #           prior(student_t(3, 0, 10), class = sigma),
              #           prior(lkj(4), class = cor)),
              iter = 2000, warmup = 1000, chains = 3, cores = 3,
              seed = 5)
summary(mT_odt)
summary(mT_odt)$fixed
summary(mT_odt)$random

ggplot(data = Tur, 
       aes(x   = Year_0,
           y   = value,
           col = as.factor(Region)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size     = .7,
             alpha    = .8,
             position = "jitter")+
  geom_smooth(method = lm,
              se     = FALSE, 
              size   = 2,
              alpha  = .8)+
  theme_minimal()+
  labs(title    = "Linear Relationship for Different Years of Teacher Experience as Observed", 
       subtitle = "The linear relationship between the two is not the same for all classes", 
       col      = "Years of\nTeacher\nExperience")

#### COLONISATION ACROSS SITES BY TEMP ####
temp <- read.csv('./climate/worlclim2_processedtemp.csv')

#replace incorrect site names in temp
temp$site <- as.character(temp$site)
temp$site[26:44] <- c("High", "Low", "Gudmedalen", "Arhelleren", "Rambera", 
                      "Lavisdalen", "Vikesland", "Hogsete", 
                      "Skjellingahaugen", "Ovstedal", "Veskre", 
                      "Ulvhaugen", "Fauske", "Alrust", "High", "Low", "Mid", "MC", "PP")
#[26:44] replaced

dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
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
  filter(Region %in% c("NO_Skjellingahaugen", "NO_Gudmedalen", "NO_Lavisdalen", "NO_Ulvhaugen", "CH_Lavey", "CN_Damxung", "CN_Gongga", "US_Arizona", "DE_Susalps", "SE_Abisko", "IT_MatschMazia")) %>%
  mutate(specrich = map(comm, ~ {.} %>% group_by(Year, destPlotID) %>% summarize(SR=n_distinct(SpeciesName))), 
         colonisation = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "appearance")),
         extinction = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "disappearance")),
         turnover = map(comm, ~turnover(.x, time.var= "Year", species.var= "SpeciesName", abundance.var= "Rel_Cover", replicate.var="destPlotID", metric = "total"))) 


#Join temp data
Temp <- temp %>% select(gradient, site, T_ann_cor)

dd_temp <- left_join(dd, Temp, by=c("Region"="gradient", "originSiteID"="site")) %>% 
  mutate(T_ann_origin = T_ann_cor) %>% select(-T_ann_cor)
dd_Temp <- left_join(dd_temp, Temp, by=c("Region"="gradient", "destSiteID"="site")) %>% 
  mutate(T_ann_dest = T_ann_cor, T_warm = T_ann_dest-T_ann_origin) %>% 
  select(-T_ann_cor)

#No data for DE_Susalps, US_Arizona is looking weird (I think sites reversed, making absolute for now)
#also three data points (18-20 missing NA)

# Calculate per plot
dd_T <- dd_Temp %>%
  filter(!Region %in% c("DE_Susalps", "CN_Damxung")) %>%
  mutate(comm_sim = map(comm, ~.x %>% select(ODT, destPlotID) %>% distinct())) %>%
  mutate(dat = pmap(.l = list(C=colonisation, E=extinction, To=turnover), .f = function(C, E, To){
    C <- C %>% rename(value = appearance)
    E <- E %>% rename(value = disappearance)
    To <- To %>% rename(value = total)
    bind_rows('C' = C,'E'= E, 'T' = To, .id='comdyn')})) %>% 
  mutate(dat = map2(dat, comm_sim, ~left_join(.x, .y, by = "destPlotID"))) %>%
  mutate(dat = map(dat, ~.x %>% filter(ODT == "warmed"))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Year_0 = Year-min(Year)))) %>%
  mutate(dat = map(dat, ~.x %>% mutate(Temp_cum = Year_0*abs(T_warm)))) %>%
  unnest(dat) %>% 
  filter(comdyn %in% c('C', 'E', 'T'))  


#### Analyze turnover by temperature (transplanted turfs only)

bgt <- 
  brm(
    formula = bf(rating ~ (1 | rater_id) + (1 | target_id) + (1 | condition) + 
                   (1 | rater_id:target_id) + (1 | rater_id:stimulus_type) + 
                   (1 | target_id:stimulus_type)),
    prior = c(...),
    data = dat, 
    ...
  )

bgt %>% 
  tidy_draws() %>% 
  select(starts_with("sd_"), sigma) %>% 
  transmute_all(.funs = list(sq = ~(. ^ 2))) %>% 
  mutate(total_var = rowSums(.)) %>% 
  mutate_at(.vars = vars(-total_var), 
            .funs = list(pct = ~(. / total_var))) %>% 
  map_df(.f = ~ median_hdci(., .width = 0.95), .id = "component") %>% 
  mutate(
    component = str_remove(component, "sd_"),
    component = str_remove(component, "__Intercept_sq"),
    component = str_replace(component, "sigma_sq", "residual")
  )
  