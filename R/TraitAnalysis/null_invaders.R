
#use trait_f from invadertraits_odt.R for trait data
#traitdat_all <- traitdat_f %>% mutate(SpeciesName=species)

# Use alltraits from trait_drakeplan.R
traitdat_all <- alltraits %>% mutate(SpeciesName=species) %>% select(-species)

# match with traitdat_f from invadertraits_odt.R to get invaders listed
trait_invader <- traitdat_f %>% mutate(SpeciesName=species) %>% select(originSiteID:SpeciesName, -species)

#create community matrix for all sites
tr_dat <- traitdat_all %>% left_join(., trait_invader) #%>% #need year so right join instead of left
  #filter(!rowSums(is.na(.[,8:14]))==7) %>%
  #filter(!is.na(Rel_Cover))

# create wide format trait dataframes (without year)
tr_wide <-tr_dat %>% group_by(Region, originSiteID, destSiteID) %>%
  nest() %>%
  mutate(comm_wide = map(data, ~{
    .x %>% select(ODT, destPlotID, Year, SpeciesName, invader, Rel_Cover) %>% 
      pivot_wider(names_from = SpeciesName, values_from = Rel_Cover, values_fill = list(Rel_Cover = 0), values_fn = list(Rel_Cover = sum))
  })) %>% 
    mutate(comm_meta  = map(comm_wide, select, ODT, destPlotID,invader), 
           comm_spp = map(comm_wide, select, -ODT, -destPlotID, -invader),
           comm_traits = map(data, select, leaf_area:SLA),
           comm_tot_num = map(data, ~{.x %>%
               group_by(ODT, destPlotID) %>%
               summarise(ntot=n_distinct(SpeciesName))}),
           comm_inv_num = map(data, ~{.x %>%
               filter(invader=="invader") %>%
               group_by(ODT, destPlotID) %>%
               summarise(ninv=n_distinct(SpeciesName))}))


# Calculate invasive total cover per plot
tr_results <- tr_dat %>% 
      select(Region, Year, originSiteID, destSiteID, ODT, destPlotID, invader, SpeciesName, Rel_Cover) %>%
      group_by(Region, Year, originSiteID, destSiteID, ODT, destPlotID, invader) %>%
      summarise(Total_cover = sum(Rel_Cover, na.rm=T)) 
      

tr_results %>% filter(invader=="invader") %>%
  ggplot(aes(x = Year, y = Total_cover, group=destSiteID)) + 
  TP_theme() +
  facet_grid(~Region) +
  geom_point() +
  geom_smooth() +
  labs(color="Species Status", y="Proportional Cover of Invasives", x='Year') 

# Calculate invasive species z scores for individual traits

#Calculate true trait values for invaders (mean per plot)
df2 <- tr_dat %>% 
  select(Region, Year, destSiteID, ODT, destPlotID, invader, leaf_area:SLA) %>%
  filter(invader =="invader") %>% select(-invader) %>%
  group_by(Region, destSiteID, ODT, Year, destPlotID) %>%
  summarise_at(vars(leaf_area:SLA), mean, na.rm=T) 
  
#NULL TRAITS BY PLOT:Calculate both real and null trait values and get z score. df3 was using original null which included invaders
df4 <- tr_dat %>% 
  select(Region, Year, destSiteID, ODT, destPlotID, invader, leaf_area:SLA) %>%
  group_by(Region, destSiteID, ODT, Year, destPlotID) %>%
  nest() %>%
  mutate(real = map(data, ~{.x %>%  
      filter(invader == "invader") %>%
      summarise_at(vars(leaf_area:SLA), mean, na.rm=T)})) %>%
  unnest(real) %>%
  ungroup() %>%
  filter(!rowSums(is.na(.[,7:13]))==7) %>%
  mutate(nulls = map(data, ~run_sim(1000, .x))) 


rar <- df4 %>% unnest(nulls) %>%
  mutate(z.leaf_area=(leaf_area-leaf_area_fn1)/leaf_area_fn2, 
         z.leaf_C=(leaf_C-leaf_C_fn1)/leaf_C_fn2, 
         z.leaf_N=(leaf_N-leaf_N_fn1)/leaf_N_fn2, 
         z.leaf_P=(leaf_P-leaf_P_fn1)/leaf_P_fn2, 
         z.plant_height=(plant_height-plant_height_fn1)/plant_height_fn2, 
         z.seed_mass=(seed_mass-seed_mass_fn1)/seed_mass_fn2, 
         z.sla=(SLA-SLA_fn1)/SLA_fn2) 

# Plot z scores by region, year and by elevation or cum temp
#keep in mind this is average invader traits, not individual so more power if done at individual level
# this is also at the plot level! should increase species pool to site level
#could also take weighted average of trait means at plot level?
#make this plot first plus cum temp and ancillary stuff and then come back to this

rar %>% pivot_longer(cols=z.leaf_area:z.sla) %>% 
  filter(value<10 & value >-10) %>%
  group_by(Region) %>%
  filter(Year==min(Year)) %>%
  ggplot(aes(x=name, y=value), fill="grey") +
  geom_boxplot() +
  TP_theme() +
  annotate("rect", xmin=0, xmax=Inf, ymin=-1.96, ymax=1.96, alpha=0.2) +
  labs(x="Trait", y="Effect size (Z)")
  
rar %>% 
  ggplot(aes(x=Year, y=z.sla)) +
  geom_point() +
  TP_theme() +
  facet_wrap(~Region) +
  geom_smooth(method="lm") + 
  geom_hline(yintercept=c(1.96, -1.96))

#NULL TRAITS BY SITE:Calculate both real and null trait values and get z score. df3 was using original null which included invaders
df5 <- tr_dat %>% 
  select(Region, Year, destSiteID, ODT, invader, leaf_area:SLA) %>%
  group_by(Region, destSiteID, ODT, Year) %>%
  nest() %>%
  mutate(real = map(data, ~{.x %>%  
      filter(invader == "invader") %>%
      summarise_at(vars(leaf_area:SLA), mean, na.rm=T)})) %>%
  unnest(real) %>%
  ungroup() %>%
  filter(!rowSums(is.na(.[,6:12]))==7) %>%
  mutate(nulls = map(data, ~run_sim(1000, .x))) 

rar <- df5 %>% unnest(nulls) %>%
  mutate(z.leaf_area=(leaf_area-leaf_area_fn1)/leaf_area_fn2, 
         z.leaf_C=(leaf_C-leaf_C_fn1)/leaf_C_fn2, 
         z.leaf_N=(leaf_N-leaf_N_fn1)/leaf_N_fn2, 
         z.leaf_P=(leaf_P-leaf_P_fn1)/leaf_P_fn2, 
         z.plant_height=(plant_height-plant_height_fn1)/plant_height_fn2, 
         z.seed_mass=(seed_mass-seed_mass_fn1)/seed_mass_fn2, 
         z.sla=(SLA-SLA_fn1)/SLA_fn2) 

rar %>% pivot_longer(cols=z.leaf_area:z.sla) %>% 
  filter(value<20 & value >-20) %>%
  group_by(Region) %>%
  filter(Year==min(Year)) %>%
  ggplot(aes(x=name, y=value), fill="grey") +
  geom_boxplot() +
  TP_theme() +
  annotate("rect", xmin=0, xmax=Inf, ymin=-1.96, ymax=1.96, alpha=0.2) +
  labs(x="Trait", y="Effect size (Z)")

rar %>% 
  ggplot(aes(x=Year, y=z.sla)) +
  geom_point() +
  TP_theme() +
  facet_wrap(~Region) +
  geom_smooth(method="lm") + 
  geom_hline(yintercept=c(1.96, -1.96))

m1<-lme(z.sla ~ Region, random = ~1|destSiteID, method = "ML", data=rar) 
summary(m1)
anova(m1)
mnull<-lme(z.sla ~ 1, random = ~1|destSiteID, method = "ML", data=rar) 
anova(m1, mnull)

m1<-lme(z.leaf_P ~ Region, random = ~1|destSiteID, method = "ML", data=rar) 
summary(m1)
anova(m1)
mnull<-lme(z.leaf_P ~ 1, random = ~1|destSiteID, method = "ML", data=rar) 
anova(m1, mnull)

#necessary functions
#just samples invaders across both invading and local species
traits.null <- function(x) {
  x %>% 
    mutate(invader_null = sample(invader)) %>%
    filter(invader_null == "invader") %>%
    summarise_at(vars(leaf_area:SLA), mean, na.rm=T) 
}

traits.nullv2 <- function(x) {
  x %>% 
    #group_by(destPlotID) %>% 
    nest(data=everything()) %>%
    mutate(invader_n = map(data, ~{sum(.x$invader=="invader")}),
           residents = map(data, filter, invader == "resident")) %>%
    #ungroup() %>%
    mutate(samp = map2(residents, invader_n, sample_n)) %>% 
    select(-data, -invader_n, -residents) %>%
    unnest(samp) %>%
    summarise_at(vars(leaf_area:SLA), mean, na.rm=T) 
}


run_sim <- function(iterations, dat) {
        results <- rerun(iterations, {
            traits.nullv2(dat)
          }) %>%
          bind_rows()
        
        results %>% select(leaf_area:SLA) %>%
          summarise_all(., .funs=c(mean, sd), na.rm=T)
      }
# Calculate distance based z scores (invader to mean centroid of community)
  