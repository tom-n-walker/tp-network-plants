#### Code to merge all community data (with metadata) together ####

merge_site_trait_data <- function(alldat) {
  
  
  #merge trait data from 7 sites
  dat <- alldat %>% 
    map_df("trait", .id='Region') %>%
    ungroup() 
  
  #sanity checks:
  #unique(dat$destSiteID) %in% unique(meta$destSiteID) #all true
  #unique(meta$destSiteID) %in% unique(dat$destSiteID) #all true
  # fulldat[is.na(fulldat$Rel_Cover),] #no NA Rel_covers (cover yes, arizona only has rel_cover)
  # dat[is.na(dat$Treatment),] #no NA treatments
  #fulldat %>% group_by(Region, destSiteID, Treatment) %>% summarize(n=n()) %>% View
  
  return(dat) 
  
}

# Import_tr8_trait_data <- function(alldat) {
#   tr8_traits<-tr8(species_list=c(trytraits$genus_species$submitted_name),download_list=c("dispersal_morphology", "growth_form", "woodiness", #in dataset LEDA
#                                                                                    "reprod_B", "li_form_B", #in dataset Biolflor
#                                                                                    "Drought.Tolerance", "Moisture.Use", "Vegetative.Spread.Rate", "Seed.Spread.Rate", "Propagated.by.Bare.Root")) #in dataset plants
#   return(tr8_traits)
#   }


merge_trait_data <- function(traits) {
  
  # get species means per site for site data
  site_traits_mean <- traits$sitetraits %>% 
    group_by(Country, Region, destSiteID, SpeciesName, PlantID, Trait) %>%
    dplyr::summarize(mValue = mean(Value, na.rm=T))%>%
    group_by(Country, Region, destSiteID, SpeciesName, Trait) %>%
    dplyr::summarize(siteValue = mean(mValue, na.rm=T), nRep=n()) 
  
  #get genus-species data from TRY
  #trytraits_s <-trytraits$species_only 
  trytraits_g <-traits$trytraits$genus_species %>% #gap filled based on genus and species
      rename(TLeaf_Area_cm2 = leaf_area, TC_percent = leaf_C, TN_percent = leaf_N, TP_percent = leaf_P, TPlant_Veg_Height_cm = plant_height, TSeed_Mass = seed_mass, TSLA_cm2_g = SLA, TStem_density = stem_density)
  # trait names: Plant_Veg_Height_cm, Plant_Rep_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent , CN_ratio
  
  dat_sp <- dat %>% left_join(.,sp_codes, by=c('SpeciesName'='code')) %>%
    mutate(species = ifelse(!is.na(taxa), taxa, SpeciesName)) %>%
    filter(!is.na(species), !is.na(SpeciesName)) %>%
    select(Region, Year, originSiteID, destSiteID, Treatment, destPlotID, SpeciesName, Rel_Cover, taxa, species)
  
  # Widen site trait dataframe
  site_traits_means <- site_traits_mean %>% select(-nRep) %>% spread(key = Trait, value=siteValue) %>%
    select(Country, Region, destSiteID, SpeciesName, Plant_Veg_Height_cm, Leaf_Area_cm2,  SLA_cm2_g, C_percent, N_percent)
  
  # Match site data with try trait data
  dat_tr <- inner_join(dat_sp, site_traits_means, by = c("species" = "SpeciesName", "Region", "destSiteID"))
  
  # Then match try data to those cells with missing site data
  dat_traits <- left_join(dat_tr, trytraits_g, by = c("species" = "original_name"))
  
  # Now gap-fill site data if it doesn't exist with TRY data
  traits_merged <- dat_traits %>% 
    mutate(Plant_Veg_Height_cm = ifelse(!is.na(Plant_Veg_Height_cm), Plant_Veg_Height_cm, TPlant_Veg_Height_cm),
           Leaf_Area_cm2 = ifelse(!is.na(Leaf_Area_cm2), Leaf_Area_cm2, TLeaf_Area_cm2),
           SLA_cm2_g = ifelse(!is.na(SLA_cm2_g), SLA_cm2_g, TSLA_cm2_g), 
           C_percent = ifelse(!is.na(C_percent), C_percent, TC_percent),
           N_percent = ifelse(!is.na(N_percent), N_percent, TN_percent), 
           P_percent = TP_percent, 
           Seed_Mass = TSeed_Mass, 
           Stem_density = TStem_density)
  
  return(traits_merged) 
  
}