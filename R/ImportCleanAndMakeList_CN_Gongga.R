##################
### CN_GONGGA  ###
##################

#### Cleaning Code ####
# Clean trait data
CleanTrait_CN_Gongga <- function(dat){
  dat2 <- dat %>% 
    filter(Project %in% c("LOCAL", "0", "C")) %>% 
    mutate(Treatment = plyr::mapvalues(Project, c("C", "0", "LOCAL"), c("C", "O", "Gradient"))) %>% 
    mutate(Taxon = trimws(Taxon)) %>% 
    mutate(Year = year(Date),
           Country = "CH",
           Gradient = as.character(1),
           Project = "T") %>% 
    rename(BlockID = Location) %>%
    mutate(PlotID = paste(BlockID, Treatment, sep = "-"),
           ID = paste(Site, Treatment, Taxon, Individual_number, Leaf_number, sep = "_")) %>% 
    select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, C_percent, N_percent , CN_ratio, dN15_percent, dC13_percent, P_AVG, P_Std_Dev, P_Co_Var) %>% 
    gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -BlockID, -PlotID, -Taxon) %>% 
    filter(!is.na(Value))
  
  return(dat2)
}

# Cleaning community data
CleanCommunity_CN_Gongga <- function(dat){
  dat2 <- dat %>% 
    filter(TTtreat != c("OTC")) %>% 
    rename(Year = year, Treatment = TTtreat, Taxon = speciesName, Cover = cover) %>% 
    mutate(Country = "CH",
           Gradient = "CN_Gongga") %>% 
    mutate(Taxon = recode(Taxon, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla")) %>% 
    filter(!is.na(Cover), !Cover == 0)
  
  return(dat2)
}

# Cleaning China meta community data
CleanMetaCommunity_CN_Gongga <- function(dat){
  dat2 <- dat %>% 
    select(PlotID, Year, Moss, Lichen2, Litter, BareGround, Rock, Vascular, Bryophyte, Lichen, MedianHeight_cm, MedianMossHeight_cm) %>% 
    mutate(Country = "CH",
           Site = substr(PlotID, 1,1),
           Gradient = "1")
  return(dat2)
}

CleanMeta_CN_Gongga <- function(dat){
  dat2 <- dat %>% 
    mutate(Elevation = as.numeric(as.character(Elevation)),
           Gradient = "CN_Gongga",
           Country = as.character("China"),
           Site = as.character(Site),
           YearEstablished = 2012,
           PlotSize_m2 = 0.0625)
  
  return(dat2)
}



#### IMPORT, CLEAN AND MAKE LIST #### 
Import_CN_Gongga <- function(){
  
  ### IMPORT DATA
  ## CN_Gongga
  metaCN_Gongga_raw = get(load(file = file_in("data/metaCN_Gongga.Rdata")))
  metaCommunityCN_Gongga_raw = get(load(file = file_in("data/metaCommunityCN_Gongga_2012_2016.Rdata")))
  communityCN_Gongga_raw = get(load(file = file_in("data/cover_thin_CH_2012_2016.Rdata")))
  traitCN_Gongga_raw = get(load(file = file_in("data/traits_2015_2016_China.Rdata")))

  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCN_Gongga = CleanMeta_CN_Gongga(metaCN_Gongga_raw)
  metaCommunityCN_Gongga = CleanMetaCommunity_CN_Gongga(metaCommunityCN_Gongga_raw)
  communityCN_Gongga = CleanCommunity_CN_Gongga(communityCN_Gongga_raw)
  traitCN_Gongga = CleanTrait_CN_Gongga(traitCN_Gongga_raw)
  
  CN_Gongga = list(meta = metaCN_Gongga,
                   metaCommunity = metaCommunityCN_Gongga,
                   community = communityCN_Gongga,
                   trait = traitCN_Gongga)

  
  return(CN_Gongga)
}
