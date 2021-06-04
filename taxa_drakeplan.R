#############################
### TRAIT DRAKE PLAN ########
#############################
#C. Chisholm, 13 April 2021
# J. Lynn, 3 June 2021

# Load libraries
library("drake")
library("tidyverse")
library("readxl")
library("DBI")
library("visNetwork")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)


# Source downstream trait scripts
source("R/clean_taxonomy.R") # for cleaning species lists from 'taxa' list element for all sites
source("R/site_taxa_codes.R") #for sp_codes for No, Arizona and Sweden where they use species codes in dat

# Import Taxa lists from all sites and clean to accepted names
ImportTaxaDrakePlan <- drake_plan(
  
  taxa = get_species(),
  
  cleaned = resolve_species(taxa),
  
)

# Import names from community dataframe
ImportSiteTaxaDrakePlan <- drake_plan(
  sitetaxa = merge_site_taxa_data(sitedata = tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
                                                           CH_Lavey, CH_Calanda, 
                                                           US_Colorado, US_Montana, US_Arizona,
                                                           CN_Damxung, IN_Kashmir, CN_Gongga, CN_Heibei, 
                                                           DE_Grainau, DE_Susalps, DE_TransAlps, FR_AlpeHuez, SE_Abisko,
                                                         FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2))
  
)

# Load lookup tables where available (SE_Abisko, US_Arizona, NO_XX) and add 
LookupTableDrakePlan <- drake_plan(
  se_dat = load_SE_Abisko_sptable(),
  no_dat = load_Norway_sptable(),
  us_dat = load_US_Arizona_sptable(),
  spcodes =  merge_sptable(spdat = tibble::lst(se_dat, no_dat, us_dat)) # has regions as a column but Norway is just "Norway"
  
)

MergeSpeciesCodes <- drake_plan(
  species = merge_all_taxa_data(alldat = tibble::lst(cleaned, sitetaxa, spcodes))
  
)

MyPlan <- bind_rows(ImportTaxaDrakePlan, ImportSiteTaxaDrakePlan, LookupTableDrakePlan, MergeSpeciesCodes)

conf <- drake_config(MyPlan)
conf

make(MyPlan)

loadd(species)
