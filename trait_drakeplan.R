#############################
### TRAIT DRAKE PLAN ########
#############################
#C. Chisholm, 13 April 2021

# Load libraries
library("drake")
library("tidyverse")
library("vegan")
library("readxl")
library("lubridate")
library("e1071")
library("DBI")
library("RSQLite")
library("visNetwork")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)

# Source downstream scripts
source("R/clean_taxonomy.R")
source("R/Name_repair.R")
source("R/merge_traits.R")


# Import TRY Data
ImportTRYDrakePlan <- drake_plan(

taxa = get_species(),

cleaned = resolve_species(taxa),

trytraits = load_wrangle_try(cleaned)

)

# Merge Field-collected trait data

MergeTraitDrakePlan <- drake_plan(
  sitetraits = merge_trait_data(alldat = tibble::lst(NO_Ulvhaugen, NO_Lavisdalen, NO_Gudmedalen, NO_Skjellingahaugen, 
                                             CH_Calanda, US_Colorado, CN_Gongga, FR_AlpeHuez, FR_Lautaret, IT_MatschMazia1, IT_MatschMazia2))
)

# CleanTraitDrakePlan <- drake_plan(
#   
#   #add cleaning script here, see error risk from tundra trait team as an example
# )

MyPlan <- bind_rows(ImportTRYDrakePlan, MergeTraitDrakePlan)

conf <- drake_config(MyPlan)
conf

make(MyPlan)

# Load data (sitetraits and trytraits)
loadd()

# Check all is good
drake_failed()

# View dependency graph
vis_drake_graph(MyPlan)

#Checked names! Everything w/o replacement ok
# Pulsatilla vernalis
# Cetraria islandica
# Polygonum viviparum
# Elyna myosuroides
# Vaccinium gaultherioides = Vaccinium uliginosum
# Sedum sp.
# Listera ovata = Neottia ovata
# Gentiana tenella = Gentianella tenella
# Euphrasia minima
# Nigritella nigra = Gymnadenia nigra
# Arenaria multicaulis
# Galium sp.
# Potentilla brauneana
# Anthoxantum alpinum = Anthoxanthum alpinum
# Hieracium lactucela = Pilosella lactucella
# Agrostis schraderiana = Agrostis agrostiflora
# Hieracium piloselloides
# Aster bellidiastrum
# Festuca pratense = Festuca pratensis