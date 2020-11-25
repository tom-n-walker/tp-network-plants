#Source downstream scripts
source("R/clean_taxonomy.R")

# Import Data
TraitDrakePlan <- drake_plan(
  
taxa <- get_species()

resolve_species <- resolve_species(taxa)

)
