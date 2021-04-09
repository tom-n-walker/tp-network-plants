#############################
### TRAIT DRAKE PLAN ########
#############################

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

# Source trait cleaning scripts
path <- c('./R/ImportTraitData/')    
source_files <- list.files(path, "\\.R$")  
map(paste0(path, source_files), source)

# Source downstream scripts
source("R/clean_taxonomy.R")
source("R/Name_repair.R")

# Import field collected trait data
ImportTraitDrakePlan <- drake_plan(
  
  # Get TRY data (cleaned) 
  taxa = get_species(),
  
  cleaned = resolve_species(taxa),
  
  traits <- load_wrangle_try(cleaned)
  
)


# Import TRY Data
TRYDrakePlan <- drake_plan(

taxa = get_species(),

cleaned = resolve_species(taxa),

traits <- load_wrangle_try(cleaned)

)

# Merge Trait Data
TRYDrakePlan <- drake_plan(
  
  taxa = get_species(),
  
  cleaned = resolve_species(taxa),
  
  traits <- load_wrangle_try(cleaned)
  
)

# Check and Clean Trait Data
TRYDrakePlan <- drake_plan(
  
  taxa = get_species(),
  
  cleaned = resolve_species(taxa),
  
  traits <- load_wrangle_try(cleaned)
  
)





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