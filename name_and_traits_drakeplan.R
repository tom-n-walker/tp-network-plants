source("R/clean_taxonomy.R")
source("R/Name_repair.R")

taxa <- get_species()

cleaned <- resolve_species(taxa)

traits <- load_wrangle_try(cleaned)

#traits$species_only %>% View()


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