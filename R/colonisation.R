#### COLONISATION PATTERNS ACROSS SITES ####

dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
    bind_rows(
      originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
      destControls = dat %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
      warmed =  dat %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
      .id = "ODT") %>%
      arrange(Region, originSiteID, turfID, Year)
  })) %>% 
  mutate(ID = map(comm, function(x) x %>% group_by(Year, ODT, destPlotID) %>% 
                    distinct(SpeciesName) %>% nest())) %>%
  unnest(ID) %>%
  group_by(Region, Year, ODT, destPlotID) %>%
  mutate(overlap = map(data, ~turnover_allyears(.x, Year, SpeciesName, RelCover, "total")))) 
  
#USE LAPPLY TO COUNT THROUGH ROWS? 

dd <- dat %>% select(Region, originSiteID, destSiteID, Treatment) %>% 
  distinct() %>% 
  filter(Treatment == "Warm") %>% 
  select(-Treatment) %>% 
  mutate(comm = pmap(.l = list(R = Region, O = originSiteID, D = destSiteID), .f = function(R, O, D){
    bind_rows(
      originControls = dat %>% filter(Region == R, destSiteID == O, Treatment == "LocalControl"),
      destControls = dat %>% filter(Region == R, destSiteID == D, Treatment == "LocalControl"),
      warmed =  dat %>% filter(Region == R, destSiteID == D, Treatment == "Warm"),
      .id = "ODT") %>%
      arrange(Region, originSiteID, turfID, Year)
  })) %>% 
  mutate(ID = map(comm, function(x) x %>% group_by(Year, ODT, destPlotID) %>% 
                    distinct(SpeciesName) %>% nest())) %>%
  unnest(ID) %>%
  group_by(Region, Year, ODT, destPlotID) %>%
  mutate(overlap = map(data, ~turnover_allyears(.x, Year, SpeciesName, RelCover, "total")))) 


turnover_allyears <- function(df, 
                              time.var, 
                              species.var, 
                              abundance.var, 
                              metric=c("total", "extinction","colonisation"))

# Initialize `filled` vector


# Roll and fill
rollit <- function(x) {
vec <- rep_along(1:length(x), list(na_value))

for(i in 1:length(x)) {
  rar   <- lapply(x, function(y) {y[(i-1):y]})
  vec[[i]] <- do.call(.f, rar)
}

# create character vectors of unique species from each df
d1spp <- as.character(unique(d1[[species.var]]))
d2spp <- as.character(unique(d2[[species.var]]))

# ID shared species
commspp <- intersect(d1spp, d2spp)

# count number not present in d2
disappear <- length(d1spp)-length(commspp)

# count number that appear in d2
appear <- length(d2spp)-length(commspp)

# calculate total richness
totrich <- sum(disappear, appear, length(commspp))
rar <- dd$overlap[1:3]
lag(unlist(rar))
setdiff(unlist(rar[2]), unlist(rar[3]))

mutate(overlap = map(data, ~intersect(.x - lag(.x))))
  
  mutate(PCA = map(comm_spp, ~rda(sqrt(.x)))) %>% 
  mutate(scores = map(.x = PCA, .f = fortify, display = "wa", axes = 1:2)) %>% 
  mutate(scores = map2(.x = comm_meta, .y = scores, bind_cols))

  dd2 <- dd %>% 
    select(-(comm:PCA)) %>% 
    unnest(scores) %>% 
    group_by(Region, originSiteID, destSiteID, ODT, Year) %>% 
    summarise_at(vars(matches("PC")), .funs = mean) %>% 
    group_by(Region, originSiteID, destSiteID, Year) %>% 
    nest() %>% 
    mutate(distances = map(data, ~dist(select(.x, matches("PC"))))) %>% 
    mutate(distances = map(distances, ~tibble(what = c("Low_High", "Low_TP", "High_TP"), dist = as.vector(.x)))) %>% 
    unnest(distances)