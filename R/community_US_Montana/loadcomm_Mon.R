###### US_Montana community data functions ####
#Chelsea Chisholm, 03.11.2019

load_cover_US_Montana <- function(){
  #import data
  dat13 <- read_excel('./data/US_Montana/US_Montana_commdata/Fall_2013_Cover_Data.xlsx', sheet=1)
  dat14_15 <- read_excel('./data/US_Montana/US_Montana_commdata/Montana_MIREN_trans_relevee.xlsx', sheet=1)
  
  cover13 <- dat13 %>%
    gather(SpeciesName, cover, -site, -plot, -rep) %>%
    mutate(Year=2013, Region='Montana') %>%
    rename(destSiteID=site, turfID=plot, reps=rep, Cover=cover) %>%
    mutate(Treatment = case_when(reps == "LC" & destSiteID=="low" ~ "LocalControl",
      reps == "AC" & destSiteID == "low" ~ "Warm",
      reps == "HC" & destSiteID == "low" ~ "Warm",
      reps == "AC" & destSiteID == "middle" ~ "LocalControl",
      reps == "HC" & destSiteID == "middle" ~ "Warm",
      reps == "HC" & destSiteID == "high" ~ "LocalControl",
      reps == "AC" & destSiteID == "high" ~ "Cold",
      reps == "LC" & destSiteID == "high" ~ "Cold",
      reps == "LC" & destSiteID == "middle" ~ "Cold")) %>%
    filter(Treatment %in% c('LocalControl', 'Warm', 'Cold'), !is.na(Cover)) %>%
    mutate(destSiteID=recode(destSiteID, low='Low', middle='Middle', high='High'),
           originSiteID=recode(reps, 'LC'='Low', "AC"='Middle', "HC"='High')) %>%
    select(-reps)
    
  cover14_15 <- dat14_15 %>% select(1:8) %>% #seems to be only low and middle communities here
    rename(destSiteID=Elevation, turfID=Plot, SpeciesName=Species)  %>%
    filter(!grepl("soil",Disturb_treat)) %>% #what to do about inside vs outside in column Location? removed for now
    select(-Location) %>%
    mutate(Treatment = case_when(Disturb_treat == "lowland_community" & destSiteID=="Low" ~ "LocalControl",
                                 Disturb_treat == "Alpine_community" & destSiteID == "Low" ~ "Warm",
                                 Disturb_treat == "lowland_community" & destSiteID == "Middle" ~ "Cold",
                                 Disturb_treat == "Alpine_community" & destSiteID == "Middle" ~ "Warm")) %>% #I remember Tim saying they transplanted alpine turfs downward to both middle and low. so here we are...
    filter(Treatment %in% c('LocalControl', 'Warm', 'Cold'), !is.na(Cover)) %>%
    mutate(originSiteID=recode(Disturb_treat, "lowland_community"='Low', "Alpine_community"='Alpine')) %>%
    select(-Disturb_treat)
  
  cover <- bind_rows(cover13, cover14_15)
  
  return(cover)
}
