### COLLATE SPECIES LOOKUP TABLES FROM THREE SITES ---------------------------------------------
## Script for species name matching for sites with species codes (SE_Abisko, Norway, US_Arizona)

## SE_Abisko
load_SE_Abisko_sptable <- function() {
  files <- list.files("data/SE_Abisko/SE_Abisko_commdata/") %>% 
    grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)
  splist <- map_df(files, ~ read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", .), sheet = "Species list 2012"))
  taxa <- c(splist$Name...2, splist$Name...4)
  code <- c(splist$Code...1, splist$Code...3)
  df <- data.frame(code=code, taxa=taxa)
  df <- df[complete.cases(df),]
  se_dat <- df[!df$taxa %in% c('Lichen', 'Litter', 'Moss'),] 
  
  return(se_dat)
}

## Norway
# source code needs to be outside of the function to avoid self invalidation? 
source("R/ImportData/community_NO_Norway/loadCover.r")

load_Norway_sptable <- function() {
  con <- src_sqlite(path = file_in("data/NO_Norway/seedclim.sqlite"), create = FALSE)
  #get taxonomy table
  taxa <- tbl(con, "taxon") %>%
    collect() 
  taxa <- unique(taxa$speciesName)
  no <- str_split_fixed(taxa, ' ', 2)
  no[,2] <- str_replace_all(no[,2], c('sp?'='sp', 'sp.'='sp', 'sp??'='sp', 'sp1?'='sp',
                                      'spp'='sp', 'spp.'='sp', 'spp1?'='sp'))
  no[,2] <- gsub('[[:punct:] ]+','',no[,2])
  code <- paste(substr(no[,1], 1, 3), substr(no[,2], 1, 3), sep='.')  
  no_dat <- data.frame(code=code, taxa=taxa)
  no_dat <- no_dat %>% 
    mutate(code = recode(code, Aco.spe = "Aco.sep", Car.spx = "Car.sax", Car.car = "Caru.car", 
                         Gal.spx = "Gal.sax", Ger.spl = "Ger.syl", Hup.spl = "Hup.sel", Kob.spm = "Kob.sim",
                         Luz.spc = "Luz.spi", Mel.spl = "Mel.syl", Nar.spr = "Nar.str", Oma.spn = "Oma.sup",
                         Oma.spl = "Oma.syl", Pim.spx = "Pim.sax", Pin.spl = "Pin.syl", Sel.spl = "Sel.sel",
                         Val.spm = "Val.sam", Ver.spr = "Ver.ser")) %>%
    mutate(code = if_else(code == "Hyp.mac" & taxa == "Hypericum maculatum", "Hype.mac", 
                           if_else(code == "Hyp.mac" & taxa == "Hypochaeris maculata", "Hypo.mac", code))) %>%
    mutate(code = if_else(code == "Rum.ace" & taxa == "Rumex acetosella", "Rum.acl", code))
    
  return(no_dat)
}

##Arizona
load_US_Arizona_sptable <- function() {
  splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(Species_FullName = paste(Genus, Species)) 
  
  us_dat <- data.frame(code = splist$Code, taxa=splist$Species_FullName)
  
  return(us_dat)
}

##Function to merge all
merge_sptable <- function(spdat) {
  sp_codes <- bind_rows(list("SE_Abisko" = spdat$se_dat, "Norway" = spdat$no_dat, "US_Arizona" = spdat$us_dat), .id="Region") %>%
    filter(!taxa %in% c(NA, "NA NA")) %>%
    group_by(taxa) %>%
    nest(.key  = "codes")
  
  return(sp_codes)
}