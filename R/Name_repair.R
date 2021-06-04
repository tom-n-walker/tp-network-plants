## Script for species name matching for sites with species codes (SE_Abisko, Norway, US_Arizona)

### FUNCTION TO LOAD SE_ABISKO LOOKUP TABLE -----------------------------
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
    
### FUNCTION TO LOAD ALL NORWAY LOOKUP TABLE -----------------------------
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

return(no_dat)
}

### FUNCTION TO LOAD US_ARIZONA LOOKUP TABLE -----------------------------
load_US_Arizona_sptable <- function() {
splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(Species_FullName = paste(Genus, Species)) 
  
us_dat <- data.frame(code = splist$Code, taxa=splist$Species_FullName)

return(us_dat)
}

### FUNCTION TO MERGE ALL LOOKUP TABLES -----------------------------
merge_sptable <- function(spdat) {
sp_codes <- bind_rows(list("SE_Abisko" = spdat$se_dat, "Norway" = spdat$no_dat, "US_Arizona" = spdat$us_dat), .id="Region")

return(sp_codes)
}
