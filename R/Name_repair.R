## Script for species name matching

#SE_Abisko
files <- list.files("data/SE_Abisko/SE_Abisko_commdata/") %>% 
grep(pattern = "^~", x = ., value = TRUE, invert = TRUE)
splist <- map_df(files, ~ read_excel(paste0("data/SE_Abisko/SE_Abisko_commdata/", .), sheet = "Species list 2012"))
taxa <- c(splist$Name...2, splist$Name...4)
code <- c(splist$Code...1, splist$Code...3)
df <- data.frame(code=code, taxa=taxa)
df <- df[complete.cases(df),]
se_dat <- df[!df$taxa %in% c('Lichen', 'Litter', 'Moss'),] 
    
#Norway all
source("R/ImportData/community_NO_Norway/loadCover.r")
con <- src_sqlite(path = file_in("data/NO_Norway/seedclim.sqlite"), create = FALSE)
#get taxonomy table
ImportTaxa_NO_Norway <- function(con){
  
  # need to move all code to dplyr for consistancy
  #get taxonomy table
  taxa <- tbl(con, "taxon") %>%
    collect() 
  taxa <- unique(taxa$speciesName)
  
  return(taxa)
}

no_taxa <- ImportTaxa_NO_Norway(con) 
no <- str_split_fixed(no_taxa, ' ', 2)
no[,2] <- str_replace_all(no[,2], c('sp?'='sp', 'sp.'='sp', 'sp??'='sp', 'sp1?'='sp',
                          'spp'='sp', 'spp.'='sp', 'spp1?'='sp'))
no[,2] <- gsub('[[:punct:] ]+','',no[,2])
code <- paste(substr(no[,1], 1, 3), substr(no[,2], 1, 3), sep='.')  
no_dat <- data.frame(code=code, taxa=no_taxa)

# Arizona

splist <- read_excel(file_in("data/US_Arizona/US_Arizona_commdata/Arizona community data & Climate data_TransplantNET_Rubin & Hungate 2019.xlsx"), sheet = "Species List ") %>%
    mutate(Species_FullName = paste(Genus, Species)) 
  
us_dat <- data.frame(code = splist$Code, taxa=splist$Species_FullName)


sp_codes <- rbind(se_dat, no_dat, us_dat)
