#############################
### TRANSPLANT DRAKE PLAN ###
#############################

# Load library
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("e1071")
library("DBI")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)

# source scripts
source("R/ImportCleanAndMakeList_CH_Calanda.R")
source("R/ImportCleanAndMakeList_CH_Calanda2.R")
source("R/ImportCleanAndMakeList_CH_Lavey.R")
source("R/ImportCleanAndMakeList_CN_Damxung.R")
source("R/ImportCleanAndMakeList_CN_Gongga.R")
source("R/ImportCleanAndMakeList_DE_Grainau.R")
source("R/ImportCleanAndMakeList_FR_AlpeHuez.R")
source("R/ImportCleanAndMakeList_FR_Lautaret.R")
source("R/ImportCleanAndMakeList_IN_Kashmir.R")
source("R/ImportCleanAndMakeList_NO_Norway.R")
source("R/ImportCleanAndMakeList_SE_Abisko.R")
source("R/ImportCleanAndMakeList_US_Colorado.R")
source("R/ImportCleanAndMakeList_US_Montana.R")
source("R/ImportCleanAndMakeList_IT_MatschMazia.R")
source("R/ImportCleanAndMakeList_US_Arizona.R")
source("R/ImportCleanAndMakeList_CN_Heibei.R")
source("R/Analysis_SR.R")

# Import Data
ImportDrakePlan <- drake_plan(

  NO_Ulvhaugen = ImportClean_NO_Norway(g = 1), 
  NO_Lavisdalen = ImportClean_NO_Norway(g = 2),
  NO_Gudmedalen = ImportClean_NO_Norway(g = 3),
  NO_Skjellingahaugen = ImportClean_NO_Norway(g = 4),

  CH_Lavey = ImportClean_CH_Lavey(), 
  CH_Calanda = ImportClean_CH_Calanda(), #CC
  CH_Calanda2 = ImportClean_CH_Calanda2(), #CC

  US_Colorado = ImportClean_US_Colorado(), #CC
  US_Montana = ImportClean_US_Montana(), #CC
  US_Arizona = ImportClean_US_Arizona(), 

  CN_Gongga = ImportClean_CN_Gongga(), #DE
  CN_Damxung = ImportClean_CN_Damxung(), #CC
  IN_Kashmir = ImportClean_IN_Kashmir(), #DE
  CN_Heibei = ImportClean_CN_Heibei(),

  DE_Grainau = ImportClean_DE_Grainau(), #DE
  FR_AlpeHuez = ImportClean_FR_AlpeHuez(), #DE
  SE_Abisko = ImportClean_SE_Abisko(), #CC
  FR_Lautaret = ImportClean_FR_Lautaret(), #CC
  IT_MatschMazia = ImportClean_IT_MatschMazia() #CC
)

#Make taxa vectors all a dataframe with column name 'speciesName', keep consistent across all dataframes
#destBlockID needs to be added to Norway data, Gongga, etc. destBlockID <- NA


AnalyzeDrakePlan <- drake_plan(
  CN_Gongga_lm = AnalyzeSR(CN_Gongga)
)

MyPlan <- ImportDrakePlan

#MyPlan <- bind_rows(ImportDrakePlan, AnalyzeDrakePlan)

conf <- drake_config(MyPlan)
conf
make(MyPlan, keep_going = TRUE)
loadd()
failed()
vis_drake_graph(conf, targets_only = TRUE)
vis_drake_graph(conf)


