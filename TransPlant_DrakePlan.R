#############################
### TRANSPLANT DRAKE PLAN ###
#############################

# Load library
library("drake")
library("tidyverse")
library("readxl")
library("lubridate")
library("e1071")

# drake configurations
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# trick
pn <- . %>% print(n = Inf)

# source scripts
source("R/ImportCleanAndMakeList_CN_Gongga.R")
source("R/ImportCleanAndMakeList_NO_Norway.R")
source("R/ImportCleanAndMakeList_US_Colorado.R")
source("R/ImportCleanAndMakeList_CH_Lavey.R")
source("R/ImportCleanAndMakeList_CH_Calanda.R")
source("R/ImportCleanAndMakeList_IN_Kashmir.R")
source("R/ImportCleanAndMakeList_CN_Damxung.R")
source("R/ImportCleanAndMakeList_DE_Grainau.R")
source("R/ImportCleanAndMakeList_FR_AlpeHuez.R")
source("R/Analysis_SR.R")

# Import Data
ImportDrakePlan <- drake_plan(
  CN_Gongga = ImportClean_CN_Gongga(),
  
  NO_Ulvhaugen = ImportClean_NO_Norway(g = 1),
  NO_Lavisdalen = ImportClean_NO_Norway(g = 2),
  NO_Gudmedalen = ImportClean_NO_Norway(g = 3),
  NO_Skjellingahaugen = ImportClean_NO_Norway(g = 4),
  
  US_Colorado = ImportClean_US_Colorado(),
  
  CH_Lavey = ImportClean_CH_Lavey(),
  CH_Calanda = ImportClean_CH_Calanda(),
  
  IN_Kashmir = ImportClean_IN_Kashmir(),
  
  CN_Damxung = ImportClean_CN_Damxung(),
  
  DE_Grainau = ImportClean_DE_Grainau(),
  
  FR_AlpeHuez = ImportClean_FR_AlpeHuez()
)

AnalyzeDrakePlan <- drake_plan(
  CN_Gongga_lm = AnalyzeSR(CN_Gongga)
)

MyPlan <- ImportDrakePlan

#MyPlan <- bind_rows(ImportDrakePlan, AnalyzeDrakePlan)

conf <- drake_config(MyPlan)
make(MyPlan)
loadd()
vis_drake_graph(conf, targets_only = TRUE)
