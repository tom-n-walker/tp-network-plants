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
source("R/ImportCleanAndMakeList_IT_MatschMazia.R")
source("R/ImportCleanAndMakeList_IN_Kashmir.R")
source("R/ImportCleanAndMakeList_NO_Norway.R")
source("R/ImportCleanAndMakeList_SE_Abisko.R")
source("R/ImportCleanAndMakeList_US_Colorado.R")
source("R/ImportCleanAndMakeList_US_Montana.R")


# Import Data
ImportDrakePlan <- drake_plan(

  NO_Ulvhaugen = ImportClean_NO_Norway(g = 1),
  NO_Lavisdalen = ImportClean_NO_Norway(g = 2),
  NO_Gudmedalen = ImportClean_NO_Norway(g = 3),
  NO_Skjellingahaugen = ImportClean_NO_Norway(g = 4), #DE check data with Aud

  CH_Lavey = ImportClean_CH_Lavey(), 
  CH_Calanda = ImportClean_CH_Calanda(),
  CH_Calanda2 = ImportClean_CH_Calanda2(),

  US_Colorado = ImportClean_US_Colorado(),
  US_Montana = ImportClean_US_Montana(),
  #US_Arizona, not in % cover so need to convert after using traits? #DE

  CN_Gongga = ImportClean_CN_Gongga(),
  CN_Damxung = ImportClean_CN_Damxung(),
  IN_Kashmir = ImportClean_IN_Kashmir(),
  #Insert Heibei data (from Wang) #DE

  DE_Grainau = ImportClean_DE_Grainau(),
  FR_AlpeHuez = ImportClean_FR_AlpeHuez(),
  SE_Abisko = ImportClean_SE_Abisko(),
  FR_Lautaret = ImportClean_FR_Lautaret(), #Follow up with her on 2018 data (still waiting...)
  IT_MatschMazia = ImportClean_IT_MatschMazia 
)


source("R/Analysis_SR.R")
source("R/plots.R")
AnalyzeDrakePlan <- drake_plan(
  #create data.list, run 'collatedata()'
  ##can you use ImportDrakePlan$target to add targest to data.list (get(ImportDrakePlan$target))
  alldat = list(NO_Gudmedalen, NO_Ulvhaugen, NO_Skjellingahaugen, NO_Lavisdalen, CH_Lavey, CH_Calanda, CH_Calanda2,
                 US_Colorado, US_Montana, CN_Gongga, CN_Damxung, IN_Kashmir, DE_Grainau, FR_AlpeHuez, SE_Abisko, FR_Lautaret, IT_MatschMazia), 
  names(alldat) = c('NO_Gudmedalen', 'NO_Ulvhaugen', 'NO_Skjellingahaugen', 'NO_Lavisdalen', 'CH_Lavey', 'CH_Calanda', 'CH_Calanda2',
                    'US_Colorado', 'US_Montana', 'CN_Gongga', 'CN_Damxung', 'IN_Kashmir', 'DE_Grainau', 'FR_AlpeHuez', 'SE_Abisko', 'FR_Lautaret', 'IT_MatschMazia'),
  SR = AnalyzeSR(alldat)
)

#insert manuscript/presentation drake plan, can have several manuscripts building off same data
MyPlan <- bind_rows(ImportDrakePlan)
conf <- drake_config(MyPlan)
make(MyPlan)
loadd()
vis_drake_graph(conf, targets_only = TRUE)


