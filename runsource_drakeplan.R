#### Run drake plan for Transplant project ####
## C. Chisholm, chelsea.chisholm@usys.ethz.ch

# Load libraries
library("drake")
library("tidyverse")
library("vegan")
library("ggvegan")
library("readxl")
library("lubridate")
library("e1071")
library("DBI")
library("RSQLite")
library("visNetwork")

# Source drakeplan
r_make(source = "TransPlant_DrakePlan.R")

# Load data (for now, combined abundance data for all sites, dat)
loadd()

# Check all is good
drake_failed()

# View dependency graph
r_vis_drake_graph(source = "TransPlant_DrakePlan.R", targets_only = TRUE)
