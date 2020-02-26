library("drake")
library("tidyverse")
library("vegan")
library("ggvegan")
library("readxl")




r_make(source = "TransPlant_DrakePlan.R")
loadd()

failed()

#view dependency graph
r_vis_drake_graph(source = "TransPlant_DrakePlan.R", targets_only = TRUE)
