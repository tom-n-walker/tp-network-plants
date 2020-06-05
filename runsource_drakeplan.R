library("drake")
library("tidyverse")
library("vegan")
library("ggvegan")
library("readxl")
library("lubridate")




r_make(source = "TransPlant_DrakePlan.R")
loadd()

drake_failed()

#view dependency graph
r_vis_drake_graph(source = "TransPlant_DrakePlan.R", targets_only = TRUE)
