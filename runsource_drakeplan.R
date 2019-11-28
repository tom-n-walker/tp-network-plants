library("drake")

r_make(source = "TransPlant_DrakePlan.R")

failed()

#view dependency graph
r_vis_drake_graph(source = "TransPlant_DrakePlan.R", targets_only = TRUE)