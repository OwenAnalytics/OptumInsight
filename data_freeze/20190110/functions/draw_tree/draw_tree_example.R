##################################################
#' Program name: draw_tree_example.R
#' 
#' Description: Provide test cases and examples of using 
#' the function draw_tree.R
#' 
#' Author: Mengbing Li
#' 
#' Created: 02/03/2019
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(igraph)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/functions/draw_tree")
source("draw_tree.R")

#' draw_tree(inputRoots, textSize = 0.62, 
#' nodeColor = "lightblue", nodeSize = 18, edgeWidth = 2)

# examples:
draw_tree("342")
draw_tree(341)
draw_tree(c("342", "001"))
draw_tree("456.1")
draw_tree("3452", nodeSize = 10, textSize = 0.45)

pdf("tree_453.pdf", width = 20, height = 6)
draw_tree(453, nodeSize = 10, textSize = 0.4)
dev.off()
# test:
draw_tree(c("342", "c")) # "C" is not a valid ICD-9 code
draw_tree(c("3422")) # "3422" is not a valid ICD-9 code
draw_tree(c("342233", "324")) # "342233" is not of length 3, 4, or 5
