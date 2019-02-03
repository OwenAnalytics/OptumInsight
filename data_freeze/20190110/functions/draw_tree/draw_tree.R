##################################################
#' Program name: draw_tree.R
#' 
#' Description: The function draws tree structure of the ICD-9 codes.
#' 
#' Author: Mengbing Li
#' 
#' Created: 01/31/2019
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(igraph)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/functions/draw_tree")


# draw tree --------------------------------------------------

#' The function takes in a character vector of ICD-9 codes,
#' can take 3, 4, or 5-digit codes

draw_tree <- function(inputRoots, 
    textSize = 0.62, nodeColor = "lightblue", nodeSize = 18,
    edgeWidth = 2){
  
  # read path data
  treePaths <- readRDS("draw_tree.rds")
  
  # remove the dot if there is any
  inputRoots_noDot <- gsub("\\.", "", inputRoots)
  
  # check whether the inputs have length 3, 4, or 5 digits
  badLength <- !nchar(inputRoots_noDot) %in% 3:5
  if(any(badLength)){
    badInputRoots <- inputRoots[badLength]
    outputMessage <- paste(badInputRoots,
      "are not valid ICD-9 codes. A code should be 3, 4, or 5 digits long.")
    stop(outputMessage)
  }
  
  # check whether the inputs are valid ICD-9 codes
  allValidCodes <- gsub("\\.", "", c(treePaths$parent, treePaths$child))
  invalidInput <- inputRoots[which(!inputRoots_noDot %in% allValidCodes)]

  # find invalid inputs
  if(length(invalidInput) != 0){
    outputMessage <- paste(invalidInput, "are invalid ICD-9 codes.")
    stop(outputMessage)
  }
  
  # subset path data
  # get root
  roots <- ifelse(substr(inputRoots_noDot, 1, 1)=="E",
                 substr(inputRoots_noDot, 1, 4),
                 substr(inputRoots_noDot, 1, 3))
  treeData <- treePaths[root %in% roots,]

  # obtain root names
  inputRootNames <- unique(treeData$RootName)
  
  if(length(inputRoots) > 1)  par(ask=TRUE)
  
  for (j in 1:length(inputRoots)){
    # convert into igraph data
    codeTree <- graph.data.frame(
      treeData[root == roots[j],], directed=FALSE, vertices=NULL)
    
    # set text size in the plot
    V(codeTree)$label.cex = textSize
    
    plot(codeTree, 
         vertex.label = V(codeTree)$name, 
         vertex.label.color = "black",
         vertex.size = nodeSize,
         vertex.color = nodeColor, vertex.frame.color = "gray10",
         edge.width = edgeWidth,
         layout = layout_as_tree,
         main = paste0("Structure of ICD-9 Code ", roots[j], ": 
                       ", inputRootNames[j]))
  }
  par(ask=FALSE)
}
