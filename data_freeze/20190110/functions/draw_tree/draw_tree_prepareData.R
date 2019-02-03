##################################################
#' Program name: draw_tree.R
#' 
#' Description: process data for use in draw_tree.R
#' 
#' Author: Mengbing Li
#' 
#' Created: 02/01/2019
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/functions/draw_tree")

codeList <- readRDS("../../icd9_CompleteCodeList.rds")


### build individual tree structure data ------------------------------------

# extract nodes in three levels and add a dot after three digits
source("../icd9_hierarchy.R")
codeList$root <- icd9_3digits(codeList, code)
codeList$icd9_4digits <- icd9_4digits(codeList, code, dot = TRUE)
codeList$icd9_5digits <- icd9_5digits(codeList, code, dot = TRUE)


# obtain all roots
diagRoots <- unique(codeList$root) ### Change to input roots ###

treePaths <- c()
# iterate over each root to create the corresponding tree
for(currentDiagRoots in diagRoots){
  subDiagData <- codeList[root == currentDiagRoots,]
  
  # convert path representation to network representation
  currentDiagNetwork <- unique(subDiagData[, c("root", "icd9_4digits")])
  colnames(currentDiagNetwork) <- c("parent", "child")
  if (any(currentDiagNetwork$child != "")) { # if a level-4 code is present
    # obtain edges between level 4 and level 5
    diagNetwork45 <- unique(subDiagData[icd9_5digits != "", c("icd9_4digits", "icd9_5digits")])
    if (nrow(diagNetwork45) != 0) { # if edges are present
      colnames(diagNetwork45) <- c("parent", "child")
      currentDiagNetwork <- rbind(currentDiagNetwork, diagNetwork45)
    }
  }
  currentDiagNetwork <- currentDiagNetwork[order(parent, child)]
  
  treePaths <- rbind(treePaths, currentDiagNetwork)
}


treePaths$root <- icd9_3digits(treePaths, parent)


# add name to the root --------------------------------------
# obtain major names of the 3-digit codes
load("icd9ChaptersMajor.RData")

namedRoots <- stack(icd9ChaptersMajor)
colnames(namedRoots) <- c("root", "RootName")

treePaths <- merge(x = treePaths, y = namedRoots,
                  by = "root", all.x = TRUE)
treePaths <- treePaths[, c("parent", "child", "root", "RootName")]
treePaths$RootName <- as.character(treePaths$RootName)


saveRDS(treePaths, file = "draw_tree.rds")
