library(XML)
library(plyr)
library(scales)
library(tidyverse)
library(stringr)

##############

# easyxml.r provides several functions for taking XML organized with any arbitrary schema
# and turning it into as many tabular files as there are xml node types. Every node has
# a unique ID issued by these functions, and they can be merged and re-merged at will.
# The main function here is read.file.

##############

########## Functions ##########

###############
# Function: 
###############
recurse <- function(xml.root, frames, up.level.ids, i) {
  
  # node type
  node.type <- xmlName(xml.root) # node name which will be the dataframe title
  
  # this will be a list of IDs that is passed from the previous level up
  current.ids <- up.level.ids
  current.id.name <- paste0("xml_key_", node.type)
  current.ids[[current.id.name]] <- i
  
  # if a dataframe for this node doesn't exist, create it
  if (node.type %in% names(frames)) {
    # do nothing
  } else {
    frames[[node.type]] <- data.frame()
  }
  
  # add this parent node to its relevant list
  frames[[node.type]] <- node.to.node.list(xml.root, frames[[node.type]], current.ids)
  
  num.nodes = xmlSize(xml.root) # number of nodes in current node
  
  if (num.nodes > 0) {
    for (i in 1:num.nodes) {
      current.node <- xml.root[[i]]
      frames <- recurse(current.node, frames, current.ids, i)
    }
  }
  return(frames)
}

###############
# Function: 
###############
node.to.node.list <- function(xml.root, list.of.nodes, current.ids) {
  node.type.name <- xmlName(xml.root) # node name which will be the dataframe title
  attributes <- xmlAttrs(xml.root) # attribute values
  attributes.w.ids <- c(attributes, current.ids)
  
  if(is.null(attributes)) {
    return(list.of.nodes)
  }
  
  attribute.names <- names(attributes) # attribute names
  frame.cols <- colnames(list.of.nodes)
  list.of.nodes <- rbind.fill(list.of.nodes, data.frame(t(attributes.w.ids)))
  return(list.of.nodes)
}

###############
# Function: 
###############
read.file <- function(filename) {
  frames <- list()
  xml.file <- xmlParse(file=filename)
  xml.root = xmlRoot(xml.file)
  frames <- recurse(xml.root, frames, list(), 0) # this is the real meat
  return(frames)
}


