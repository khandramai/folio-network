#####################################################################################
# FOLIO Network graph visualizer                                                    
#
# R-script for visualization of Folio modules dependencies 
# in the form of a network graph.
#
####################################################################################

library(jsonlite)
library(igraph)
library(dplyr)
library(gh)
library(gdata)
library(stringr)

j_repos <- gh("/users/:username/repos", username = "folio-org")
vapply(j_repos, "[[", "", "name")

# INPUT ---> List of module names for processing
modules <- c("ui-orders")

nodes <- c()
type <- c()
from <- c()
to <- c()

# Core logic
for(i in 1:length(modules)) {
  
  if(startsWith(modules[[i]], "ui")) {
    descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                 modules[[i]], 
                                 "/master/package.json", sep=""))
    module <- modules[[i]]
    reqs <- descriptor[["stripes"]][["okapiInterfaces"]]

    for(i in 1:length(reqs[[1]])) {
      requires[[i]] <- reqs[[3]][[i]]
    }
    
  } else {
    descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                 modules[[i]], 
                                 "/master/descriptors/ModuleDescriptor-template.json", sep=""))
    provides <- descriptor[["provides"]]
    req <- descriptor[["requires"]]
    
    if(length(provides[["id"]]) == 0) {
      module <- descriptor[["name"]] 
    } else {
      module <- provides[["id"]]
    }
    
    requires <- c()
    
    for(k in 1:length(req[[1]])) {
      tmp <- strsplit(req[[1]][[k]], "[.]")[[1]]
      if(length(tmp) > 1) {
        result <- tmp[[1]]
      } else {
        result <- tmp
      }
      if(!(result %in% requires)) {
        requires[length(requires) + 1] <- result
      }
    }
  }
  
  
  if(!(module %in% nodes)) {
    nodes[length(nodes) + 1] <- module
    if(startsWith(modules[[i]], "edge")) {
      type[length(type) + 1] <- 1
    } else {
      type[length(type) + 1] <- 2
    }
  }
  
  

  

  
  for(j in 1 : length(requires)) {
    dependency <- paste(requires[[j]])
    to[length(to) + 1] <- dependency 
    from[length(from) + 1] <- module
    if(!(dependency %in% nodes)) {
      nodes[length(nodes) + 1] <- dependency
      type[length(type) + 1] <- 2
    }
  }
}

# Data frames constructing
modules <- data.frame(nodes, type)
relations <- data.frame(from, to)

# Net-object constructing
net <- graph_from_data_frame(relations, directed=TRUE, vertices=modules)


# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Graph settings
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$type]
V(net)$size <- 10
V(net)$frame.color <- "white"
V(net)$label.cex <- 0.9
deg <- degree(net, mode="all")
V(net)$size <- 5*sqrt(deg)
E(net)$arrow.mode <- 0
E(net)$width <- 2

# OUPTUP <- graph
plot(net)

