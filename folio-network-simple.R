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

j_repos <- gh("/users/:username/repos", username = "folio-org")
vapply(j_repos, "[[", "", "name")

# INPUT ---> List of module names for processing
modules <- c("ui-orders", "ui-vendors", "mod-orders", "edge-oai-pmh", "mod-oai-pmh", "edge-orders", "mod-inventory", "mod-gobi")

nodes <- c()
type <- c()
from <- c()
to <- c()

# Core logic
for(i in 1:length(modules)) {
  
  requires <- c()
 
  
  if(startsWith(modules[[i]], "ui")) {
    print("Processing UI module")
    descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                 modules[[i]], 
                                 "/master/package.json", sep=""), flatten = TRUE)
    module <- modules[[i]]
    
    reqs <- names(descriptor[["stripes"]][["okapiInterfaces"]])
    
    for(k in 1:length(reqs)) {
      tmp <- strsplit(reqs[[k]], "[.]")[[1]]
      if(length(tmp) > 1) {
        result <- tmp[[1]]
      } else {
        result <- tmp
      }
      if(!(result %in% requires)) {
        requires[length(requires) + 1] <- result
      }
    }
  } else {
    print("Processing MOD module")
    descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                 modules[[i]], 
                                 "/master/descriptors/ModuleDescriptor-template.json", sep=""))
    
    provides <- descriptor[["provides"]]
    if(length(provides[["id"]]) == 0) {
      module <- descriptor[["name"]] 
    } else {
      module <- provides[["id"]]
    }
    
    reqs <- descriptor[["requires"]]
    
    for(k in 1:length(reqs[[1]])) {
      tmp <- strsplit(reqs[[1]][[k]], "[.]")[[1]]
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
    if(startsWith(modules[[i]], "ui")) {
      type[length(type) + 1] <- 1
    } else if(startsWith(modules[[i]], "edge")) {
      type[length(type) + 1] <- 2
    } else {
      type[length(type) + 1] <- 3
    }
  }

  
  for(j in 1 : length(requires)) {
    dependency <- paste(requires[[j]])
    to[length(to) + 1] <- dependency 
    from[length(from) + 1] <- module
    if(!(dependency %in% nodes)) {
      nodes[length(nodes) + 1] <- dependency
      if(startsWith(dependency, "ui")) {
        type[length(type) + 1] <- 1
      } else if(startsWith(dependency, "edge")) {
        type[length(type) + 1] <- 2
      } else {
        type[length(type) + 1] <- 3
      }
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
colrs <- c("gray50", "gold", "tomato")
V(net)$color <- colrs[V(net)$type]
V(net)$size <- 10
V(net)$frame.color <- "white"
V(net)$label.cex <- 0.9
V(net)$frame.color="#555555"
deg <- degree(net, mode="all")
V(net)$size <- 5*sqrt(deg)
E(net)$arrow.mode <- 0
E(net)$width <- 2

# OUPTUP <- graph
l <- layout_with_lgl(net) 
plot(net, layout=l)
legend(x=-1.5, y=-1.1, c("UI Module","EDGE Module", "Back-End Module"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
