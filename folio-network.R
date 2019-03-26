#####################################################################################
# FOLIO Network graph visualizer                                                    #
#
# R-script for visualization of dependencies between Folio modules dependencies 
# in the form of a network graph.
#
####################################################################################

library(jsonlite)
library(igraph)
library(dplyr)

# INPUT ---> List of module names for processing
modules <- c("mod-orders", "mod-inventory", "edge-oai-pmh", "edge-patron", "edge-orders", "mod-oai-pmh")

nodes <- c()
from <- c()
to <- c()

# Core logic
for(i in 1:length(modules)) {
  
  descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                               modules[[i]], 
                               "/master/descriptors/ModuleDescriptor-template.json", sep=""))
  
  requires <- descriptor[["requires"]]
  provides <- descriptor[["provides"]]
  
  if(length(provides[["id"]]) == 0) {
    module <- descriptor[["name"]] 
  } else {
    module <- paste(provides[["id"]], provides[["version"]], sep=":")
  }
  
  if(!(module %in% nodes)) {
    nodes[length(nodes) + 1] <- module
  }
  
  for(j in 1 : length(requires[[1]])) {
    dependency <- paste(requires[[1]][[j]], requires[[2]][[j]], sep=":")
    to[length(to) + 1] <- dependency 
    from[length(from) + 1] <- module
    if(!(dependency %in% nodes)) {
      nodes[length(nodes) + 1] <- dependency
    }
  }
}

# Data frames constructing
modules <- data.frame(nodes)
relations <- data.frame(from, to)

# Net-object constructing
net <- graph_from_data_frame(relations, directed=TRUE, vertices=modules)

# Graph settings
V(net)$size <- 10
V(net)$frame.color <- "white"
V(net)$color <- "orange"
V(net)$label.cex <- 0.9
deg <- degree(net, mode="all")
V(net)$size <- 2*deg
E(net)$arrow.mode <- 0
E(net)$width <- 2

# Layout type
l <- layout_with_lgl(net)

# OUPTUP <- graph
plot(net, layout=l)

