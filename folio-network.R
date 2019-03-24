library(jsonlite)
library(igraph)
library(dplyr)

nodes <- c()
from <- c()
to <- c()

modules <- c("mod-inventory")

for(i in 1:length(modules)) {
  descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", modules[[i]], "/master/descriptors/ModuleDescriptor-template.json", sep=""))
  requires <- descriptor[["requires"]]
  init = length(nodes)
  for (j in 1 : length(requires[[1]])) {
    from[[j + init]] <- paste(descriptor[["provides"]][["id"]], descriptor[["provides"]][["version"]], sep=":")
    to[[j + init]] <- paste(requires[[1]][[j]], requires[[2]][[j]], sep=":")
    nodes[[j + init]] <- paste(requires[[1]][[j]], requires[[2]][[j]], sep=":")
  }
  nodes[[1 + length(nodes)]] <- paste(descriptor[["provides"]][["id"]], descriptor[["provides"]][["version"]], sep=":")
}

nodes <- unique(nodes)

modules <- data.frame(nodes)
relations <- data.frame(from, to)

net <- graph_from_data_frame(relations, directed=TRUE, vertices=modules)

V(net)$size <- 10
V(net)$frame.color <- "white"
V(net)$color <- "orange"
V(net)$label.cex <- 0.8
E(net)$arrow.mode <- 0
E(net)$width <- 2
l <- layout_with_fr(net)
plot(net, layout=l)

