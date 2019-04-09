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

folio_network <- function(modules, isExternalDependenciesIncluded, isVersionsEnabled){
  nodes <- c()
  type <- c()
  from <- c()
  to <- c()
  mods <- c()
  reqs <- c()


  # Core logic
  for(i in 1:length(modules)) {
    if(startsWith(modules[[i]], "ui")) {
      descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                   modules[[i]], 
                                   "/master/package.json", sep=""), flatten = TRUE)
      if(isVersionsEnabled) {
        mods[[i]] <- paste(descriptor[["name"]], descriptor[["version"]], sep=":")
      } else {
        mods[[i]] <- descriptor[["name"]]
      }
      reqs[[i]][[1]] <- names(descriptor[["stripes"]][["okapiInterfaces"]])
      reqs[[i]][[2]] <- unlist(descriptor[["stripes"]][["okapiInterfaces"]], use.names = FALSE)
    } else {
      descriptor <- fromJSON(paste("https://raw.githubusercontent.com/folio-org/", 
                                   modules[[i]], 
                                   "/master/descriptors/ModuleDescriptor-template.json", sep=""))
      provides <- descriptor[["provides"]]
      
      if(isVersionsEnabled) {
        mods[[i]] <- if(length(provides[["id"]]) > 1) {
          strsplit(provides[["id"]][[1]], "[.]")[[1]][[1]]
        } else if(length(provides[["id"]]) == 0) {
          descriptor[["name"]] 
        } else {
          paste(provides[["id"]], provides[["version"]], sep=":")
        }
      } else {
        mods[[i]] <- if(length(provides[["id"]]) > 1) {
          strsplit(provides[["id"]][[1]], "[.]")[[1]][[1]]
        } else if(length(provides[["id"]]) == 0) {
          descriptor[["name"]] 
        } else {
          provides[["id"]]
        }
      }
      reqs[[i]] <- descriptor[["requires"]]
    }
  }
  
  
  for(i in 1:length(mods)) {
    requires <- c()
    print(reqs[[i]][[1]])
    if(!is.null(reqs) & length(reqs[[i]][[1]]) > 0) {
      if(startsWith(mods[[i]], "@folio")) {
        for(k in 1:length(reqs[[i]][[1]])) {
          tmp <- strsplit(reqs[[i]][[1]][[k]], "[.]")[[1]]
          if(isVersionsEnabled) {
            if(length(tmp) > 1) {
              result <- paste(tmp[[1]], reqs[[i]][[2]][[k]], sep=":")
            } else {
              result <- paste(tmp, reqs[[i]][[2]][[k]], sep=":")
            }
          } else {
            if(length(tmp) > 1) {
              result <- tmp[[1]]
            } else {
              result <- tmp
            }
          }
          
          if(isExternalDependenciesIncluded) {
            if(!(result %in% requires)) {
              requires[length(requires) + 1] <- result
            }
          } else {
            if(result %in% mods & !(result %in% requires)) {
              requires[length(requires) + 1] <- result
            }
          }
        }
      } else {
        for(k in 1:length(reqs[[i]][[1]])) {
            tmp <- strsplit(reqs[[i]][[1]][[k]], "[.]")[[1]]
            if(isVersionsEnabled) {
              if(length(tmp) > 1) {
                result <- paste(tmp[[1]], reqs[[i]][[2]], sep=":")
              } else {
                result <- paste(tmp, reqs[[i]][[2]], sep=":")
              }
            } else {
              if(length(tmp) > 1) {
                result <- tmp[[1]]
              } else {
                result <- tmp
              }
              
            }
             
          if(isExternalDependenciesIncluded) {
            if(!(result[[1]] %in% requires)) {
              requires[length(requires) + 1] <- result[[1]]
            }
          } else {
            if(result %in% mods & !(result %in% requires)) {
              requires[length(requires) + 1] <- result[[1]]
            }
          }
        }
      }
    }
    
    
    # Add nodes from modules
    if(!(mods[[i]] %in% nodes)) {
      nodes[length(nodes) + 1] <- mods[[i]]
      if(startsWith(modules[[i]], "ui")) {
        type[length(type) + 1] <- 1
      } else if(startsWith(modules[[i]], "edge")) {
        type[length(type) + 1] <- 2
      } else {
        type[length(type) + 1] <- 3
      }
    }
    
    # Create new nodes from dependencies
    if(length(requires) > 0) {
      for(j in 1 : length(requires)) {
        dependency <- requires[[j]]
        to[length(to) + 1] <- dependency 
        from[length(from) + 1] <- mods[[i]]
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
  V(net)$size <- if(isExternalDependenciesIncluded) {
    5*sqrt(deg)
  } else {
    10
  }
  E(net)$arrow.mode <- 0
  E(net)$width <- 2
  
  # OUPTUP <- graph
  l <- layout_with_lgl(net) 
  plot(net)
  legend(x=-1.5, y=-1.1, c("UI Module","EDGE Module", "Back-End Module"), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
}

modules <- c("mod-vendors")
folio_network(modules, TRUE, FALSE)

