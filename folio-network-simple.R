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
library(httr)
library(threejs) 
library(htmlwidgets) 

# Get all FOLIO modules from GitHub
getAllFolioModules <- function() {
  j_repos <- gh("/users/:username/repos", username = "folio-org", .limit = 200)
  all_folio_modules <- vapply(j_repos, "[[", "", "name")
  folio_modules <-c()
  for(i in 1:length(all_folio_modules)) {
    if(startsWith(all_folio_modules[i], "ui-") || startsWith(all_folio_modules[i], "mod-") || startsWith(all_folio_modules[i], "edge-")) {
      folio_modules[[length(folio_modules) + 1]] <- all_folio_modules[[i]]
    }
  }
  return(folio_modules)
}

getDataFromGitHub <- function(path) {
  url <- paste("https://raw.githubusercontent.com", path, sep = "")
  resp <- GET(url)
  if (status_code(resp) == 200) {
    return(jsonlite::fromJSON(content(resp, "text"), flatten = TRUE))
  }
}

getNodeType <- function(name, acq_nodes) {
  if(startsWith(name, "@folio")) {
    return(1)
  } else if(endsWith(name, "Edge API")) {
    return(2)
  } else {
    return(3)
  }
}

getRelationsData <- function(modules, isVersionsEnabled) {
  mods <- c()
  reqs <- c()
  for(i in 1:length(modules)) {
    
    if(startsWith(modules[[i]], "ui")) {
      print(paste("UI module", modules[[i]], sep=":"))
      descriptor <- getDataFromGitHub(paste("/folio-org/", 
                                            modules[[i]], 
                                            "/master/package.json", sep=""))
    } else {
      print(paste("MOD module", modules[[i]], sep=":"))
      descriptor <- getDataFromGitHub(paste("/folio-org/", 
                                            modules[[i]], 
                                            "/master/descriptors/ModuleDescriptor-template.json", sep=""))
    }
    
    if(!is.null(descriptor)) {
      if(startsWith(modules[[i]], "ui")) {
        mods[[i]] <- if(isVersionsEnabled) {
          paste(descriptor[["name"]], descriptor[["version"]], sep=":")
        } else {
          descriptor[["name"]]
        }
        names <- names(descriptor[["stripes"]][["okapiInterfaces"]])
        versions <- unlist(descriptor[["stripes"]][["okapiInterfaces"]], use.names = FALSE)
      } else {
        provides <- descriptor[["provides"]]
        id <- provides[["id"]][[1]]
        version <- provides[["version"]][[1]]
        mods[[i]] <- if(length(id) > 1) {
            strsplit(id[[1]], "[.]")[[1]][[1]]
          } else if(length(id) == 0) {
            descriptor[["name"]] 
          } else {
            if(isVersionsEnabled) {
              paste(id, version, sep=":")
            } else {
              id
            }
          }
        names <- descriptor[["requires"]][["id"]]
        versions <- descriptor[["requires"]][["version"]]
      }
      if(isVersionsEnabled) {
        if(length(names) > 0) {
          reqs[[mods[[i]]]] <- paste(names, versions, sep=":")
        }
      } else {
        reqs[[mods[[i]]]] <- unlist(lapply(strsplit(as.character(names), "[.]"),"[", 1))
        reqs[[mods[[i]]]] <- unique(reqs[[mods[[i]]]])
      }
    }
  }
  return(reqs)
}

getNetData <- function(modules, isExternalDependenciesIncluded, isVersionsEnabled) {
  nodes <- c() 
  type <- c()
  is_acq <- c()
  from <- c()
  to <- c()
  data <- getRelationsData(modules, isVersionsEnabled)
  names <- names(data)
  for(j in 1:length(names)) {
    name <- names[[j]]
    dependencies <- data[[name]]
    if(!(name %in% nodes)) {
      nodes[[length(nodes) + 1]] <- name
      type[[length(type) + 1]] <- getNodeType(name)
    }
    for(k in 1:length(dependencies)) {
      if(isExternalDependenciesIncluded) {
        from[[length(from) + 1]] <- name
        to[[length(to) + 1]] <- dependencies[[k]]
        if(!(dependencies[[k]] %in% nodes)) {
          nodes[[length(nodes) + 1]] <- dependencies[[k]]
          type[[length(type) + 1]] <- getNodeType(dependencies[[k]])
        }
      } else {
        if(dependencies[[k]] %in% names) {
          from[[length(from) + 1]] <- name
          to[[length(to) + 1]] <- dependencies[[k]]
          if(!(dependencies[[k]] %in% nodes)) {
            nodes[[length(nodes) + 1]] <- dependencies[[k]]
            type[[length(type) + 1]] <- getNodeType(dependencies[[k]])
          }
        }
      }
    }
  }
  modules <- data.frame(nodes, type)
  relations <- data.frame(from, to)
  return(graph_from_data_frame(relations, directed=TRUE, vertices=modules))
}

##########################################

ACQ_MODULES <- c("mod-orders", "ui-orders", "edge-orders", "ui-vendors", "mod-vendors", "mod-organization-storage", "edge-oai-pmh", "mod-oai-pmh", "mod-gobi", "mod-organizations-storage")
FOLIO_MODULES <- getAllFolioModules()


#isExternalDependenciesIncluded = TRUE
#isVersionsEnabled = FALSE
#folio_data <- getRelationsData(FOLIO_MODULES, isVersionsEnabled)
#acq_data <- getRelationsData(ACQ_MODULES, isVersionsEnabled)
#acq_nodes_indexes <- match(names(acq_data), names(folio_data))

#ACQ NETWORK DATA
acq_net_closed <- getNetData(ACQ_MODULES, F, F)
acq_net_closed_versions <- getNetData(ACQ_MODULES, F, T)
acq_net_opened <- getNetData(ACQ_MODULES, T, F) 
acq_net_opened_versions <- getNetData(ACQ_MODULES, T, T)

# FOLIO NETWORK DATA
folio_net_closed <- getNetData(FOLIO_MODULES, F, F)
folio_net_closed_versions <- getNetData(FOLIO_MODULES, F, T)
folio_net_opened <- getNetData(FOLIO_MODULES, T, F) 
folio_net_opened_versions <- getNetData(FOLIO_MODULES, T, T)


#######################################################################
#                         Interactive graphs                          #
#######################################################################
net.js <- acq_net_opened_versions
deg <- degree(net.js, mode="all")
V(net.js)$size <- 3.5*sqrt(deg)

colrs <- c("#00ff00", "#ffd700", "#ff0000")
V(net.js)$color <- colrs[V(net.js)$type]
V(net.js)$label <- V(net.js)$name
gjs <- graphjs(net.js, main="ACQ Modules Network", 
               bg="black", 
               showLabels=T, 
               stroke=T, 
               curvature=0.1, 
               attraction=0.1, 
               repulsion=0.8, 
               opacity=0.1,
               font.main="18px Arial")

print(gjs)
saveWidget(gjs, file="ACQ-Network-versions.html") 
browseURL("FOLIO-Network-versions.html")
# Community detection (by optimizing modularity over partitions):
#clp <- cluster_optimal(net)
#class(clp)
?graphjs

#######################################################################
#                           Static graphs                             #
#######################################################################
plotNetworkGraph <- function(net) {
  
  # Graph settings
  colrs <- c("#7fff00", "gold", "tomato")
  V(net)$color <- colrs[V(net)$type]
  V(net)$size <- 10
  V(net)$frame.color <- "white"
  #V(net)$label <- NA
  V(net)$label.cex <- 0.7
  V(net)$frame.color="#555555"
  deg <- degree(net, mode="all")
  V(net)$size <- 3.5*sqrt(deg)
  E(net)$arrow.mode <- 0
  E(net)$width <- 2
  
  # OUPTUP <- graph
  l <- layout_with_lgl(net) 
  plot(net, layout=layout_with_kk)
  legend(x=-1.0, y=-1.0, c("UI Module","EDGE Module", "Back-End Module"), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
}

#lotNetworkGraph(acq_net_closed_versions)
#plotNetworkGraph(acq_net_opened)
plotNetworkGraph(acq_net_opened_versions)

#edge_density(net)
#reciprocity(net)
#dyad_census(net) # Mutual, asymmetric, and null node pairs 2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity
#diameter(net, directed=F, weights=NA)
#transitivity(net, type="global")
