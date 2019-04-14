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

j_repos <- gh("/users/:username/repos", username = "folio-org", .limit = 200)
all_folio_modules <- vapply(j_repos, "[[", "", "name")

folio_modules <-c()

for(i in 1:length(all_folio_modules)) {
  if(startsWith(all_folio_modules[i], "ui-") || startsWith(all_folio_modules[i], "mod-") || startsWith(all_folio_modules[i], "edge-")) {
    folio_modules[[length(folio_modules) + 1]] <- all_folio_modules[[i]]
  }
}

ACQ_MODULES <- c("ui-orders", "edge-orders", "mod-orders", "ui-vendors", "mod-vendors", "edge-oai-pmh", "mod-oai-pmh", "mod-gobi")
#j_repos <- gh("/users/:username/repos", username = "folio-org")
#modules <- vapply(j_repos, "[[", "", "name")

getDataFromGitHub <- function(path) {
  url <- modify_url("https://raw.githubusercontent.com", path = path)
  resp <- GET(url)
  if (status_code(resp) == 200) {
    return(jsonlite::fromJSON(content(resp, "text"), flatten = TRUE))
  }
}

print(getDataFromGitHub(paste("/folio-org/", 
                              "ui-invoice", 
                              "/master/package.json", sep="")))

getNodeType <- function(name) {
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
        
        mods[[i]] <- if(isVersionsEnabled) {
          if(length(provides[["id"]]) > 1) {
            strsplit(provides[["id"]][[1]], "[.]")[[1]][[1]]
          } else if(length(provides[["id"]]) == 0) {
            descriptor[["name"]] 
          } else {
            paste(provides[["id"]], provides[["version"]], sep=":")
          }
        } else {
          if(length(provides[["id"]]) > 1) {
            strsplit(provides[["id"]][[1]], "[.]")[[1]][[1]]
          } else if(length(provides[["id"]]) == 0) {
            descriptor[["name"]] 
          } else {
            provides[["id"]]
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

getNetData <- function(data, isExternalDependenciesIncluded, isVersionsEnabled) {
    nodes <- c() 
    type <- c()
    from <- c()
    to <- c()
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

getNetGraph <- function(modules, isExternalDependenciesIncluded, isVersionsEnabled) {
  data <- getRelationsData(modules, isVersionsEnabled)
  graph <- getNetData(data, isExternalDependenciesIncluded, isVersionsEnabled)
  return(graph)
}

# Community detection (by optimizing modularity over partitions):
#clp <- cluster_optimal(net)
#class(clp)

plotNetworkGraph <- function(modules, isExternalDependenciesIncluded, isVersionsEnabled) {
  net <- getNetGraph(modules, isExternalDependenciesIncluded, isVersionsEnabled)
  # Graph settings
  colrs <- c("gray50", "gold", "tomato")
  V(net)$color <- colrs[V(net)$type]
  V(net)$size <- 10
  V(net)$frame.color <- "white"
  V(net)$label <- NA
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
  l <- layout_with_kk(net) 
  plot(net)
  legend(x=-1.0, y=-1.0, c("UI Module","EDGE Module", "Back-End Module"), pch=21,
         col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
}

plotNetworkGraph(folio_modules, TRUE, TRUE)
#plotNetworkGraph(ACQ_MODULES, TRUE, TRUE)


  
