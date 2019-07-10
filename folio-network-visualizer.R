library(jsonlite)
library(igraph)
library(dplyr)
library(gh)
library(gdata)
library(httr)
library(threejs) 
library(htmlwidgets)
library(networkD3)

#######################################################################
#                            GitHub Utils                             #
#######################################################################

GITHUB_TOKEN = ""

getAllFolioModules <- function() {
  j_repos <- gh("/users/:username/repos", username = "folio-org", .limit = 200, .token = GITHUB_TOKEN)
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

#######################################################################
#                             Data Utils                              #
#######################################################################

getNetData = function(modules) {
  
  data = data.frame(module=character(), 
                    from_name=character(), 
                    from_version=character(), 
                    from_name_version=character(), 
                    to_name=character(), 
                    to_version=character(), 
                    to_name_version=character())
  
  for(k in 1:length(modules)) {
    
    if(startsWith(modules[[k]], "ui")) {
      print(paste("UI module", modules[[k]], sep=":"))
      descriptor <- getDataFromGitHub(paste("/folio-org/", 
                                            modules[[k]], 
                                            "/master/package.json", sep=""))
      
      ui_data = data.frame(to_name=character(), to_version=character())
      
      if(!is.null(descriptor)) {
        
        stripes = descriptor[["stripes"]]
        
        if(!is.null(stripes) & length(stripes) > 0) {
          okapi_interfaces = stripes[["okapiInterfaces"]]
          if(!is.null(okapi_interfaces) & length(okapi_interfaces) > 0) {
            to_name = names(okapi_interfaces)
            to_version = unlist(okapi_interfaces, use.names = FALSE)
            ui_data = dplyr::union(ui_data, data.frame(to_name, to_version))
          } 
        }
        
        optional_dependencies = descriptor[["optionalDependencies"]]
        if(!is.null(optional_dependencies) & length(optional_dependencies) > 0) {
          to_name = names(optional_dependencies)
          to_version = unlist(optional_dependencies, use.names = FALSE)
          ui_data = dplyr::union(ui_data, data.frame(to_name, to_version))
        } 
        
        dev_dependencies = descriptor[["devDependencies"]]
        if(!is.null(dev_dependencies) & length(dev_dependencies) > 0) {
          to_name = names(dev_dependencies)
          to_version = unlist(dev_dependencies, use.names = FALSE)
          dev = data.frame(to_name, to_version)
          dev = dplyr::filter(dev, substr(to_name, 1, 6) == "@folio")
          ui_data = dplyr::union(ui_data, dev)
        } 
        
        dependencies = descriptor[["dependencies"]]
        if(!is.null(dependencies) & length(dependencies) > 0) {
          to_name = names(dependencies)
          to_version = unlist(dependencies, use.names = FALSE)
          dep = data.frame(to_name, to_version)
          dep = dplyr::filter(dep, substr(to_name, 1, 6) == "@folio")
          ui_data = dplyr::union(ui_data, dep)
        } 
        
        ui_data$to_name_version = paste(paste(ui_data$to_name, ui_data$to_version, sep = ":"))
        
        ui_data[,"module"] = c(modules[[k]])
        ui_data[,"from_name"] = c(descriptor[["name"]]) 
        ui_data[,"from_version"] = c(descriptor[["version"]])
        ui_data[,"from_name_version"] =  paste(paste(ui_data$from_name, ui_data$from_version, sep = ":"))
        
        data = dplyr::union(data, ui_data)
      }
      
    } else if(startsWith(modules[[k]], "mod")) {
      print(paste("MOD module", modules[[k]], sep=":"))
      descriptor <- getDataFromGitHub(paste("/folio-org/", 
                                            modules[[k]], 
                                            "/master/descriptors/ModuleDescriptor-template.json", sep=""))
      
      if(!is.null(descriptor)) {
        
        provides = descriptor[["provides"]]
        
        if(!is.null(provides) & length(provides) > 0) {
          for(i in 1:nrow(provides)) {
            requires = descriptor[["requires"]]
            to_name = c()
            to_version = c()
            if(!is.null(requires) & length(requires) > 0) {
              names = requires[["id"]]
              versions = requires[["version"]]
              for(j in 1:length(names)) {
                for(ver in strsplit(versions[i], "\\s+")[[1]]) {
                  to_name[length(to_name) + 1] = names[j]
                  to_version[length(to_version) + 1] = ver
                }
              }
            } else {
              to_name[length(to_name) + 1] = c("NA")
              to_version[length(to_version) + 1] = c("NA")
            }
            mod_data = data.frame(to_name, to_version)
            mod_data$to_name_version = paste(paste(mod_data$to_name, mod_data$to_version, sep = ":"))
            
            
            mod_data[,"from_name"] = c(head(provides, nrow(provides))[["id"]][[i]]) 
            mod_data[,"from_version"] = c(head(provides, nrow(provides))[["version"]][[i]])
            mod_data[,"from_name_version"] =  paste(paste(mod_data$from_name, mod_data$from_version, sep = ":"))
            mod_data[,"module"] = c(modules[[k]])
            
            data = dplyr::union(data, mod_data)
          }
        } 
      }
    } else if(startsWith(modules[[k]], "edge")) {
      print(paste("EDGE module", modules[[k]], sep=":"))
      descriptor <- getDataFromGitHub(paste("/folio-org/", 
                                            modules[[k]], 
                                            "/master/descriptors/ModuleDescriptor-template.json", sep=""))
      if(!is.null(descriptor)) {
        requires = descriptor[["requires"]]
        if(!is.null(requires) & length(requires) > 0) {
          to_name <- descriptor[["requires"]][["id"]]
          to_version <- descriptor[["requires"]][["version"]]
          edge_data = data.frame(to_name, to_version)
          
          edge_data$to_name_version = paste(paste(edge_data$to_name, edge_data$to_version, sep = ":"))
          
          edge_data[,"from_name"] = c(descriptor[["name"]]) 
          edge_data[,"from_version"] = c("")
          edge_data[,"from_name_version"] = paste(paste(edge_data$from_name, edge_data$from_version, sep = ""))
          edge_data[,"module"] = c(modules[[k]])
          
          data = dplyr::union(data, edge_data)
        }
      }
    } else {
      # --> errors should be logged
    }
  }
  return(data)
}

getNodeTypesByNames <- function(names) {
  types = c()
  for(name in names) {
    if(startsWith(name, "@folio")) {
      types[length(types) + 1] = "UI"
    } else if(endsWith(name, "Edge API")) {
      types[length(types) + 1] = "EDGE"
    } else {
      types[length(types) + 1] = "BE"
    }
  }
  return(types);
}

getIdsByNames = function(n, rel_names) {
  result = c()
  for(r in rel_names) {
    result[length(result) + 1] = dplyr::filter(n, n$name == r)[["id"]]
  }
  return(result)
}

#######################################################################
#                          Data Processing                           #
#######################################################################

# All FOLIO Modules
data = getNetData(getAllFolioModules())

# ACQ Modules
# data = getNetData(c("ui-orders", "mod-orders", "edge-orders", "mod-invoice", "mod-oai-pmh", "edge-oai-pmh", "mod-gobi", "ui-invoice", "ui-organizations", "mod-finance", "ui-finance"))


data = dplyr::filter(data, substr(data$from_name, 1, 1) != "_")
data = dplyr::filter(data, data$to_name != "NA")
nodes=data.frame(name = dplyr::union(data$from_name, data$to_name))
nodes$id = seq.int(nrow(nodes)) - 1
nodes$group = getNodeTypesByNames(nodes$name)
nodes$title = paste0("<p><b>", nodes$name,"</b></p>")
relations = data.frame(from = data$from_name, 
                       to = data$to_name, 
                       from_id = getIdsByNames(nodes, data$from_name), 
                       to_id = getIdsByNames(nodes, data$to_name), 
                       value = c(10))

size <- table(data$from_name)
ddd = data.frame(size)

colnames(ddd) <- c("name", "size")

nodes = dplyr::left_join(nodes, ddd)
nodes$size[!is.na(nodes$size)] = 20 + 2 * nodes$size
nodes$size[is.na(nodes$size)] = 30

############################################ visNetwork ############################################
library(visNetwork)

vis_nodes = data.frame(id = nodes$id, label = nodes$name, group = nodes$group, size = nodes$size, title = nodes$title)
vis_edges = data.frame(from = relations$from_id, to = relations$to_id)

vis = visNetwork(vis_nodes, vis_edges, height = "900px", width = "100%", main = "FOLIO Modules") %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = FALSE) %>%
  visIgraphLayout() %>% 
  visLegend(width = 0.1, position = "left") %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), nodesIdSelection = TRUE, selectedBy = "group") %>%
  visLayout(randomSeed = 1234)

saveWidget(vis, file="ACQ-Network.html") 
browseURL("ACQ-Network.html")

print(vis)
