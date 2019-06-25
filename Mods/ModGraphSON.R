newGson <- function() {
  glst <- list()
  glst$nnodes <- 0
  glst$nedges <- 0
  glst$nprops <- 0
  glst$nodelst <- list()
  glst$edgeelst <- list()
  glst$nodeNameLookup <- list()
  glst$edgeLookup <- list()
  return(glst)
}
addPropByName <- function(glst, name, key, val) {
  idx <- glst$nodeNameLookup[[name]]
  return(addPropByIdx(glst, idx, key, val))
}
addPropById <- function(glst, id, key, val) {
  idx <- glst$idlookup[[id]]
  return(addPropByIdx(glst, idx, key, val))
}
addPropByIdx <- function(glst, idx, key, val) {
  #  print(sprintf("%d %s %s",idx,key,val))
  proplst <- list()
  proplst$val <- val
  proplst$id <- glst$nprops
  glst$nprops <- glst$nprops + 1
  glst$nodelst[[idx]]$props[[key]] <- proplst
  return(glst)
}
addNode <- function(glst, nodename, nodelabel) {
  node <- list()
  node$id <- glst$nnodes + 1
  node$name <- nodename
  node$label <- nodelabel
  node$props <- list()
  node$ninn <- 0
  node$innodes <- list()
  node$inclasses <- character(0)
  node$notn <- 0
  node$otnodes <- list()
  node$otclasses <- character(0)
  glst$nodelst[[glst$nnodes + 1]] <- node
  glst$nodeNameLookup[[nodename]] <- glst$nnodes + 1
  glst$idlookup[[node$id]] <- glst$nnodes + 1
  glst <- addPropByIdx(glst, glst$nnodes + 1, "name", nodename)
  #  glst <- addPropByName(glst,nodename,"namename",nodename)
  #  glst <- addPropById(glst,node$id,"nameid",nodename)
  glst$nnodes <- glst$nnodes + 1
  return(glst)
}

getNodeByName <- function(glst, nodename) {
  idx <- glst$nodeNameLookup[[nodename]]
  if (is.null(idx)) {
    msg <- sprintf("%s does not exist in getNodeByName", nodename)
    stop(msg)
  }
  return(idx)
}

getEdgeKey <- function(nodefrName, nodetoName, class) {
  ekey <- sprintf("%s||%s||%s", nodefrName, nodetoName, class)
  return(ekey)
}

edgeExists <- function(glst, nodefrName, nodetoName, class) {
  ekey <- getEdgeKey(nodefrName, nodetoName, class)
  eidx <- glst$edgeLookup[[ekey]]
  rv <- TRUE
  if (is.null(eidx)) rv <- FALSE
  # print(sprintf("edgeExists (%s):%s",ekey,rv))
  return(rv)
}

addEdge <- function(glst, nodefrName, nodetoName, class, dupbehavior = "quiet") {
  if (edgeExists(glst, nodefrName, nodetoName, class)) {
    if ("warn" %in% dupbehavior) {
      msg <- sprintf("Duplicated edge from %s to %s of class %s", nodefrName, nodetoName, class)
    }
    if ("stop" %in% dupbehavior) {
      stop(msg)
    }
    return
  }
  fridx <- getNodeByName(glst, nodefrName)
  toidx <- getNodeByName(glst, nodetoName)
  edge <- list()
  edge$fridx <- fridx
  edge$toidx <- toidx
  edge$class <- class
  innode <- glst$nodelst[[fridx]]
  otnode <- glst$nodelst[[toidx]]
  edgeid <- glst$nedges + 1
  glst$nodelst[[fridx]]$otnodes[[innode$notn + 1]] <- list(id = edgeid, idx = toidx, class = class)
  glst$nodelst[[toidx]]$innodes[[otnode$ninn + 1]] <- list(id = edgeid, idx = fridx, class = class)
  glst$nodelst[[fridx]]$notn <- innode$notn + 1
  glst$nodelst[[toidx]]$ninn <- otnode$ninn + 1
  #  glst$nodelst[[fridx]]$inclasses <- union(innode$inclasses, class)
  glst$nodelst[[fridx]]$otclasses <- union(innode$otclasses, class)
  glst$nodelst[[toidx]]$inclasses <- union(otnode$inclasses, class)
  edgekey <- getEdgeKey(nodefrName, nodetoName, class)
  glst$edgeLookup[[edgekey]] <- glst$nedges + 1
  #  glst$nodelst[[toidx]]$otclasses <- union(otnode$classes, class)
  glst$edgelst[[glst$nedges + 1]] <- edge
  glst$nedges <- glst$nedges + 1
  return(glst)
}
getInOutNodeLine <- function(direction, node) {
  if (direction == "in") {
    ntodo <- node$ninn
    classes <- node$inclasses
    lnodes <- node$innodes
    ioWord <- "outV"
  } else {
    ntodo <- node$notn
    classes <- node$otclasses
    lnodes <- node$otnodes
    ioWord <- "inV"
  }
  #print(sprintf("getInOutNodeLine dir:%s ntodo:%d",direction,ntodo))
  if (ntodo == 0) return("")
  l <- ""
  nclasswrit <- 1
  for (class in classes) {
    if (nclasswrit > 1) {
      l <- sprintf('%s,', l) # add a comma ... sometimes
    }
    l <- sprintf('%s "%s":[', l, class)
    nwrit <- 1
    for (i in 1:ntodo) {
      lnode <- lnodes[[i]]
      idxid <- lnode$id
      idx <- lnode$idx
      lclass <- lnode$class
      # print(sprintf("%s lclass:%s",direction,lclass))
      if (lclass == class) {
        if (nwrit > 1) {
          l <- sprintf('%s,', l) # add a comma ... sometimes
        }
        l <- sprintf('%s { "id":%s, "%s":%s }', l, idxid, ioWord, idx)
        nwrit <- nwrit+1
      }
    }
    l <- sprintf('%s ]', l)
    nclasswrit <- nclasswrit+1
  }
  return(l)
}
getNodePropLine <- function(props) {
  if (length(props) == 0) return("")
  l <- ""
  for (i in 1:length(props)) {
    pname <- names(props)[i]
    proplst <- props[[i]]
    pval <- proplst$val
    pid <- proplst$id
    propcnt <- propcnt + 1
    if (i > 1) {
      l <- sprintf('%s,', l) # add a comma ... sometimes
    }
    l <- sprintf('%s "%s":[{ "id":%s, "value":"%s" }]', l, pname, pid, pval)
  }
  return(l)
}
getNodeLine <- function(node) {
  id <- node$id
  lab <- node$label
  pl <- getNodePropLine(node$props)
  il <- getInOutNodeLine("in", node)
  ol <- getInOutNodeLine("out", node)
  l <- sprintf('{ "id":%d, "label":"%s", "inE":{%s}, "outE":{%s}, "properties":{%s} }', id, lab, il, ol, pl)
  return(l)
}
writeGsonFile <- function(glst, fname) {
  propcnt <<- 0
  llst <- list()
  nl <- 0
  if (glst$nnode > 0) {
    for (i in 1:glst$nnode) {
      llst[[length(llst) + 1]] <- getNodeLine(glst$nodelst[[i]])
    }
  }
  lvek <- unlist(llst)
  writeLines(lvek, fname)
  print(sprintf("Wrote %d lines to %s", length(lvek), fname))
}

