library(png)
library(rjson)
library(plotrix)
library(scales)


source("mods/ModFpObjs.R")
source("mods/ModFpReadWrite.R")
source("mods/ModFloorPlot.R")
source("mods/ModGeom.R")
source("mods/ModGraphSON.R")
options(warn=2) # warn=1 is stop on warning


fpname <- "images/DE-BHO-4-reducedTo33pct.png"
fpxlim <- c(39.015, -43.448)  # full view
fpylim <- c(-22.3, 25.63)
fpxlim <- c(10, -43.448) # right part
fpylim <- c(-10.0, 25.0)

# Read and write
fpln <- readFloorPlanJson("ObjData/cur_gen_bho_x.json")
tracks <- readLogs()

aname <- "ObjData/AirFlow.csv"
ename <- "ObjData/Electric.csv"
rv <- readBldSystems(aname,ename,fpln$rdf)
glb <- list()
glb$airdf <- rv$airdf
glb$elcdf <- rv$elcdf

circrad <- 0.25
circdelt <- 1
pcircdf <- genprobcircs(fpxlim, fpylim, circdelt)
pcircdf <- filterOutOutside(pcircdf, fpln$wdf, c("OutsideBound"), c("SlotWall"), fpln$ptsdflist)
pcircdf <- filterInRooms(pcircdf, fpln$rdf, fpln$ptsdflist)

# Now do the plot
fpp <- list()

selected <- c("nbitmap", "grid", "nodes", "links", "tracks", "rooms", "walls", "edlin", "pcircs", "airflow", "electric")
selected <- c("nbitmap", "grid", "rooms", "walls", "airflow", "electric", "elnames", "afnames")
selected <- c("nbitmap", "grid", "rooms", "walls", "airflow", "electric", "elnames", "afnames","afintvav","afmduct","affduct","elsource","elmcond","elfcond")
fpp <- list()
fpp$imm <- list()
if ("bitmap" %in% selected) {
  fpp$imm$img <- readPNG(fpname)
}
if ("grid" %in% selected) {
  fpp$imm$grid <- TRUE
}
fpp$imm$xlim <- fpxlim
fpp$imm$ylim <- fpylim
fpp$imm$zoomxlim <- fpp$imm$xlim
fpp$imm$zoomylim <- fpp$imm$ylim
fpp$imm$framewid <- 1
fpp$imm$framecolor <- "green"
if ("nodes" %in% selected) {
  fpp$nodes <- list()
  fpp$nodes$ndf <- fpln$ndf
  fpp$nodes$color <- "blue"
}
if ("links" %in% selected) {
  fpp$links <- list()
  fpp$links$ldf <- fpln$ldf
  fpp$links$color <- "pink"
  fpp$links$width <- 1
}
if ("tracks" %in% selected) {
  fpp$tracks <- tracks
  fpp$tracks$width <- 1
}
if ("rooms" %in% selected) {
  fpp$rooms$rdf <- fpln$rdf
  fpp$rooms$ptsdflist <- fpln$ptsdflist
  fpp$rooms$width <- 2
  fpp$rooms$color <- "purple"
}
if ("walls" %in% selected) {
  fpp$walls$wdf <- fpln$wdf
  fpp$walls$ptsdflist <- fpln$ptsdflist
  fpp$walls$width <- 2
  fpp$walls$color <- "blue"
}
if ("loccircs" %in% selected) {
  #pcircdf <- data.frame(x=c(0,1),y=c(0,1),prob=0.5)
  fpp$pcircs$ldf <- pcircdf
  fpp$pcircs$rad <- circrad
  fpp$pcircs$color <- "pink"
}
if ("airflow" %in% selected) {
  #pcircdf <- data.frame(x=c(0,1),y=c(0,1),prob=0.5)
  fpp$airflow$adf <- glb$airdf
  fpp$airflow$width <- 3
  fpp$airflow$color <- "green"
  fpp$airflow$colortxt <- "darkgreen"
  fpp$airflow$colorfill <- "blue"
  fpp$airflow$cex <- 0.6
  fpp$airflow$donames <- "afnames" %in% selected
  fpp$airflow$domconds <- "afmduct" %in% selected
  fpp$airflow$dofconds <- "affduct" %in% selected
  fpp$airflow$dovents <- "afvents" %in% selected
  fpp$airflow$dovavs <- "afintvav" %in% selected
  fpp$airflow$dointakes <- "afintvav" %in% selected
}
if ("electric" %in% selected) {
  #pcircdf <- data.frame(x=c(0,1),y=c(0,1),prob=0.5)
  fpp$electric$edf <- glb$elcdf
  fpp$electric$width <- 3
  fpp$electric$color <- "red"
  fpp$electric$colortxt <- "darkred"
  fpp$electric$colorfill <- "yellow"
  fpp$electric$cex <- 0.6
  fpp$electric$donames <- "elnames" %in% selected
  fpp$electric$dosource <- "elmsource" %in% selected
  fpp$electric$domconds <- "elmcond" %in% selected
  fpp$electric$dofconds <- "elfcond" %in% selected
  fpp$electric$dojuncs <- "eljuncs" %in% selected
}
#if ("edlin" %in% selected) {
#fpp$edlin <- list()
#fpp$edlin$pts <- edlinrv$pts
#fpp$edlin$color <- "blue"
#fpp$edlin$width <- 1
#}
makeFloorPlot(fpp)


addAirFlowNodes <- function(glst, airdf) {
  nadf <- nrow(airdf)
  lnkclass <- "airFlow"
  if (nadf <= 0) return
  for (i in 1:nadf) {
    nname <- airdf$name[i]
    class <- airdf$class[i]
    len <- airdf$len[i]
    watts <- airdf$powerwatts[i]
    glst <- addNode(glst, nname, class)
    glst <- addPropByName(glst, nname, "len", len)
    glst <- addPropByName(glst, nname, "watts", watts)
  }
  # all the nodes have to be added before we can add edges
  for (i in 1:nadf) {
    nname <- airdf$name[i]
    uname <- airdf$upstream[i]
    dname <- airdf$downstream[i]
    if (uname != "") {
      glst <- addEdge(glst, uname, nname, lnkclass)
    }
    if (dname != "") {
      glst <- addEdge(glst, nname, dname, lnkclass)
    }
  }
  return(glst)
}

addElectricNodes <- function(glst, eledf) {
  nedf <- nrow(eledf)
  lnkclass1 <- "powers"
  lnkclass2 <- "poweredby"
  if (nedf <= 0) return
  for (i in 1:nedf) {
    nname <- eledf$name[i]
    class <- eledf$class[i]
    len <- eledf$len[i]
    watts <- eledf$powerwatts[i]
    glst <- addNode(glst, nname, class)
    glst <- addPropByName(glst, nname, "len", len)
    #glst <- addPropByName(glst, nname, "watts", watts)
  }
  # all the nodes have to be added before we can add edges
  for (i in 1:nedf) {
    nname <- eledf$name[i]
    uname <- eledf$upstream[i]
    dname <- eledf$downstream[i]
    if (uname != "") {
      glst <- addEdge(glst, uname, nname, lnkclass1)
    #  glst <- addEdge(glst, nname, uname, lnkclass2)
    }
    if (dname != "") {
      glst <- addEdge(glst, nname, dname, lnkclass1)
    #  glst <- addEdge(glst, dname, nname, lnkclass2)
    }
  }
  return(glst)
}


addElectricAirFlowLinks <- function(glst, airdf, eledf) {
  nadf <- nrow(airdf)
  lnkclass1 <- "powers"
  lnkclass2 <- "poweredby"
  if (nadf <= 0) return
#        main_eledf <- eledf[!grepl("\\.",eledf$name),]
  for (ia in 1:nadf) {
    if (airdf$powerwatts[ia]>0){
      ie <- which(eledf$name==airdf$poweredby[ia] )
      aname <- airdf$name[ia]
      ename <- eledf$name[ie]
      #print(sprintf("%s is to be powered by %s",aname,ename))
      glst <- addEdge(glst, ename, aname, lnkclass1)
      #glst <- addEdge(glst, aname, ename, lnkclass2)
    }
  }
  return(glst)
}


glst <- newGson()
glst <- addAirFlowNodes(glst, glb$airdf)
glst <- addElectricNodes(glst, glb$elcdf)
glst <- addElectricAirFlowLinks(glst, glb$airdf, glb$elcdf)
gfname1 <- "d:/tinkerpop/apache-gremlin-console-3.1.1-incubating/gson2.json"
gfname2 <- "ObjData/gsonall.json"

writeGsonFile(glst, gfname1)
writeGsonFile(glst, gfname2)
