# Json io data
#-------------
gstr <- function(r,sel, defstr="") {
  if (is.null(r[[sel]])) return(defstr)
  return(r[[sel]])
}
extractRooms <- function(jsonroomlst, ptsdflist) {
  rdf <- data.frame()
  for (r in jsonroomlst) {
    #print(r)
    rvek <- unlist(r)
    #print(rvek)
    rptsdf <- data.frame()
    rln <- length(rvek)
    if (length(r$pts)>0 ) {
      rptsdf <- data.frame(t(matrix(unlist(r$pts), nrow = 4)))
      names(rptsdf) <- c("class", "z", "y", "x") # note that we reverse x and z - damn unity left-handed coords!
      rptsdf$x <- as.numeric(as.character(rptsdf$x))
      rptsdf$y <- as.numeric(as.character(rptsdf$y))
      rptsdf$z <- as.numeric(as.character(rptsdf$z))
      # copy the last row into the first if we have a loop
      nr <- nrow(rptsdf)
      if (rptsdf$class[[nr]] == "loop") {
        rptsdf[nr,] <- rptsdf[1,]
      }
    }
    ptidx <- length(ptsdflist) + 1
    rdf1 <- data.frame(num = gstr(r,"num"), name = gstr(r,"name"), dname = gstr(r,"dname"), r.created = gstr(r,"r-created"), roomtype=gstr(r,"roomtype","Room"), ptidx = ptidx)
    if (nrow(rptsdf)>0){
      rdf1$xm <- round(mean(rptsdf$x,na.rm=T),2)
      rdf1$ym <- round(mean(rptsdf$y,na.rm=T),2)
      rdf1$zm <- round(mean(rptsdf$z,na.rm=T),2)
    } else {
      rdf1$xm <- 0
      rdf1$ym <- 0
      rdf1$zm <- 0
    }
    rdf <- rbind(rdf, rdf1)
    ptsdflist[[ptidx]] <- rptsdf
  }
  rdf$dname <- as.character(rdf$dname)
  rdf$dname <- ifelse( rdf$dname!="", rdf$dname, as.character(rdf$num) )
  rv <- list(rdf = rdf, ptsdflist = ptsdflist)
  return(rv)
}

extractWalls <- function(jsonwalllst, ptsdflist) {
  wdf <- data.frame()
  for (w in jsonwalllst) {
    wvek <- unlist(w)
    wptsdf <- data.frame()
    wln <- length(wvek)
    if (length(w$pts)>0) {
      wptsdf <- data.frame(t(matrix(unlist(w$pts), nrow = 4)))
      names(wptsdf) <- c("class", "z", "y", "x") # note that we reverse x and z - damn unity left-handed coords!
      wptsdf$x <- as.numeric(as.character(wptsdf$x))
      wptsdf$y <- as.numeric(as.character(wptsdf$y))
      wptsdf$z <- as.numeric(as.character(wptsdf$z))
      # copy the last row into the first if we have a loop
      nr <- nrow(wptsdf)
      if (wptsdf$class[[nr]] == "loop") {
        wptsdf[nr,] <- wptsdf[1,]
      }
    }
    ptidx <- length(ptsdflist) + 1
    wdf1 <- data.frame( name=gstr(w,"name"), r.created=gstr(w,"r-created"), ptidx = ptidx)
    wdf <- rbind(wdf, wdf1)
    ptsdflist[[ptidx]] <- wptsdf
  }
  rv <- list(wdf = wdf, ptsdflist = ptsdflist)
  return(rv)
}

bkxcoord <- c(-21.73, -16.1, -8.1, -8.1, 12.34, 23.56)
bkycoord <- c( 12.5,    2.1,  2.1, 12.5, 16.44, 16.44)
extractBeacons <- function(jsonbeaklst, ptsdflist) {
  bkdf <- data.frame()
  nbk <- length(bkxcoord)
  for (i in 1:nbk) {
    bname <- paste0("Beacon-",i)
    x <- bkxcoord[i]
    y <- bkycoord[i]
    bkdf1 <- data.frame( idx=i,name=bname,x=x,y=y)
    bkdf <- rbind(bkdf, bkdf1)
  }
  rv <- list(bkdf = bkdf)
  return(rv)
}

extractFromJsonListToDf <- function(jsonlst) {

  # extract nodes  
  nodevek <- unlist(jsonlst$nodes$list)
  nodedf <- data.frame(t(matrix(unlist(nodevek), nrow = 4)))
  names(nodedf) <- names(nodevek)[1:4]

  # idxfd <- which(grepl("FrontDesk",nodedf$name))

  # rotate -90 degrees 
  tmp <- nodedf$pt.x
  nodedf$pt.x <- nodedf$pt.z
  nodedf$pt.z <- tmp

  nodedf$pt.x <- as.numeric(as.character(nodedf$pt.x))
  nodedf$pt.y <- as.numeric(as.character(nodedf$pt.y))
  nodedf$pt.z <- as.numeric(as.character(nodedf$pt.z))

  linkvek <- unlist(jsonlst$links$list)
  ldf <- data.frame(t(matrix(unlist(linkvek), nrow = 3)))
  names(ldf) <- names(linkvek)[1:3]
  ldf$nn1 <- match(ldf$n1, nodedf$name)
  ldf$nn1.pt.x <- nodedf$pt.x[ldf$nn1]
  ldf$nn1.pt.y <- nodedf$pt.y[ldf$nn1]
  ldf$nn1.pt.z <- nodedf$pt.z[ldf$nn1]
  ldf$nn2 <- match(ldf$n2, nodedf$name)
  ldf$nn2.pt.x <- nodedf$pt.x[ldf$nn2]
  ldf$nn2.pt.y <- nodedf$pt.y[ldf$nn2]
  ldf$nn2.pt.z <- nodedf$pt.z[ldf$nn2]

  ptsdflist <- list()
  rrv <- extractRooms(jsonlst$rooms$list, ptsdflist)
  wrv <- extractWalls(jsonlst$walls$list, rrv$ptsdflist)
  bkrv <- extractBeacons(jsonlst$walls$list, rrv$ptsdflist)

  rv <- list(ldf = ldf, ndf = nodedf, rdf = rrv$rdf, ptsdflist = wrv$ptsdflist, wdf=wrv$wdf, bkdf=bkrv$bkdf)
  return(rv)
}

readFloorPlanJson <- function(fname) {
  floorJsonList <- fromJSON(paste(readLines(fname), collapse = ""))
  rv <- list()
  edat <- extractFromJsonListToDf(floorJsonList)
  rv$fpjl <- floorJsonList
  rv$ndf <- edat$ndf
  rv$ldf <- edat$ldf
  rv$rdf <- edat$rdf
  rv$wdf <- edat$wdf
  rv$bkdf <- edat$bkdf
  rv$ptsdflist <- edat$ptsdflist
  return(rv)
}

getptlist <- function(idx, ptsdflist) {
 # print(sprintf("getptlist idx:%d",idx))
  ptdf <- ptsdflist[[idx]]
  ptlist <- list()
  nrows <- nrow(ptdf)
  if (!is.null(ptdf) && nrows>0) {
  #  print(sprintf("getptlist idx:%d  npts:%d",idx,nrow(ptdf)))
    for (i in 1:nrows) {
      # loop is a special designator meaning close to the firat point
      if (i == nrows && ptdf$class[i] == "loop") { 
        # for the time being use zero, 
        # but the first point would probably be better 
        pt <- list(x = 0, y = 0, z = 0)
        lpt <- list(class = ptdf$class[i], pt = pt)
      } else {
        pt <- list(x = ptdf$z[i], y = ptdf$y[i], z = ptdf$x[i])
        lpt <- list(class = ptdf$class[i], pt = pt)
      }
      ptlist[[i]] <- lpt
    }
  }
  return(ptlist)
}
writeFloorPlanJson <- function(fproot, fname) {
  # convert rdf to list
  fpjl <- fproot$fpjl

  # The previous rooms, walls, etc in the json list are overwritten here
  # with the values that are in the data frames since they presumably have been
  # edited

  # rooms
  fpjl$rooms <- list(list = list())
  rdf <- fproot$rdf
  if (nrow(rdf) > 0) {
    for (i in 1:nrow(rdf)) {
      ptlist <- getptlist(rdf$ptidx[i], fproot$ptsdflist)
      rlst <- list(
              num = rdf$num[i],
              name = rdf$name[i],
              `r-created` = rdf$r.created[i],
              roomtype = rdf$roomtype[i],
              pts = ptlist
              )
      fpjl$rooms$list[[i]] <- rlst
    }
  }

  # walls
  fpjl$walls <- list(list = list())
  wdf <- fproot$wdf
  for (i in 1:nrow(wdf)) {
    ptlist <- getptlist(wdf$ptidx[i],fproot$ptsdflist)
    wlst <- list(
              name = wdf$name[i],
              `r-created` = wdf$r.created[i],
              pts = ptlist
              )
    fpjl$walls$list[[i]] <- wlst
  }

  writeLines(toJSON(fpjl), fname)
}

#Test Rooms
testWritingJsonData <- function(fpJsonList, newfname) {
  pt <- c("x" = 1, "y" = 2, "z" = 3)

  room1 <- makeNewRoom("4004", "BillGates")
  room1 <- addPtToRoom(room1, c("x" = -4.79, "y" = 0, "z" = -8.36), "ww")
  room1 <- addPtToRoom(room1, c("x" = 0.32, "y" = 0, "z" = -8.36), "ww")
  room1 <- addPtToRoom(room1, c("x" = 0.32, "y" = 0, "z" = -8.67), "wh")
  room1 <- addPtToRoom(room1, c("x" = 0.32, "y" = 0, "z" = -9.46), "dw")
  room1 <- addPtToRoom(room1, c("x" = 0.32, "y" = 0, "z" = -16.06), "ww")
  room1 <- addPtToRoom(room1, c("x" = -1.99, "y" = 0, "z" = -20.56), "ww")
  room1 <- addPtToRoom(room1, c("x" = -4.79, "y" = 0, "z" = -18.98), "ww")
  room1 <- addPtToRoom(room1, c("x" = 0, "y" = 0, "z" = 0), "loop")

  room2 <- makeNewRoom("4007", "KonradZuse")
  room2 <- addPtToRoom(room2, c("x" = 7.96, "y" = 0, "z" = -21.38), "ww")
  room2 <- addPtToRoom(room2, c("x" = 13.37, "y" = 0, "z" = -24.28), "wd")
  room2 <- addPtToRoom(room2, c("x" = 14.24, "y" = 0, "z" = -24.75), "hw")
  room2 <- addPtToRoom(room2, c("x" = 14.49, "y" = 0, "z" = -24.89), "ww")
  room2 <- addPtToRoom(room2, c("x" = 12.65, "y" = 0, "z" = -28.45), "ww")
  room2 <- addPtToRoom(room2, c("x" = 6.11, "y" = 0, "z" = -24.93), "ww")
  room2 <- addPtToRoom(room2, c("x" = 0, "y" = 0, "z" = 0), "loop")

  room3 <- makeNewRoom("4016")
  room3 <- addPtToRoom(room3, c("x" = 19.50, "y" = 0, "z" = -2.98), "ww")
  room3 <- addPtToRoom(room3, c("x" = 19.50, "y" = 0, "z" = -3.02), "wh")
  room3 <- addPtToRoom(room3, c("x" = 19.50, "y" = 0, "z" = -3.92), "dw")
  room3 <- addPtToRoom(room3, c("x" = 19.50, "y" = 0, "z" = -5.66), "ww")
  room3 <- addPtToRoom(room3, c("x" = 23.40, "y" = 0, "z" = -5.66), "ww")
  room3 <- addPtToRoom(room3, c("x" = 23.40, "y" = 0, "z" = -2.98), "ww")
  room3 <- addPtToRoom(room3, c("x" = 0, "y" = 0, "z" = 0), "loop")

  room4 <- makeNewRoom("room-nopoints")

  fpJsonList$rooms <- addRoomToJsonList(fpJsonList$rooms, room1)
  fpJsonList$rooms <- addRoomToJsonList(fpJsonList$rooms, room2)
  fpJsonList$rooms <- addRoomToJsonList(fpJsonList$rooms, room3)
  fpJsonList$rooms <- addRoomToJsonList(fpJsonList$rooms, room4)

  #Test Walls
  wall1 <- makeNewWall("OutsideBound")
  wall1 <- addPtToWall(wall1, c("x" = 24.50, "y" = 0, "z" = -38.28), "w")
  wall1 <- addPtToWall(wall1, c("x" = -9.75, "y" = 0, "z" = -19.79), "w")
  wall1 <- addPtToWall(wall1, c("x" = -9.75, "y" = 0, "z" = 12.60), "w")
  wall1 <- addPtToWall(wall1, c("x" = -3.83, "y" = 0, "z" = 35.31), "w")
  wall1 <- addPtToWall(wall1, c("x" = 24.50, "y" = 0, "z" = 35.31), "w")
  wall1 <- addPtToWall(wall1, c("x" = 0, "y" = 0, "z" = 0), "loop")

  wall2 <- makeNewWall("SlotHole")
  wall2 <- addPtToWall(wall2, c("x" = -9.75, "y" = 0, "z" = 12.60), "w")
  wall2 <- addPtToWall(wall2, c("x" = -6.90, "y" = 0, "z" = 23.23), "w")
  wall2 <- addPtToWall(wall2, c("x" =  12.6, "y" = 0, "z" = 23.23), "w")
  wall2 <- addPtToWall(wall2, c("x" =  12.6, "y" = 0, "z" = 12.60), "w")
  wall2 <- addPtToWall(wall2, c("x" = 0, "y" = 0, "z" = 0), "loop")

  wall3 <- makeNewWall("wall3-nopoints")

  fpJsonList$walls <- addWallToJsonList(fpJsonList$walls, wall1)
  fpJsonList$walls <- addWallToJsonList(fpJsonList$walls, wall2)
  fpJsonList$walls <- addWallToJsonList(fpJsonList$walls, wall3)

  writeFloorPlanJson(fpJsonList, newfname)
  return(fpJsonList)
}


# HoloLens Log Processing
# read in lines
readPosFromLog <- function(fname) {
  lines <- readLines(fname)
  poslines <- lines[grepl("Pos\\:\\(", lines)]
  poslines <- gsub("Pos\\:\\(", ",", poslines)
  poslines <- gsub("\\) Fwd\\:\\(", ",", poslines)
  poslines <- gsub("\\) Up\\:\\(", ",", poslines)
  poslines <- gsub("\\)", "", poslines)
  if (length(poslines) == 0) return(NULL);

  ddf <- read.csv(text = poslines)
  names(ddf) <- c("t", "pos.x", "pos.y", "pos.z", "fwd.x", "fwd.y", "fwd.z", "up.x", "up.y", "up.z")
  ddf <- ddf[order(ddf$t),]
  return(ddf)
}
transform <- function(df, org.x, org.z, rotdeg, trn.x, trn.z) {
  rotrad <- pi * rotdeg / 180
  s <- sin(rotrad)
  c <- cos(rotrad)
  px <- df$pos.x - org.x
  pz <- df$pos.z - org.z
  df$pos.x <- px * c - pz * s + trn.x
  df$pos.z <- px * s + pz * c + trn.z
  return(df)
}


readTrack <- function(trklst, fname, clr, read.rot, read.transx, read.transy, maxpts = NULL, mode = "log") {
  if (mode == "log") {
    df1 <- readPosFromLog(fname)
  } else {
    df1 <- read.csv(fname)
  }
  if (is.null(df1)) return(NULL)
  df1$pos.z <- -df1$pos.z
  df1 <- transform(df1, 0, 0, read.rot, read.transx, read.transy)
  df1 <- transform(df1, 0, 0, 112, -23.4, 6.4)
  track <- list();
  track$color <- clr
  org.x <- df1$pos.x[1]
  org.z <- df1$pos.z[1]
  df1$pos.x <- df1$pos.x - org.x
  df1$pos.z <- df1$pos.z - org.z
  track$name <- fname
  track$org.x <- org.x
  track$org.z <- org.z
  track$t.absmin <- min(df1$t)
  track$t.absmax <- max(df1$t)
  track$t.min <- min(df1$t)
  track$t.max <- max(df1$t)
  if (is.null(maxpts) | maxpts==0) {
    track$tdf <- df1
  } else {
    track$tdf <- df1[1:maxpts,]
  }
  trklst[[length(trklst) + 1]] <- track
  print(sprintf("Read %d rows from %s",nrow(df1),fname))
  return(trklst)
}

readLogs <- function() {
  rv <- list()
  rv$track <- list()
  #rv$track <- readTrack(rv$track,"birdblobs/hlbirdlog-20171017T205546UTC", "darkgreen", 0, 0, 0, NULL) # empty file
  rv$track <- readTrack(rv$track,"birdblobs/hlbirdlog-20171020T115841UTC", "darkgreen", 0, 0, 0, 40)
  rv$track <- readTrack(rv$track,"birdblobs/hlbirdlog-20171020T115347UTC", "blue", -77, 0, -3.1, 50)

  return(rv)
}

readTracks <- function(rv) {
  csvfnames <- list.files(path = "tracks/", pattern = "\\.csv$")
  #print(csvfnames)
  for (rootname in csvfnames) {
    fname <- paste0("tracks/", rootname)
    #print(fname)
    rv$track <- readTrack(rv$track, fname, "darkred",0,0,0,0, mode = "csv")
  }
  print(sprintf("There are now %d tracks",length(rv$track)))
  return(rv)
}

readAirflow <- function(fname){
  adf <- read.csv(fname,stringsAsFactors=F)
  adf$name <- trimws(adf$name)
  adf$upstream <- trimws(adf$upstream)
  adf$downstream <- trimws(adf$downstream)
  if (sum(duplicated(adf$name))>0){
    stop("Names in adf must be unique")
  }
  adf$x1 <-  adf$ux
  adf$y1 <-  adf$uz
  adf$z1 <-  adf$uy
  adf$x2 <-  adf$ux
  adf$y2 <-  adf$uz
  adf$z2 <-  adf$uy
  adf$len <- 0
  for (i in 1:nrow(adf)){
    if (adf$class[i]=="AirDuct"){
      #print(adf$name[i])
      uidx <- which(adf$name==adf$upstream[i])
      didx <- which(adf$name==adf$downstream[i])
      adf$x1[i] <-  adf$ux[uidx]
      adf$y1[i] <-  adf$uz[uidx]
      adf$z1[i] <-  adf$uy[uidx]
      adf$x2[i] <-  adf$ux[didx]
      adf$y2[i] <-  adf$uz[didx]
      adf$z2[i] <-  adf$uy[didx]
    }
  }
  for (i in 1:nrow(adf)){
    if (adf$class[i]=="AirDuct"){
      #print(adf$name[i])
      uidx <- which(adf$name==adf$upstream[i])
      didx <- which(adf$name==adf$downstream[i])
      if (adf$class[uidx]=="AirDuct"){
        rv <- findClosestPointToSegment(adf$x2[i],adf$y2[i], adf$x1[uidx],adf$y1[uidx],adf$x2[uidx],adf$y2[uidx])
        adf$x1[i] <-  rv$x
        adf$y1[i] <-  rv$y
        adf$z1[i] <-  adf$uy[didx] # used downstream value
      }
    }
  }
  dx <- adf$x2-adf$x1
  dy <- adf$y2-adf$y1
  dz <- adf$z2-adf$z1
  adf$len <- round(sqrt(dx*dx + dy*dy + dz*dz),4)
  adf$xm <- (adf$x1 + adf$x2)/2
  adf$ym <- (adf$y1 + adf$y2)/2
  adf$zm <- (adf$z1 + adf$z2)/2
  return(adf)
}

readElectric <- function(fname){
  edf <- read.csv(fname,stringsAsFactors=F)
  edf$name <- trimws(edf$name)
  edf$upstream <- trimws(edf$upstream)
  edf$downstream <- trimws(edf$downstream)
  if (sum(duplicated(edf$name))>0){
    stop("Names in edf must be unique")
  }
  edf$x1 <-  edf$ux
  edf$y1 <-  edf$uz
  edf$z1 <-  edf$uy
  edf$x2 <-  edf$ux
  edf$y2 <-  edf$uz
  edf$z2 <-  edf$uy
  for (i in 1:nrow(edf)){
    if (edf$class[i]=="ElecCond"){
      #print(edf$name[i])
      #print(edf$upstream[i])
      #print(edf$downstream[i])
      uidx <- which(edf$name==edf$upstream[i])
      didx <- which(edf$name==edf$downstream[i])
      edf$x1[i] <-  edf$ux[uidx]
      edf$y1[i] <-  edf$uz[uidx]
      edf$z1[i] <-  edf$uy[uidx]
      edf$x2[i] <-  edf$ux[didx]
      edf$y2[i] <-  edf$uz[didx]
      edf$z2[i] <-  edf$uy[didx]
    }
  }
  dx <- edf$x2-edf$x1
  dy <- edf$y2-edf$y1
  dz <- edf$z2-edf$z1
  edf$len <- round(sqrt(dx*dx + dy*dy + dz*dz),4)
  edf$xm <- (edf$x1 + edf$x2)/2
  edf$ym <- (edf$y1 + edf$y2)/2
  edf$zm <- (edf$z1 + edf$z2)/2
  return(edf)
}

normval <- function(dx,dy,dz){
  return(sqrt(dx*dx + dy*dy + dz*dz))
}
findClosestConduit <- function(x, y, z, sydf,seekclass) {
  nsydf <- nrow(sydf)
  if (nsydf<=0){
    stop("no power")
  }
  mindist <- 9e9
  imin <- 0
  rvmin <- NULL
  found <- FALSE
  foundname <- ""
  for (i in 1:nsydf) {
    if (sydf$class[i]==seekclass){
      rv <- findClosestPointToSegment(x,z,sydf$x1[i],sydf$y1[i],sydf$x2[i],sydf$y2[i])
      #dist <- normval(rv$x-x,rv$y-y,0)
      dist <- normval(rv$x-x,0,rv$y-z)
      if (dist<mindist){
        mindist <- dist
        foundname <- sydf$name[i]
        imin <- i
        rvmin <- rv
        found <- TRUE
      }
    }
  }
  rv <- list()
  rv$found <- found
  rv$foundname <- foundname
  rv$seekclass <- seekclass
  rv$imin <- imin
  rv$mindist <- mindist
  rv$x <- rvmin$x
  rv$y <- rvmin$y
  rv$lambda <- rvmin$lambda
  return(rv)
}
getNewName <- function(basename,existingnames){
  niter <- 0
  maxiter <- length(existingnames)+1
  ival <- 1
  newname <- sprintf("%s.%d",basename,ival)
  while (newname %in% existingnames && niter<maxiter ){
    ival <- ival+1
    newname <- sprintf("%s.%d",basename,ival)
    niter <- niter+1
  }
  return(newname)
}

addDuctFromXyz <- function(x,y,z,airdf,main_airdf){
  # Find Closest Conduit and Point - main ducts have no point in their name
  rv <- findClosestConduit(x, y, z, main_airdf,"AirDuct") 
  if (!rv$found) return # probably no AirDucts
  # Find Duct/Vent names
  closestDuctName <- airdf$name[rv$imin]
  newDuctName <- getNewName(closestDuctName,airdf$name)
  newVentName <- gsub("ADUCT","AVENT",newDuctName)

  # Add Duct
  ddf <- data.frame(name=newDuctName,class="AirDuct",room="")
  ddf$ux <- 0
  ddf$uy <- 0
  ddf$uz <- 0
  ddf$upstream <- closestDuctName
  ddf$downstream <- newVentName
  ddf$powerwatts <- 0
  ddf$width <- 0.1
  ddf$height <- 0.1
  ddf$x1 <- rv$x
  ddf$y1 <- rv$y
  ddf$z1 <- y
  ddf$x2 <- x
  ddf$y2 <- z
  ddf$z2 <- y
  ddf$len <- normval(x-rv$x,0,z-rv$y)
  ddf$xm <- (ddf$x1 + ddf$x2)/2
  ddf$ym <- (ddf$y1 + ddf$y2)/2
  ddf$zm <- (ddf$z1 + ddf$z2)/2

  # Add Vent
  vdf <- data.frame(name=newVentName,class="AirVent",room="")
  vdf$ux <- x
  vdf$uy <- z
  vdf$uz <- y
  vdf$upstream <- newDuctName
  vdf$downstream <- ""
  vdf$powerwatts <- 10
  vdf$width <- 0
  vdf$height <- 0
  vdf$x1 <- x
  vdf$y1 <- z
  vdf$z1 <- y
  vdf$x2 <- x
  vdf$y2 <- z
  vdf$z2 <- y
  vdf$len <- 0
  vdf$xm <- (vdf$x1 + vdf$x2)/2
  vdf$ym <- (vdf$y1 + vdf$y2)/2
  vdf$zm <- (vdf$z1 + vdf$z2)/2

  airdf <- rbind(airdf,ddf,vdf)

  return(airdf)
}
addDuctForRoom <- function(rdf,i,airdf,main_airdf){
  #print(sprintf("Adding duct for room:%s   %s",rdf$num[i],rdf$name[i]))
  x <- rdf$xm[i]
  y <- rdf$ym[i]
  z <- rdf$zm[i]
  airdf <- addDuctFromXyz(x,y,z,airdf,main_airdf)
  return(airdf)
}

addDuctForRooms <- function(rdf,airdf){
  ntodo <- nrow(rdf)
  if (ntodo<=0) return
  main_airdf <- airdf[!grepl("\\.",airdf$name),]
  if (nrow(main_airdf)<=0){
    stop("no main airducts found")
    return
  }

  for (i in 1:ntodo){
    rnum <- rdf$num[i]
    if (rnum=="4091") next   # ouside areas have no vents
    if (rnum=="4092") next
    if (rnum=="4093") next
    if (rnum=="4094") next
    if (rnum=="4095") next
    if (rnum=="4046.5") next
    if (rnum=="401") next  # elevators have no vents
    if (rnum=="402") next  
    airdf <- addDuctForRoom(rdf,i,airdf,main_airdf) 
  }
  return(airdf)
}

addElectricAirFlowPower <- function( airdf, elcdf) {
  nadf <- nrow(airdf)
  lnkclass1 <- "powers"
  lnkclass2 <- "poweredby"
  if (nadf <= 0) return
  airdf$poweredby <- ""
  main_elcdf <- elcdf[!grepl("\\.",elcdf$name),]
  for (ia in 1:nadf) {
    if (airdf$powerwatts[ia]>0){
      x <- airdf$xm[ia]
      y <- airdf$ym[ia]
      z <- airdf$zm[ia]
      rv <- findClosestConduit(x,z,y,main_elcdf,"ElecCond")
      ie <- rv$imin
      airdf$poweredby[ia] <- elcdf$name[ie]
      # now add a new electric conduit there
      closestCondName <- elcdf$name[rv$imin]
      newCondName <- getNewName(closestCondName,elcdf$name)
      ddf <- data.frame(name=newCondName,class="ElecCond",room="")
      ddf$ux <- 0
      ddf$uy <- 0
      ddf$uz <- 0
      ddf$upstream <- closestCondName
      ddf$downstream <- ""
      ddf$powerwatts <- 0
      ddf$x1 <- rv$x
      ddf$y1 <- rv$y
      ddf$z1 <- z
      ddf$x2 <- x
      ddf$y2 <- y
      ddf$z2 <- z
      ddf$len <- normval(x-rv$x,0,z-rv$y)
      ddf$xm <- (ddf$x1 + ddf$x2)/2
      ddf$ym <- (ddf$y1 + ddf$y2)/2
      ddf$zm <- (ddf$z1 + ddf$z2)/2

      elcdf <- rbind(elcdf,ddf)
    }
  }
  rv <- list()
  rv$airdf <- airdf
  rv$elcdf <- elcdf
  return(rv)
}
readBldSystems <- function(aname,ename,rdf){
  airdf <- readAirflow(aname)
  elcdf <- readElectric(ename)
  airdf <- addDuctForRooms(rdf,airdf)
  rv <- addElectricAirFlowPower(airdf,elcdf)
  return(rv)
}