library(png)
library(rjson)
# par(mar=c(2,2,2,2))
#Replace the directory and file information with your info


# Rooms
addroomtojson <- function(roomlist,newroom) {
  nl <- length(roomlist$list)
  roomlist$list[nl+1] <- list(newroom)
  return(roomlist)
}

makeroom <- function (roomname){
  t <- Sys.time()
  attr(t,"tzone") <- "UTC"
  newroom <- list()
  newroom[["name"]] <- roomname
  newroom[["r-created"]] <- format(t,"%Y-%m-%d %H:%M:%S %Z")
  newroom[["pts"]] <- list(list("list"=list()))
  return(newroom)
}

addpttoroom <- function (room,pt,ptclass){
  nl <- length(room[["pts"]]$list)
  room[["pts"]]$list[nl+1] <- list(list("class"=ptclass,"pt"=pt))
  return(room)
}
# Walls
addwalltojson <- function(walllist,newwall) {
  nl <- length(walllist$list)
  walllist$list[nl+1] <- list(newwall)
  return(walllist)
}

makewall <- function (wallname){
  t <- Sys.time()
  attr(t,"tzone") <- "UTC"
  newwall <- list()
  newwall[["name"]] <- wallname
  newwall[["r-created"]] <- format(t,"%Y-%m-%d %H:%M:%S %Z")
  newwall[["pts"]] <- list(list("list"=list()))
  return(newwall)
}

addpttowall <- function (wall,pt,ptclass){
  nl <- length(wall[["pts"]]$list)
  wall[["pts"]]$list[nl+1] <- list(list("class"=ptclass,"pt"=pt))
  return(wall)
}


readblddata <- function(fname){
  blddatalst <- fromJSON(paste(readLines(fname), collapse=""))
}


writeblddata <- function(blddatalst,fname){
  writeLines(toJSON(blddatalst),fname)
}

#Test Rooms
testWritingJsonData <- function(bld_data_list){
  room1 <- makeroom("room1")
  pt <- c("x" = 1, "y" = 2, "z" = 3)
  room1 <- addpttoroom(room1,pt,"w")
  room1 <- addpttoroom(room1,pt+1,"w")
  
  room2 <- makeroom("room2")
  room2 <- addpttoroom(room2,pt+2,"d")
  
  room3 <- makeroom("room3")
  
  bld_data_list$rooms <- addroomtojson(bld_data_list$rooms, room1 )
  bld_data_list$rooms <- addroomtojson(bld_data_list$rooms, room2 )
  bld_data_list$rooms <- addroomtojson(bld_data_list$rooms, room3 )
  
  #Test Walls
  wall1 <- makewall("wall1")
  pt <- c("x" = 1, "y" = 2, "z" = 3)
  wall1 <- addpttowall(wall1,pt,"w")
  wall1 <- addpttowall(wall1,pt+1,"w")
  
  wall2 <- makewall("wall2")
  wall2 <- addpttowall(wall2,pt+2,"d")
  
  wall3 <- makewall("wall3")
  
  bld_data_list$walls <- addwalltojson(bld_data_list$walls, wall1 )
  bld_data_list$walls <- addwalltojson(bld_data_list$walls, wall2 )
  bld_data_list$walls <- addwalltojson(bld_data_list$walls, wall3 )
  
  
  writeblddata(bld_data_list,"new_gen_bho_x.json")
  return (bld_data_list)
}



extractNodesAndLinks <- function(jsonlst){
  
  # extract nodes  
  nodevek <- unlist(jsonlst$nodes$list)
  nodedf <- data.frame(t(matrix(unlist(nodevek),nrow=4)))
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
  ldf <- data.frame(t(matrix(unlist(linkvek),nrow=3)))
  names(ldf) <- names(linkvek)[1:3]
  ldf$nn1 <- match(ldf$n1,nodedf$name)
  ldf$nn1.pt.x <- nodedf$pt.x[ldf$nn1]
  ldf$nn1.pt.y <- nodedf$pt.y[ldf$nn1]
  ldf$nn1.pt.z <- nodedf$pt.z[ldf$nn1]
  ldf$nn2 <- match(ldf$n2,nodedf$name)
  ldf$nn2.pt.x <- nodedf$pt.x[ldf$nn2]
  ldf$nn2.pt.y <- nodedf$pt.y[ldf$nn2]
  ldf$nn2.pt.z <- nodedf$pt.z[ldf$nn2]
  rv <- list(ldf=ldf,ndf=nodedf)
  return(rv)
}


  

plotthings <- function(ima,imx,imy,ndf,ldf){
  #Set up the plot area
  plot(1:2, type='n', main="Rooms", xlab="x", ylab="y",xlim=imx,ylim=imy,col="blue",asp=1)
  
  #Get the plot information so the image will fill the plot box, and draw it
  lim <- par()
  #rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  rasterImage(ima, imx[1], imy[1], imx[2], imy[2])
  grid()
  
  points(ndf$pt.x,ndf$pt.z, col="blue")
  lx <- c(imx[1],imx[1],imx[2],imx[2],imx[1] )
  ly <- c(imy[1],imy[2],imy[2],imy[1],imy[1] )
  lines(lx,ly, lwd=1, col="green")
  
  for (i in 1:nrow(ldf)){
    lx <- c(ldf$nn1.pt.x[i],ldf$nn2.pt.x[i])
    ly <- c(ldf$nn1.pt.z[i],ldf$nn2.pt.z[i])
    lines(lx,ly, lwd=1, col="pink")
  }
  
}

ima <- readPNG("DE-BHO-4-reducedTo33pct.png")
imx <- c(39.015,-43.448)
imy <- c(-22.3,25.63)

bdlst <- readblddata("gen_bho_x.json")

bdlst1 <- testWritingJsonData(bdlst)

rv <- extractNodesAndLinks(bdlst1)
plotthings(ima,imx,imy,rv$ndf,rv$ldf)


# HoloLens Log Processing
# read in lines
readPosFromLog <- function( fname ){
  lines <- readLines(fname)
  poslines <- lines[grepl("Pos\\:\\(",lines)]
  poslines <- gsub("Pos\\:\\(",",",poslines)
  poslines <- gsub("\\) Fwd\\:\\(",",",poslines)
  poslines <- gsub("\\) Up\\:\\(",",",poslines)
  poslines <- gsub("\\)","",poslines)
  df <- read.csv(text=poslines)
  names(df) <- c("t","pos.x","pos.y","pos.z","fwd.x","fwd.y","fwd.z","up.x","up.y","up.z")
  return(df)
}
transform <- function(df,org.x,org.z,rotdeg,trn.x,trn.z){
  rotrad <- pi*rotdeg/180
  s <- sin(rotrad)
  c <- cos(rotrad)
  px <- df$pos.x - org.x
  pz <- df$pos.z - org.z
  df$pos.x <- px*c - pz*s  + trn.x
  df$pos.z <- px*s + pz*c  + trn.z
  return(df)
}



df1 <- readPosFromLog("birdblobs/hlbirdlog-20171020T115347UTC")
df1 <- transform(df1,0,0,77,0,3.1)
df1$pos.z <- - df1$pos.z

df2 <- readPosFromLog("birdblobs/hlbirdlog-20171020T115841UTC")
df2$pos.z <- - df2$pos.z

df1 <- transform(df1,0,0,112,-23,6)
df2 <- transform(df2,0,0,112,-23,6)


# Add tracks to plot
lines(df1$pos.x,df1$pos.z,col="purple")
lines(df2$pos.x,df2$pos.z,col="blue")
