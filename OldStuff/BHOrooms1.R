library(png)
library(rjson)
par(mar=c(2,2,2,2))
#Replace the directory and file information with your info
ima <- readPNG("DE-BHO-4-reducedTo33pct.png")
xlim <- c(39.015,-43.448)
ylim <- c(-22.3,25.63)


bldjson <- paste(readLines("gen_bho_x.json"), collapse="")

json_data <- fromJSON(bldjson)
nodevek <- unlist(json_data$nodes$list)
nodedf <- data.frame(t(matrix(unlist(nodevek),nrow=4)))
names(nodedf) <- names(nodevek)[1:4]

idxfd <- which(grepl("FrontDesk",nodedf$name))

# rotate -90 degrees
tmp <- nodedf$pt.x
nodedf$pt.x <- nodedf$pt.z
nodedf$pt.z <- tmp

nodedf$pt.x <- as.numeric(as.character(nodedf$pt.x))
nodedf$pt.y <- as.numeric(as.character(nodedf$pt.y)) 
nodedf$pt.z <- as.numeric(as.character(nodedf$pt.z))

json_data <- fromJSON(bldjson)
linkvek <- unlist(json_data$links$list)
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

#Set up the plot area
plot(1:2, type='n', main="Rooms", xlab="x", ylab="y",xlim=xlim,ylim=ylim,col="blue",asp=1)

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
#rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rasterImage(ima, xlim[1], ylim[1], xlim[2], ylim[2])
grid()

points(nodedf$pt.x,nodedf$pt.z, col="blue")
lx <- c(xlim[1],xlim[1],xlim[2],xlim[2],xlim[1] )
ly <- c(ylim[1],ylim[2],ylim[2],ylim[1],ylim[1] )
lines(lx,ly, lwd=1, col="green")

for (i in 1:nrow(ldf)){
  lx <- c(ldf$nn1.pt.x[i],ldf$nn2.pt.x[i])
  ly <- c(ldf$nn1.pt.z[i],ldf$nn2.pt.z[i])
  lines(lx,ly, lwd=1, col="pink")
}

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


lines(df1$pos.x,df1$pos.z,col="purple",xlim=c(-20,30),ylim=c(-10,35))
lines(df2$pos.x,df2$pos.z,col="blue")