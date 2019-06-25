library(png)
library(rjson)
par(mar=c(2,2,2,2))
#Replace the directory and file information with your info
ima <- readPNG("DE-BHO-4-reducedTo33pct.png")
xl <- c(-40,40)
yl <- c(-20,20)
xl <- c(-43.448,39.015)
yl <- c(-22.3,25.63)

ul <- c(-22.3,39.015)
ur <- c(25.63,39.015)
lr <- c(25.63,-43.448)
ll <- c(-22.3,-43.448)

json_data <- fromJSON(paste(readLines("gen_bho.json"), collapse=""))
nodevek <- unlist(json_data$nodes$list)
nodedf <- data.frame(t(matrix(unlist(nodevek),nrow=4)))
names(nodedf) <- names(nodevek)[1:4]
nodedf$pt.x <- as.numeric(as.character(nodedf$pt.x)) - json_data$floorplan$translate$x
nodedf$pt.y <- as.numeric(as.character(nodedf$pt.y)) - json_data$floorplan$translate$y
nodedf$pt.z <- as.numeric(as.character(nodedf$pt.z)) - json_data$floorplan$translate$z
nodedf$pt.z <- -nodedf$pt.z

json_data <- fromJSON(paste(readLines("gen_bho.json"), collapse=""))
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
plot(1:2, type='n', main="Rooms", xlab="x", ylab="y",xlim=xl,ylim=yl,col="blue")

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
#rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rasterImage(ima, xl[1], yl[1], xl[2], yl[2])
#grid()

points(nodedf$pt.z,nodedf$pt.x, col="blue")
lx <- c(xl[1],xl[1],xl[2],xl[2],xl[1] )
ly <- c(yl[1],yl[2],yl[2],yl[1],yl[1] )
lines(lx,ly, lwd=1, col="green")

for (i in 1:nrow(ldf)){
  lx <- c(ldf$nn1.pt.z[i],ldf$nn2.pt.z[i])
  ly <- c(ldf$nn1.pt.x[i],ldf$nn2.pt.x[i])
  lines(lx,ly, lwd=1, col="red")
}