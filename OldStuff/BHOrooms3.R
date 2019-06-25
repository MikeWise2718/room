library(png)
library(rjson)


source("mods/ModFpObjs.R")
source("mods/ModFpReadWrite.R")


plotthings <- function(fpp) {
  #Set up the plot area
  if (!is.null(fpp$imm)) { 
    imx <- fpp$imm$xlim
    imy <- fpp$imm$ylim

    plot(1:2, type='n', main="Rooms", xlab="x", ylab="y",xlim=imx,ylim=imy,col="blue",asp=1)
    lx <- c(imx[1], imx[1], imx[2], imx[2], imx[1])
    ly <- c(imy[1], imy[2], imy[2], imy[1], imy[1])

  #Get the plot information so the image will fill the plot box, and draw it
  #rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    rasterImage(fpp$imm$img, imx[1], imy[1], imx[2], imy[2])
    grid()
    lines(lx, ly, lwd = 1, col = "green")
  }

  if (!is.null(fpp$nodes)){
    ndf <- fpp$nodes$ndf
    points(ndf$pt.x, ndf$pt.z, col = "blue")
  }

  
  if (!is.null(fpp$links)) {
    ldf <- fpp$links$ldf
    for (i in 1:nrow(ldf)) {
      lx <- c(ldf$nn1.pt.x[i], ldf$nn2.pt.x[i])
      ly <- c(ldf$nn1.pt.z[i], ldf$nn2.pt.z[i])
      lines(lx, ly, lwd = 1, col = "pink")
    }
  }
  # Add tracks to plot
  if (!is.null(fpp$tracks)) {
    for (i in 1:length(fpp$tracks)) {
      tdf <- fpp$tracks$track[[i]]$tdf
      clr <- fpp$tracks$track[[i]]$color
      lines(tdf$pos.x, tdf$pos.z, col = clr)
    }
  }
}


# Read and write
fpln <- readFloorPlanJson("gen_bho_x.json")
fpjl <- testWritingJsonData(fpln$fpjl, "new_gen_bho_x.json")




tracks <- readlogs()


# Now do the plot
fpp <- list()
fpp$imm <- list()
fpp$imm$img <- readPNG("images/DE-BHO-4-reducedTo33pct.png")
fpp$imm$xlim <- c(39.015, -43.448)
fpp$imm$ylim <- c(-22.3, 25.63)
fpp$nodes <- list()
fpp$nodes$ndf <- fpln$ndf
fpp$links <- list()
fpp$links$ldf <- fpln$ldf
fpp$tracks <- tracks



plotthings(fpp)



