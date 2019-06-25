findClosestPointToSegment <- function(ptx,pty,sx0,sy0,sx1,sy1 ){
  sdx <- sx1-sx0
  sdy <- sy1-sy0
  wx <- ptx-sx0
  wy <- pty-sy0
  lambda <- (wx*sdx + wy*sdy) / (sdx*sdx + sdy*sdy)
  clambda <- min(max(lambda,0),1)
  rv <- list()
  rv$lambda <- lambda
  rv$x <- sx0 + clambda*sdx
  rv$y <- sy0 + clambda*sdy
  return(rv)
}

genprobcircs <- function(xlim, ylim, delt, hex=F) {
  xstar <- min(xlim)
  nx <- trunc((abs(xlim[2] - xlim[1]) / delt) + 1)
  ny <- trunc((abs(ylim[2] - ylim[1]) / delt) + 1)
  if ((nx %% 2) == 1) nx = nx + 1 # we like even numubers
  if ((ny %% 2) == 1) ny = ny + 1
  xvals <- min(xlim) + (1:nx) * delt
  yvals <- min(ylim) + (1:ny) * delt
  cdf <- expand.grid(x = xvals, y = yvals)
  if (hex) {
    idx <- ((1:(nx * ny) %% 2) == 0)
    cdf[idx, 2] <- cdf[idx, 2] + delt / 2
  }
  cdf$prob <- 1 / nrow(cdf)
  cdf$mprob <- 1

  cdf$sel <- FALSE
  cdf$act <- FALSE
  cdf <- cbind(idx=1:nrow(cdf),cdf)
  cdf
}

testInside <- function(wptdf, pcircdf,who="") {
  # run along the points and see if the oriented area of the 
  # subtended triangles formed by the points in question
  # and the line segs always have the same sign.
  # if so we are "inside"
  rvek <- NULL
  nrc <- nrow(pcircdf)
  #  print(sprintf("  testInside %s  nrc:%d", who,nrc))
  cx <- pcircdf$x
  cy <- pcircdf$y
  nrw <- nrow(wptdf)
  wx <- wptdf$x
  wy <- wptdf$z
  allPos <- 0
  allNeg <- 0
  # rans <- sample(1:10, nrc, replace = T)
  for (i in 1:nrc) {
    discAllPos <- T
    discAllNeg <- T
    far <- F
    for (j1 in 1:nrw) {
      j2 <- j1 + 1
      if (j2 > nrw) j2 <- 1
      rx <- wx[j2] - wx[j1]
      ry <- wy[j2] - wy[j1]
      vx <- cx[i] - wx[j1]
      vy <- cy[i] - wy[j1]
      disc <- rx * vy - ry * vx
      if (is.null(disc)) {
        print("warning: disc is NULL")
      }
      if (is.na(disc)) {
        print("warning: disc is NA")
      }
      if (disc < 0) discAllPos <- F
      else if (disc > 0) discAllNeg <- F
      }
    if (discAllPos) {
      allPos <- allPos + 1
    }
    if (discAllNeg) {
      allNeg <- allNeg + 1
    }
    rvek <- c(rvek, (discAllPos || discAllNeg))
  }
 # print(sprintf("  testInside %s nrc:%d   nrw:%d   allPos:%d   allNeg:%d",who, nrc, nrw, allPos, allNeg))
  return(rvek)
}
filterInInside <- function(pcircdf, wptdf, who="") {
  inside <- testInside(wptdf, pcircdf, who = who)
  return(pcircdf[inside,])
}
filterInOutside <- function(pcircdf, wptdf, who="") {
  inside <- testInside(wptdf, pcircdf,who=who)
  return(pcircdf[!inside,])
}

filterOutOutside <- function(pcircdf, wdf, includeWalls, excludeWalls, ptsdflist) {
  # wdf is a dataframe containing the walls we need
  # ptsdflist is a list of dataframew with the actual points
  # these points are indexed from wdf

  inresdf <- NULL
  for (wname in includeWalls) {
    wob <- getWallFromWdf(wdf, wname)
  #  print(sprintf("filtering %s in %d", wname, wob$ptidx))
    inres1df <- filterInInside(pcircdf, ptsdflist[[wob$ptidx]])
    inresdf <- rbind(inresdf, inres1df)
  }

  exresdf <- inresdf
  for (wname in excludeWalls) {
    wst <- getWallFromWdf(wdf, wname)
 #   print(sprintf("filtering %s out %d", wname, wst$ptidx))
    exresdf <- filterInOutside(exresdf, ptsdflist[[wst$ptidx]])
  }
  exresdf
}

filterInRooms <- function(pcircdf, rdf, ptsdflist,roomlist=NULL,roomtypelist=NULL) {
  # rdf is a dataframe containing the rooms 
  # ptsdflist is a list of dataframew with the actual points
  # these points are indexed from wdf
  resdf <- NULL
  if (!is.null(roomlist)) {
    froomlist <- factor(roomlist, levels = levels(rdf$num))
    #print(froomlist)
    rdf <- rdf[ rdf$num %in% froomlist,]
  }
  if (!is.null(roomtypelist)) {
    froomtypelist <- factor(roomtypelist, levels = levels(rdf$roomtype))
    rdf <- rdf[rdf$roomtype %in% froomtypelist,]
  }
  if (nrow(rdf) > 0) {
    for (i in 1:nrow(rdf)) {
      rname <- rdf$name[i]
      idx <- rdf$ptidx[i]
      ptsdf <- ptsdflist[[idx]]
      if (nrow(ptsdf) > 0) {
        res1df <- filterInInside(pcircdf, ptsdf,who=rname)
        resdf <- rbind(resdf, res1df)
      }
    }
  }
  resdf
}

filterOutAllRooms <- function(pcircdf, rdf, ptsdflist) {
  # rdf is a dataframe containing the rooms 
  # ptsdflist is a list of dataframew with the actual points
  # these points are indexed from wdf
  resdf <- pcircdf
  if (nrow(rdf) > 0) {
    for (i in 1:nrow(rdf)) {
      rname <- rdf$name[i]
      idx <- rdf$ptidx[i]
      ptsdf <- ptsdflist[[idx]]
      if (nrow(ptsdf) > 0) {
        resdf <- filterInOutside(resdf, ptsdf,who=rname)
      }
    }
  }
  resdf
}