


makeFloorPlot <- function(fpp) {
  #Set up the plot area
  par(mar = c(2.1,4,0,0)) # b,l,t,r in units of "lines of text"
  if (!is.null(fpp$imm)) {
    imx <- fpp$imm$xlim
    imy <- fpp$imm$ylim
    zimx <- fpp$imm$zoomxlim
    zimy <- fpp$imm$zoomylim

    plot(1:2, type = 'n', main = "", xlab = "", ylab = "Meters", xlim = zimx, ylim = zimy, col = "blue", asp = 1)
    lx <- c(imx[1], imx[1], imx[2], imx[2], imx[1])
    ly <- c(imy[1], imy[2], imy[2], imy[1], imy[1])

    #Get the plot information so the image will fill the plot box, and draw it
    #rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    if (!is.null(fpp$imm$img)) {
      rasterImage(fpp$imm$img, imx[1], imy[1], imx[2], imy[2])
    }
    if (!is.null(fpp$imm$grid)) {
      grid()
    }
    lines(lx, ly, lwd = 1, col = "green")
  } else {bayes
    plot(1:2, type = 'n', main = "", xlab = "", ylab = "Meters", xlim=c(-20,20),ylim=c(-15,15), asp = 1)
  }

  if (!is.null(fpp$nodes)) {
    ndf <- fpp$nodes$ndf
    points(ndf$pt.x, ndf$pt.z, col = fpp$nodes$color)
  }
  if (!is.null(fpp$links)) {
    ldf <- fpp$links$ldf
    for (i in 1:nrow(ldf)) {
      lx <- c(ldf$nn1.pt.x[i], ldf$nn2.pt.x[i])
      ly <- c(ldf$nn1.pt.z[i], ldf$nn2.pt.z[i])
      lines(lx, ly, lwd = fpp$links$width, col = fpp$links$color)
    }
  }
  if (!is.null(fpp$rooms)) {
    rdf <- fpp$rooms$rdf
    print(sprintf("total num rooms:%d",nrow(rdf)))
    for (i in 1:nrow(rdf)) {
      ptsdflistidx <- rdf$ptidx[[i]]
      lldf <- fpp$rooms$ptsdflist[[ptsdflistidx]]
      lines(lldf$x, lldf$z, lwd = fpp$rooms$width, col = fpp$rooms$color)
      #points(lldf$x, lldf$z, col = fpp$rooms$color)
    }
  }
  if (!is.null(fpp$roomnums)) {
    rdf <- fpp$roomnums$rdf
    print(sprintf("total num roomnums:%d",nrow(rdf)))
    for (i in 1:nrow(rdf)) {
      ptsdflistidx <- rdf$ptidx[[i]]
      lldf <- fpp$roomnums$ptsdflist[[ptsdflistidx]]
      if (nrow(lldf)>0){
        txt <- rdf$num[i]
        x <- mean(lldf$x)
        z <- mean(lldf$z)
        text(x, z, labels=txt, cex = fpp$roomnums$cex, col = fpp$roomnums$color )
        #points(x,z, col = fpp$roomnums$color)
      }
    }
  }
  if (!is.null(fpp$roomnames)) {
    rdf <- fpp$roomnames$rdf
    print(sprintf("total num roomnames:%d",nrow(rdf)))
    for (i in 1:nrow(rdf)) {
      ptsdflistidx <- rdf$ptidx[[i]]
      lldf <- fpp$roomnames$ptsdflist[[ptsdflistidx]]
      if (nrow(lldf)>0){
        txt <- rdf$dname[i]
        if (txt=="") txt <- rdf$num[i]
        x <- mean(lldf$x)
        z <- mean(lldf$z)
        text(x, z, labels=txt, cex = fpp$roomnames$cex, col = fpp$roomnames$color )
      }
    }
  }
  if (!is.null(fpp$walls)) {
    wdf <- fpp$walls$wdf
    print(sprintf("total num walls:%d", nrow(wdf)))
    for (i in 1:nrow(wdf)) {
      ptsdflistidx <- wdf$ptidx[[i]]
      lldf <- fpp$walls$ptsdflist[[ptsdflistidx]]
      clr <- c("darkred", "darkgreen", "darkblue")[[(i %% 3) + 1]]
      lines(lldf$x, lldf$z, lwd = fpp$walls$width, col = fpp$walls$color)
      #points(lldf$x, lldf$z, col = fpp$walls$color)
    }
  }
  if (!is.null(fpp$beacons)) {
    bkdf <- fpp$beacons$bkdf
    clr <-  fpp$beacons$color
    print(sprintf("total num beacons:%d", nrow(bkdf)))
    for (i in 1:nrow(bkdf)) {
      xb <- bkdf$x[i]
      yb <- bkdf$y[i]
      draw.circle(xb,yb,radius=0.4, lty=1,lwd=2, nv = 12, border=clr)
      txt <- sprintf("B:  %d",i)
      text(xb,yb, labels=txt,cex=0.8, col=clr)
    }
  }

  if (!is.null(fpp$blockers)) {
    bdf <- fpp$blockers$bdf
    print(sprintf("total blockers:%d", nrow(bdf)))
    clr <- fpp$blockers$color
    lwd <- fpp$blockers$width
    segments(bdf$x1, bdf$z1, bdf$x2, bdf$z2, lwd = lwd, col = clr)
  }
  if (!is.null(fpp$inters)) {
    idf <- fpp$inters$idf
    print(sprintf("total inters:%d", nrow(idf)))
    clr <- fpp$inters$color
    lwd <- fpp$inters$width
    points(idf$xi, idf$zi, col = clr, cex=2, pch=1)
  }
  if (!is.null(fpp$cinters)) {
    idf <- fpp$cinters$idf
    print(sprintf("total cinters:%d", nrow(idf)))
    clr <- fpp$cinters$color
    lwd <- fpp$cinters$width
    points(idf$xi, idf$zi, col = clr, cex = 2, pch = 1)
  }
  # Add tracks to plot
  if (!is.null(fpp$tracks)) {
    trktime <- fpp$tracks$tracktime
    ntodo <- length(fpp$tracks$track)
    print(sprintf("ntodo:%d",ntodo))
    if (ntodo > 0) {
      for (i in 1:ntodo) {
        trk <- fpp$tracks$track[[i]]
#        renderTrack(fpp, trk, trk$color, trk$org.x, trk$org.z, 0)
        renderTrack(fpp, trk, "darkblue", trk$org.x, trk$org.z, 0)
        if (!is.null(trktime)){
           tpp <- getPtOnTrack(trktime,trk)
           draw.circle( trk$org.x+tpp$x, trk$org.z+tpp$z, rad=0.2)
        }
      }

    }

  }
  # Add edlin to plot (these are the lines that we edit to create walls, etc.)
  if (!is.null(fpp$edlin)) {
    if (length(fpp$edlin$pts) > 0) {
      ptdf <- data.frame(t(matrix(unlist(fpp$edlin$pts), nrow = 2)))
      names(ptdf) <- c("x", "y")
      lines( ptdf$x, ptdf$y, lwd = fpp$edlin$width, col = fpp$edlin$color)
      points(ptdf$x, ptdf$y,  col = fpp$edlin$color)
    }
  }
  # Add location circles to plot
  if (!is.null(fpp$pcircs)) {
    ldf <- fpp$pcircs$ldf
    if (!is.null(ldf) & nrow(ldf) > 0) {
      dotracks <- fpp$pcircs$ltracks
      selall <- fpp$pcircs$selcircsall
      trk <- fpp$pcircs$track
      nang <- fpp$pcircs$anglesteps
      clr <- ifelse( ldf$sel, "purple", fpp$pcircs$color )
      rad <- fpp$pcircs$rad
      print(sprintf("plot locmarks dotracks:%s  selall:%s",dotracks,selall))
      for (i in 1:nrow(ldf)) {
        # note that draw.circle is not vectorizable, thus this is in a loop
        #draw.circle(ldf$x[i], ldf$y[i], radius=rad, lty=0, nv = 8, col = clr[i])
        rect(ldf$x[i]-rad,ldf$y[i]-rad, ldf$x[i]+rad,ldf$y[i]+rad, lty=0, col = clr[i])
        if (dotracks) {
          if (selall | ldf$sel[i]) {
            for (j in 0:(nang-1)) {
              ang <- j*2*pi/nang
              renderTrack(fpp, trk, "darkred", ldf$x[i], ldf$y[i], ang)
            }
          }
        }
      }
    }
  }

  if (!is.null(fpp$bayes)) {
    bcdf <- fpp$bayes$bcdf
    ldf <- fpp$bayes$ldf
    if (nrow(bcdf) > 0 && nrow(ldf)>0) {
      mrgdf <- bcdf %>% group_by(idx) %>% summarize(x=mean(x),y=mean(y),prob=sum(prob))
      mxprob <- max(mrgdf$prob)
      mrgdf$mprob <- mrgdf$prob/mxprob
      #print(mrgdf)
      clr <- fpp$bayes$color
      #asteps <- fpp$bayes$anglesteps
      #nv <- asteps
      #if (asteps<3) nv <- 4
      nv <- 12
      for (i in 1:nrow(mrgdf)) {
        # note that draw.circle is not vectorizable, thus this is in a loop
        x0 <- mrgdf$x[i]
        y0 <- mrgdf$y[i]
        rad <- fpp$bayes$rad * mrgdf$mprob[i]
        draw.circle(x0,y0, radius=rad, lty=0, nv=nv, col=clr)
      }

      dotracks <- fpp$bayes$btracks
      selall <- fpp$bayes$selcircsall
      trk <- fpp$bayes$track
      nang <- fpp$bayes$anglesteps
      rad <- fpp$bayes$rad
      trktime <- fpp$bayes$tracktime
      for (i in 1:nrow(bcdf)) {
        x0 <- bcdf$x[i]
        y0 <- bcdf$y[i]
        aidx <- bcdf$aidx[i]
        ang <- (aidx - 1) * 2 * pi / nang
        s = sin(ang)
        c = cos(ang)
        x1 <- x0 - s * rad * bcdf$mprob[i] # the minus sign because left handed...
        y1 <- y0 + c * rad * bcdf$mprob[i]
        segments(x0, y0, x1, y1, lwd = 2, col = "black") # angle markers
        if (dotracks) {
          if (selall | ldf$sel[i]) {
            if (bcdf$ninters[i] <= fpp$bayes$maxisect) {
              renderTrack(fpp, trk, "darkgreen", x0, y0, ang, t = trktime)
            }
          }
        }
      }
    }
  }
    if (!is.null(fpp$airflow)) {
    adf <- fpp$airflow$adf
    dctdf <- adf[adf$class=="AirDuct",]
    dcmdf <- dctdf[!grepl("\\.",dctdf$name),]
    dcfdf <- dctdf[grepl("\\.",dctdf$name),]
    vendf <- adf[adf$class %in% c("AirVent"),]
    intkdf <- adf[adf$class %in% c("AirIntake"),]
    vavdf <- adf[adf$class=="AirVav",]
    print(sprintf("total airducts:%d", nrow(dctdf)))
    print(sprintf("total main airducts:%d", nrow(dcmdf)))
    print(sprintf("total feed airducts:%d", nrow(dcfdf)))
    clr <- fpp$airflow$color
    clrtxt <- fpp$airflow$colortxt
    clrfill <- fpp$airflow$colorfill
    cex <- fpp$airflow$cex
    lwd <- fpp$airflow$width
    txtyofs <- 0.5
    donames <- fpp$airflow$donames
    domconds <- fpp$airflow$domconds && (nrow(dcmdf)>0)
    dofconds <- fpp$airflow$dofconds && (nrow(dcfdf)>0)
    dovents <- fpp$airflow$dovents && (nrow(vendf)>0)
    dovavs  <- fpp$airflow$dovavs  && (nrow(vavdf)>0)
    dointakes  <- fpp$airflow$dointakes  && (nrow(intkdf)>0)
    if (domconds){
      segments(dcmdf$x1, dcmdf$y1, dcmdf$x2, dcmdf$y2, lwd = 3, col = clr)
      if (donames){
        text(dcmdf$xm,dcmdf$ym+txtyofs, labels=dcmdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dofconds){
      segments(dcfdf$x1, dcfdf$y1, dcfdf$x2, dcfdf$y2, lwd = 1, col = clr)
      if (donames){
        text(dcfdf$xm,dcfdf$ym+txtyofs, labels=dcfdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dovents){
      drawcircs(vendf$xm, vendf$ym,radius=0.2, lty=1,lwd=1, nv = 12, border=clr)
      if (donames){
        text(vendf$xm,vendf$ym+txtyofs, labels=vendf$name,cex=cex, col=clrtxt)
      }
    }
    if (dovavs){
      drawcircs(vavdf$xm, vavdf$ym,radius=0.4, lty=1,lwd=1, nv = 4, border=clr,col=clrfill)
      if (donames){
        text(vavdf$xm,vavdf$ym, labels=vavdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dointakes){
      drawrects(intkdf$xm, intkdf$ym,width=0.6,height=0.6, lty=1,lwd=3, border=clr,col=clrfill)
      if (donames){
        text(intkdf$xm,intkdf$ym, labels=intkdf$name,cex=cex, col=clrtxt)
      }
    }
  }
  if (!is.null(fpp$electric)) {
    edf <- fpp$electric$edf
    ecdf <- edf[edf$class=="ElecCond",]
    ecmdf <- ecdf[!grepl("\\.",ecdf$name),]
    ecfdf <- ecdf[grepl("\\.",ecdf$name),]
    ejdf <- edf[edf$class %in% c("ElecJunc"),]
    esdf <- edf[edf$class %in% c("ElecSource"),]
    print(sprintf("total electric conduits:%d", nrow(ecdf)))
    clr <- fpp$electric$color
    clrtxt <- fpp$electric$colortxt
    clrfill <- fpp$electric$colorfill
    cex <- fpp$electric$cex
    lwd <- fpp$electric$width
    txtyofs <- 0.5
    donames <- fpp$electric$donames  
    dosource <- fpp$electric$dosource 
    domconds <- fpp$electric$domconds && (nrow(ecmdf)>0)
    dofconds <- fpp$electric$dofconds && (nrow(ecfdf)>0)
    dojuncs <- fpp$electric$dojuncs && (nrow(ejdf)>0)
    if (domconds){
      segments(ecmdf$x1, ecmdf$y1, ecmdf$x2, ecmdf$y2, lwd = 3, col = clr)
      if (donames){
        text(ecmdf$xm,ecmdf$ym+txtyofs, labels=ecmdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dofconds){
      segments(ecfdf$x1, ecfdf$y1, ecfdf$x2, ecfdf$y2, lwd = 1, col = clr)
      if (donames){
        text(ecfdf$xm,ecfdf$ym+txtyofs, labels=ecfdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dojuncs){
      drawcircs(ejdf$xm, ejdf$ym,radius=0.2, lty=1,lwd=1, nv = 8, border=clr)
      if (donames){
        text(ejdf$xm,ejdf$ym+txtyofs, labels=ejdf$name,cex=cex, col=clrtxt)
      }
    }
    if (dosource){
      drawrects(esdf$xm, esdf$ym,height=0.6,width=0.6, lty=1,lwd=3, border=clr,col=clrfill)
      if (donames){
        text(esdf$xm,esdf$ym+txtyofs, labels=esdf$name,cex=cex, col=clrtxt)
      }
    }
  }
}

getTrackAtTime <- function(t,trk){
  pt <- getPtOnTrack(t,trk)
  ntdf <- trk$tdf
  ntdf$pos.x <- ntdf$pos.x - pt$x
  ntdf$pos.z <- ntdf$pos.z - pt$z
  return(ntdf)
}

getPtOnTrack <- function(t,trk){
  rv <- list(x=0,z=0)
  tdf <- trk$tdf
  nr <- nrow(tdf)
  if (nr<=0) return(rv)
  tvt <- tdf$t
  tvx <- tdf$pos.x
  tvz <- tdf$pos.z
  rv$x <- approx(tdf$t,tvx,yleft=tvx[1],yright=tvx[nr], xout=t)$y
  rv$z <- approx(tdf$t,tvz,yleft=tvz[1],yright=tvz[nr], xout=t)$y
  return(rv)
}

renderTrack <- function(fpp, trk, clr, orgx,orgz,ang,t=NULL) {
  tdf <- trk$tdf
  if (!is.null(t)){
    tdf <- getTrackAtTime(t,trk)
  }
  idx <- trk$t.min <= tdf$t & tdf$t <= trk$t.max
  nptb <- nrow(tdf)
  tdf <- tdf[idx,]
  npta <- nrow(tdf)
  s <- sin(ang)
  c <- cos(ang)
  xx <- c*tdf$pos.x - s*tdf$pos.z + orgx
  zz <- s*tdf$pos.x + c*tdf$pos.z + orgz
#  if (is.null(npta)) npta <- -1
#  if (is.null(nptb)) nptb <- -1
#  print(sprintf("renderTrack nptbef:%d nptaft:%d -  org: %2f %2f %2f",nptb,npta,orgx,orgz,ang))
#  lines(xx, zz, lwd = trk$width, col = clr)
  lines(xx, zz, lwd = 2, col = clr)
}

draw.rect <- function(xcen, ycen,width=0.4,height=0.4, lty=1,lwd=2, density=NULL,angle=45, border="black",col=NA){
  xmn <- xcen-(width/2)
  xmx <- xcen+(width/2)
  ymn <- ycen-(height/2)
  ymx <- ycen+(height/2)
  rect(xmn,ymn,xmx,ymx,density=density,angle=angle,col=col,lty=lty,lwd=lwd,border=border)
}

drawrects <- function(xv, yv,width=0.4,height=0.4, lty=1,lwd=2, nv = 12,density=NULL,angle=45, border="black",col=NA){
  ntodo <- length(xv)
  if (ntodo<=0) return;
  for (i in 1:ntodo){
    draw.rect(xv[i],yv[i],width=width,height=height,density=density,angle=angle,lty=lty,lwd=lwd,border=border,col=col)
  }
}

drawcircs <- function(xv, yv,radius=0.4, lty=1,lwd=2, nv = 12,density=NULL,angle=45, border="black",col=NA){
  ntodo <- length(xv)
  if (ntodo<=0) return;
  for (i in 1:ntodo){
    draw.circle(xv[i],yv[i],radius=radius,density=density,angle=angle,lty=lty,lwd=lwd,nv=nv,border=border,col=col)
  }
}