library(shiny)
library(ggplot2)
library(png)
library(rjson)
library(plotrix)
library(dplyr)

# To do
# - Have Unity write out the floor plan bitmap corner coords into the json
#   and then use those. 
# - Investigate the reversed X-axis orientation (due to Unity's Left Handedness?)
#   and handle automatically.
# - Figure out exactly when we can hit a breakpoint in a sourced module 
#   With a shiny app you hit a breakpoint in a sourced model if you started it with "sourcing"
#   You don't if you started it with the "Run Shiny App" icon, that gives you "module is not in current executing frame"

source("mods/ModFpObjs.R")
source("mods/ModFpReadWrite.R")
source("mods/ModFloorPlot.R")
source("mods/ModGeom.R")

# Initialzation ----

testfilename <- "ObjData/test1.json"
initfilename <- "ObjData/cur_gen_bho_x.json"
airflowfname <- "ObjData/AirFlow.csv"
electricfname <- "ObjData/Electric.csv"

imfname <- "images/DE-BHO-4-reducedTo33pct.png"
homeImxlim <- c(39.015, -43.448)
homeImylim <- c(-22.3, 25.63)

defFiltXrange <- c(-50,50)
defFiltYrange <- c(-50,50)

heit <- 1800
heits <- paste0(heit, "px")

glb <- reactiveValues(fpln = list(), tracks = list(), lastBeakIdx = 1,lastTrackIdx = 1)
fpln <- readFloorPlanJson(initfilename)
glb$fpln <- fpln
rdf <- fpln$rdf
rv <- readBldSystems(airflowfname,electricfname,rdf)
glb$airdf <- rv$airdf
glb$elcdf <- rv$elcdf

glbtracks <- readLogs()
trackstemp <- readTracks(glbtracks)

# this gets around needing a reactive context to assign the length
glb$tracks <- trackstemp
glb$lastTrackIdx <- 1
#glb$lastTrackIdx <- length(trackstemp$track)
trackstemp <- NULL

glbcirc <- reactiveValues(locdf = data.frame(), circrad = 0.25, homeImxlim = homeImxlim, homeImylim = homeImylim, 
                          circdelt = 1, filtXrange=defFiltXrange,filtYrange =defFiltYrange,
                          cidf = data.frame(), sumcidf = data.frame(), lastCircIdx = 1,   nang = 1, hexgrid=F)

bayes <- reactiveValues(pPrio = data.frame(), pLike = data.frame(), pPost = data.frame())

roomtypes <- c("Room", "ConfRoom", "SmallConfRoom", "OpenDeskArea", "OpenArea",
               "MensRoom", "WomensRoom", "Bathroom", "Kitchen",
               "StorageRoom", "Balcony", "OfficeRoom", "DeadSpace")

pointclass <- c("ww", "wd", "wh", "dw", "hw", "wi", "iw", "ii")

getnextptclass <- function(lstclass) {
  switch(lstclass,
        "ww" = "ww",
        "wi" = "iw",
        "wh" = "dw",
        "wd" = "hw",
        "dw" = "ww",
        "hw" = "ww",
        "iw" = "ww",
        "ii" = "iw",
        "na" = "na",
        "ww" # default
        )
}



## Styles ---
databstyle <- "font-size:80%;font-family:'Lucida Console New'"
butblue <- "color: #fff; background-color: #4285f4; border-color: #2e6da4; font-size:80%" # blue
butgreen <- "color: #fff; background-color: #3aa853; border-color: #2e6da4; font-size:80%" # green
butyellow<- "color: #fff; background-color: #fbbc05; border-color: #2e6da4; font-size:80%" # yellow
butred <- "color: #fff; background-color: #ea4335; border-color: #2e6da4; font-size:80%"# red
dispst1 <- "display: inline-block;vertical-align:bottom; text-align:center; width: 60px; height:29px;"

dispst2 <- "display: inline-block;vertical-align:bottom; text-align:center; width: 66px; height:80px;"

#  Shiny UI ----

u <- shinyUI(fluidPage(

 sidebarLayout(position = "left",
    sidebarPanel(h3("Bayesian Localization"),
     tabsetPanel(id="sidetab1",
        tabPanel("Prob Space",
           checkboxGroupInput("locRTfilter","Roomtype Filter:",
                               c("Outside" = "outside", "Rooms" = "rooms", "Conf Room" = "confrooms", 
                                "Open Desk" = "opendesk","NotRooms" = "notrooms" ),
                                selected = c("outside"), inline = T),
           checkboxGroupInput("locRoomfilter","Room Filter:",
                               c("Gates" = "gates", "Hopper" = "hopper", "Moore" = "moore", 
                                "Zuse" = "zuse", "Nixdorf" = "nixdorf", "Leibnitz" = "leibnitz"),
                                selected = c( "moore"), inline = T),
           sliderInput("locxrange", "X-Range:", min = defFiltXrange[1], max = defFiltXrange[2], value = defFiltXrange),
           sliderInput("locyrange", "Y-Range:", min = defFiltYrange[1], max = defFiltXrange[2], value = defFiltYrange),

           numericInput("gridsize", "Prob Dist Grid Size (M)", value=1, min=0.25, max=5, step=0.25, width="160px"),
           checkboxInput("hexgrid", "Hex Grid", value=T, width = "160px"),
           sliderInput("anglesteps", "Prob Dist Angles", min=1, max=12, value=1, step=1, round= T)
         ),
           tabPanel("Time",
             uiOutput("timerangeControls")
           ),
           tabPanel("View",
             checkboxGroupInput("fpvis", "View:",
                               c("Nodes" = "nodes", "Links" = "links", "Walls" = "walls",
                                 "Rooms" = "rooms","Beacons"="beacons", "Tracks" = "tracks", "EdLine" = "edlin",
                                 "Blockers" = "blockers", "Intersections" = "ninters", "Bitmap" = "bitmap",
                                 "Grid" = "grid","Room Nums"="roomnums","Room Names"="roomnames",
                                 "Locs" = "locmarks","Ltracks"="ltracks"),
                                 selected = c("blockers", "edlin", "bitmap_no", "bayes", "locmarks"), inline = T),
             checkboxGroupInput("airvis", "AirFlow View:",
                               c("AirFlow" = "airflow","Intakes"="afintakes", "Main Ducts"="afmduct","Feeder Ducts"="affduct","Vents"="afvents","Vavs"="afvavs","A-names"="afnames"), 
                               selected = c("airflow","afintakes","afmduct"),inline = T),
             checkboxGroupInput("elcvis", "Electric View:",
                               c("Electric"="electric","Source"="elsource","Main Conduits"="elmcond","Feeder Conduits"="elfcond","Juncts"="eljuncs","E-names"="elnames"), 
                               selected = c("electric","elsource","elmcond","elnames"),inline = T),
             checkboxGroupInput("byvis", "Bayes View:",
                               c("Bayes" = "bayes", "Btracks"="btracks", "Binters" = "binters"),
                                 selected = c("bayes"), inline = T),
             sliderInput("maxisect", "Max Track Isect Display", min=0, max=12, value=1, step=0.5, round= T),
             radioButtons("trkvis", "Tracks:",
                              c( "All" = "tracks", "Selected" = "sel-track","Multi-Loctrack" = "pctrak"), 
                              selected = "sel-track", inline = T),
             radioButtons("debehave", "Show Tracks:",
                 c("On Selected Locs" = "selcircs", "On All Locs" = "selcircsall"),
                  selected = c("selcircsall"), inline = T),
             uiOutput("indexControls")                              
           ),
           tabPanel("File",
             actionButton("savefilebut", "Save File"),
             actionButton("loadfilebut", "Load File"), br(),
             actionButton("savetotest1but", "Save to Test1"),
             actionButton("reinitbut", "Reinit")
           ),
#           ),
#        tabsetPanel(id="sidetab2",
          tabPanel("Calculus",
                    radioButtons("bayesvis", "Bayes Selection:",
                           c("Prior" = "prior", "Likelihood" = "likely", "Posterior" = "post"),
                           selected = "likely", inline = T),
                    fluidRow(
                      h4("Baysian Ops"),
                      actionButton("uniformprior", "Uniform\nPriors", style = butblue),
                      actionButton("posttoprior", "Post -> Prior", style = butblue),
                      actionButton("calcpost", "Calc Posteriors", style = butblue)
                      ),
                    fluidRow(
                      h4("Add New Info"),
                      div(style = dispst2, actionButton("addtrack", "Track", style = butgreen)),
                      div(style = dispst2, actionButton("addbeacon", "Beacon", style = butgreen)),
                      div(style = dispst2, actionButton("addroom", "Room", style = butgreen)),
                      div(style = dispst2, actionButton("addloc", "Loc", style = butgreen))
                      ),
                    sliderInput("beakdist", "Beacon Dist", value=0, min=0, max=10, step=0.25),
                    sliderInput("beakstdev", "Beacon Spread (sd)", value=1, min=0.1, max=5, step=0.1)
                  ),
          tabPanel("Edit",
            selectInput("newlinemode", "Edit Mode", c("Nothing", "Room", "Wall", "Track"), selected = "Room"),
            uiOutput("lineeditControls")
                  ),
          tabPanel("Opts",
             radioButtons("isectcalcmeth", "Isect Calc Method:",
                     c("Pure-R" = "purer",  "Batch-Cpp" = "batchcpp", "Batch-Cuda" = "batchcuda", "Call-Cuda" = "callcuda"),
                     selected = "purer", inline = T), 
            checkboxGroupInput("calcopts", "Opts:",c("Open Doors" = "opendoors","Write Inters"="writeinters"), 
                                                        selected = c("opendoors","writeinters"), inline = T),
            sliderInput("montecarloiters", "MonteCarlo Iterations", min = 0, max = 200, value = 0, step = 1, round = T)
           ),
           selected="View"  
        ),
         width = 3
     ),
     mainPanel(tabsetPanel(id = "maintab",
                tabPanel("Floor Plan",
                fluidRow(
                  actionButton("regenbut", "Redraw", style = butblue),
                  actionButton("homebut", "Home Zoom", style = butblue),
                  actionButton("backzoombut", "Back Zoom", style = butblue),
                  div(style = paste0(dispst1, butgreen), checkboxInput("createlinecb", "Line", FALSE)),
                  actionButton("newlinebut", "New Lines", style = butgreen),
                  actionButton("delpointbut", "Del Point", style = butgreen),
                  actionButton("closeloopbut", "Close Loop", style = butgreen)
                ),
                fluidRow(
                  plotOutput("floorplan", brush = brushOpts(id = "zbrush", resetOnNew = TRUE), click = "pclick", height = heits),
                  verbatimTextOutput("plotclick")
                  )
                  ),
              tabPanel("Locs", textOutput("circstx"), div(dataTableOutput("circsdf"), class = "table", style = databstyle)),
# tabPanel("Nodes", textOutput("nodestx"), div(dataTableOutput("nodesdf"), class = "table", style = databstyle)),
# tabPanel("Links", textOutput("linkstx"), div(dataTableOutput("linksdf"), class = "table", style = databstyle)),
              tabPanel("Prior", textOutput("priortx"), div(dataTableOutput("priordf"), class = "table", style = databstyle)),
              tabPanel("Likelihood", textOutput("likeltx"), div(dataTableOutput("likeldf"), class = "table", style = databstyle)),
              tabPanel("Posterior", textOutput("postrtx"), div(dataTableOutput("postrdf"), class = "table", style = databstyle)),
              tabPanel("Rooms", textOutput("roomstx"), div(dataTableOutput("roomsdf"), class = "table", style = databstyle)),
              tabPanel("Walls", textOutput("wallstx"), div(dataTableOutput("wallsdf"), class = "table", style = databstyle)),
              tabPanel("Beacons", textOutput("beakstx"), div(dataTableOutput("beaksdf"), class = "table", style = databstyle)),
              tabPanel("Tracks", textOutput("tracstx"), div(dataTableOutput("tracsdf"), class = "table", style = databstyle)),
              tabPanel("Points", textOutput("pointtx"), div(dataTableOutput("pointdf"), class = "table", style = databstyle)),
              tabPanel("Blocks", textOutput("blocktx"), div(dataTableOutput("blockdf"), class = "table", style = databstyle)),
              tabPanel("ninters", textOutput("intertx"), div(dataTableOutput("interdf"), class = "table", style = databstyle)),
              tabPanel("Cnters", textOutput("cntertx"), div(dataTableOutput("cnterdf"), class = "table", style = databstyle)),
              tabPanel("SumCnters", textOutput("smcnttx"), div(dataTableOutput("smcntdf"), class = "table", style = databstyle)),
              tabPanel("AirFlow", textOutput("airdftx"), div(dataTableOutput("airdfdf"), class = "table", style = databstyle)),
              tabPanel("Electric", textOutput("elcdftx"), div(dataTableOutput("elcdfdf"), class = "table", style = databstyle)),
              tabPanel("Stats", verbatimTextOutput('statustxt'))
             )
     )
  )
))

# Shiny Server ---

s <- shinyServer(function(input, output) {

## UI Related Helper Functions ---
  getDisplayBayesDf <- reactive({
    rv <- data.frame()
    ssel <- input$bayesvis
    if (ssel == "prior") rv <- bayes$pPrio
    if (ssel == "likely") rv <- bayes$pLike
    if (ssel == "post") rv <- bayes$pPost
    return(rv)
  })
  getDisplayBayesClr <- reactive({
    rv <- "gray"
    ssel <- input$bayesvis
    if (ssel == "prior") rv <- "pink"
    if (ssel == "likely") rv <- "lightgreen"
    if (ssel == "post") rv <- "lightblue"
    return(rv)
  })
    numPointsDf <- reactive({ length(glb$fpln$ptsdflist) })

  curPointsDf <- reactive({
    glb$fpln$ptsdflist[[as.numeric(input$ptdfidx)]]
  })

  numCircs <- reactive({ nrow(glbcirc$locdf) })
  numTracks <- reactive({ length(glb$tracks$track) })
  numBeaks <- reactive({ nrow(glb$fpln$bkdf) })

  curTrackIdx <- reactive({ glb$lastTrackIdx })

  curTrack <- reactive({
    if (length(glb$tracks$track)<=0) return(NULL)
    itrk <- curTrackIdx()
    if(is.null(itrk)) return(NULL)
    glb$tracks$track[[itrk]]
  })
  curtrkabsmin <- reactive({
    trk <- curTrack()
    trk$t.absmin
  })
  curtrkabsmax <- reactive({
    trk <- curTrack()
    trk$t.absmax
  })
  curtrkabsrng <- reactive({
    trk <- curTrack()
    range <- c(trk$t.absmin, trk$t.absmax)
    range
  })
  curtrkmin <- reactive({
    trk <- curTrack()
    trk$t.min
  })
  curtrkmax <- reactive({
    trk <- curTrack()
    trk$t.max
  })
  curtrkrng <- reactive({
    req(input$trackidx) # eliminates warning
    trk <- curTrack()
    range <- c(trk$t.min, trk$t.max)
    range
  })


## File IO --- 
  csvfilt <- t(matrix(c("All files (*.*)", "*.*", "Csv files (*.csv)", "*.csv"), 2, 2))
  jsnfilt <- t(matrix(c("All files (*.*)", "*.*", "Json files (*.json)", "*.json"), 2, 2))
  observeEvent(input$loadfilebut, {
    fname <- choose.files(caption = "Load Floor Plan Data", filters = jsnfilt, multi = F)
    if (length(fname) != 0) {
      print(sprintf("Loading %s", fname))
      glb$fpln <- readFloorPlanJson(fname)
      edlinrv$regencnt <- edlinrv$regencnt + 1
    }
  })

  observeEvent(input$savefilebut, {
    if (edlinrv$mode != "Track") {
      fname <- choose.files(caption = "Save Floor Plan Data", filters = jsnfilt, multi = F)
      if (length(fname) != 0) {
        #fname <- file.choose(new=F)
        print(sprintf("Saving %s", fname))
        writeFloorPlanJson(glb$fpln, fname)
      }
    } else {
      #fname <- choose.files(caption = "Save Track Data", filters = csvfilt, multi = F)
      #if (length(fname) != 0) {
      #fname <- file.choose(new=F)
      track <- curTrack()
      fname <- gsub("\\/", "-", track$name)
      fname <- paste0("tracks/", fname, ".csv")
      print(sprintf("Saving %s", fname))
      writeTrack(curTrack(), fname)
    }
  })
  writeTrack <- function(track, fname) {
    # put the origin back :)
    track$tdf[1, "pos.x"] <- track$org.x
    track$tdf[1, "pos.z"] <- track$org.z
    write.csv(track$tdf, fname)
    print(sprintf("Saved %d rows to %s", nrow(track$tdf), fname))
  }
  observeEvent(input$savetotest1but, {
    writeFloorPlanJson(glb$fpln, testfilename)
    print("reading test1.json")
  })


  observeEvent(input$reinitbut, {
    glb$fpln <- readFloorPlanJson(initfilename)
    print("reiniting")
  })

  idxCtlStyle <- "display: inline-block;vertical-align:bottom; text-align:center; width: 66px; height:80px;"
  output$indexControls <- renderUI({
    
    dispst <- paste0(idxCtlStyle, butblue)
    rv <- tagList(
    fluidRow(
     div(style = dispst, selectInput("ptdfidx", "Pts DF Idx", 1:numPointsDf(), 1)),
     div(style = dispst, selectInput("circidx", "Loc Idx", 1:numCircs(), glb$lastCircIdx)),
     div(style = dispst, selectInput("trackidx", "Track Idx", 1:numTracks(), glb$lastTrackIdx)),
     div(style = dispst, selectInput("beakidx", "Beac Idx", 1:numBeaks(), glb$lastBeakIdx)),
     div(style = idxCtlStyle, actionButton("dumptabdf", "Dump", style = butblue),
      actionButton("loadtabdf", "Load", style = butblue))
     )
    )
    return(rv)
  })

  observeEvent(input$trackidx, { glb$lastTrackIdx <- as.numeric(input$trackidx)  })
  observeEvent(input$circidx, { glbcirc$lastCircIdx <- as.numeric(input$circidx) })
  observeEvent(input$beakidx, { glb$lastBeakIdx <- as.numeric(input$beakidx) })

  observeEvent(input$locxrange,{
                glbcirc$filtXrange <- input$locxrange
                setupLocationStateSpace()

             })
  observeEvent(input$locyrange,{
                glbcirc$filtYrange <- input$locyrange
                setupLocationStateSpace()
             })

## Room and Wall Editing 1 ---

  edlinrv <- reactiveValues(enabled = F, mode = "Nothing", pts = list(), ptsclass = list(),
                            lastclickx = 0, lastclicky = 0, regencnt = 0, nextptclass = "ww",
                            lastcidx = 0)

  output$lineeditControls <- renderUI({
    textname <- paste0(edlinrv$mode, " Name")
    newobjname <- paste0("New-", edlinrv$mode, " Name")

    if (edlinrv$mode == "Nothing") {
      rv <- tagList()
    } else if (edlinrv$mode == "Room") {
      rv <- tagList(
              selectInput("newpointclass", "Point Class", pointclass, selected = edlinrv$nextptclass),
              selectInput("newroomtype", "Room Type", roomtypes, selected = "Room", width = "160px"),
              textInput("roomnum", "Room num", "1234"),
              textInput("objnewname", textname, newobjname),
              actionButton("newroombut", "Make New Room", style = butgreen)
              )
    } else if (edlinrv$mode == "Wall") {
      rv <- tagList(
              selectInput("newpointclass", "Point Class", pointclass, selected = edlinrv$nextptclass),
              textInput("objnewname", textname, newobjname),
              actionButton("newwallbut", "Make New Wall", style = butyellow)
              )
    } else if (edlinrv$mode == "Track") {
      rv <- tagList(
              textInput("objnewname", textname, newobjname),
              actionButton("newtrackbut", "Make New Track", style = butred)
              )
    }
    return(rv)
  })


  output$timerangeControls <- renderUI({
    print("timerangecontrols")   
    rv <- tagList(
         sliderInput("tracktimerange", "Time Range:", min = curtrkabsmin(), max = curtrkabsmax(), value = curtrkrng()),
         sliderInput("tracktime", "Time", min = curtrkmin(), max = curtrkmax(), value = 0)
      )
    return(rv)
  })

  observeEvent(input$gridsize, {
    glbcirc$circdelt <- input$gridsize
    glbcirc$hexgrid <- input$hexgrid
    setupLocationStateSpace()
  })
  observeEvent(input$hexgrid, {
    glbcirc$circdelt <- input$gridsize
    glbcirc$hexgrid <- input$hexgrid
    setupLocationStateSpace()
  })


  observeEvent(input$anglesteps, {
    glbcirc$nang <- input$anglesteps
    setupLocationStateSpace()
  })
  observeEvent(input$locRTfilter, {
    setupLocationStateSpace()
  },ignoreNULL=FALSE)
  observeEvent(input$locRoomfilter, {
    setupLocationStateSpace()
  },ignoreNULL=FALSE)

  observeEvent(input$tracktimerange, {
    isolate({
      # don't think you should replace these as a function
      glb$tracks$track[[as.numeric(input$trackidx)]]$t.min <- input$tracktimerange[1]
      glb$tracks$track[[as.numeric(input$trackidx)]]$t.max <- input$tracktimerange[2]
    })
  })

## Room and Wall Editing 2 ---

  observeEvent(input$createlinecb, {
    data <- isolate({
      zooomrv$enabled <- !input$createlinecb
      edlinrv$enabled <- input$createlinecb
    })
  })

  observeEvent(input$newlinemode, {
    edlinrv$mode <- input$newlinemode
  })
  observeEvent(input$delpointbut, {
    np <- length(edlinrv$pts)
    if (np > 0) {
      edlinrv$pts[[np]] <- NULL
      edlinrv$ptsclass[[np]] <- NULL
    }
  })
  observeEvent(input$closeloopbut, {
    np <- length(edlinrv$pts)
    if (np > 0) {
      edlinrv$pts[[np + 1]] <- edlinrv$pts[[1]]
      edlinrv$ptsclass[[np + 1]] <- "loop"
    }
  })
  observeEvent(input$newlinebut, {
    edlinrv$pts <- list()
    edlinrv$ptsclass <- list()
  })
  observeEvent(input$newroombut, {
    if (length(edlinrv$pts) == 0) {
      return;
    }
    nidx <- numPointsDf()
    eptsdf <- data.frame(t(matrix(unlist(edlinrv$pts), nrow = 2)))
    names(eptsdf) <- c("x", "z")
    eptsdf$y <- 0
    eptsdf$class <- unlist(edlinrv$ptsclass)
    glb$fpln$ptsdflist[[nidx + 1]] <- eptsdf
    newptidx <- nidx + 1
    r1df <- data.frame(num = input$roomnum, name = input$objnewname, roomtype = input$newroomtype, r.created = getCurUtcTimeStr(), ptidx = newptidx)
    glb$fpln$rdf <- rbind(glb$fpln$rdf, r1df)
    print(sprintf("Made new room:%s  ptidx:%d  roomcount:%d", input$roomnum, newptidx, nrow(glb$fpln$rdf)))
  })
  observeEvent(input$newwallbut, {
    if (length(edlinrv$pts) == 0) {
      return;
    }
    nidx <- numPointsDf()
    eptsdf <- data.frame(t(matrix(unlist(edlinrv$pts), nrow = 2)))
    names(eptsdf) <- c("x", "z")
    eptsdf$y <- 0
    eptsdf$class <- "ww"
    glb$fpln$ptsdflist[[nidx + 1]] <- eptsdf
    newptidx <- nidx + 1
    w1df <- data.frame(name = input$objnewname, r.created = getCurUtcTimeStr(), ptidx = newptidx)
    glb$fpln$wdf <- rbind(glb$fpln$wdf, w1df)
    print(sprintf("Made new wall:%s  ptidx:%d  wallcount:%d", input$objnewname, newptidx, nrow(glb$fpln$wdf)))
  })

  observeEvent(input$dumptabdf, {
    csvdf <- NULL
    sels <- input$maintab
    csvfname <- paste0("dumps/", sels)
    cvdf <- getMaintabDf()
    if (nrow(cvdf) == 0) {
      print(sprintf("No rows to write for %s", sels))
      return
    }
    csvfname <- paste0(csvfname, "-", getCurUtcTimeStrNoSpaces(), ".csv")
    write.csv(csvdf, csvfname)
    print(sprintf("Wrote %d rows to %s", nrow(csvdf), csvfname))
  })

  makenewtrack <- function(df1) {
    track <- list();
    track$name <- input$objnewname
    track$color <- "blue"
    org.x <- df1$pos.x[1]
    org.z <- df1$pos.z[1]
    df1$pos.x <- df1$pos.x - org.x
    df1$pos.y <- 0
    df1$pos.z <- df1$pos.z - org.z
    df1$fwd.x <- round(df1$pos.x - dplyr::lag(df1$pos.x, default = 0), 3)
    df1$fwd.y <- 0
    df1$fwd.z <- round(df1$pos.z - dplyr::lag(df1$pos.z, default = 0), 3)
    df1$up.x <- 0
    df1$up.y <- 1
    df1$up.z <- 0
    track$org.x <- org.x
    track$org.z <- org.z
    track$t.absmin <- min(df1$t)
    track$t.absmax <- max(df1$t)
    track$t.min <- min(df1$t)
    track$t.max <- max(df1$t)
    df1 <- df1[c("t", "pos.x", "pos.y", "pos.z", "fwd.x", "fwd.y", "fwd.z", "up.x", "up.y", "up.z")]
    track$tdf <- df1
    track
  }

  observeEvent(input$newtrackbut, {
    if (length(edlinrv$pts) == 0) {
      return;
    }
    nidx <- numPointsDf()
    eptsdf <- data.frame(t(matrix(unlist(edlinrv$pts), nrow = 2)))
    names(eptsdf) <- c("pos.x", "pos.z")
    eptsdf$t <- 1:nrow(eptsdf)
    eptsdf$pos.y <- 0
    track <- makenewtrack(eptsdf)
    lntrk <- numTracks()
    glb$tracks$track[[lntrk + 1]] <- track
    print(sprintf("Made new track %d", lntrk + 1))
  })

  roundToCm <- function(x) { round(50 * x) / 50 }

  observeEvent(input$pclick, {
    xcm <- roundToCm(input$pclick$x)
    ycm <- roundToCm(input$pclick$y)
    print(sprintf("-obs pclick %.1f %.1f",xcm,ycm))
    edlinrv$lastclickx <- xcm
    edlinrv$lastclicky <- ycm
    if (edlinrv$enabled) {
      print("edlinrv$enabled")
      np <- length(edlinrv$pts)
      xp <- xcm
      yp <- ycm
      curpclass <- input$newpointclass
      if (is.null(curpclass)) curpclass = "na"
      nxtpclass <- getnextptclass(curpclass)
      #print(sprintf("curclass:%s  nxtclass:%s", curpclass, nxtpclass))
      edlinrv$pts[[np + 1]] <- c(xp, yp)
      edlinrv$ptsclass[[np + 1]] <- curpclass
      edlinrv$nextptclass <- nxtpclass
    }
    if ("selcircs" %in% input$debehave) {
      print("selcircs")
      # Find closes circle
      cdf <- glbcirc$locdf[, c("x", "y")]
      cdf$dist <- (cdf$x - xcm) * (cdf$x - xcm) + (cdf$y - ycm) * (cdf$y - ycm)
      mindist <- min(cdf$dist)
      idx <- which(cdf$dist == mindist)
      glbcirc$locdf[idx, "sel"] <- TRUE
      edlinrv$lastcidx = idx
    }
  })

## Zoom handling ---

  zooomrv <- reactiveValues(enabled = T, fname = imfname, xlim = homeImxlim, ylim = homeImylim, zoomxlim = homeImxlim, zoomylim = homeImylim, lastzoom = list())

  observeEvent(input$backzoombut, {
    print("-backzoombut")
    nl <- length(zooomrv$lastzoom)
    if (nl > 0) {
      zooomrv$zoomxlim <- zooomrv$lastzoom[[nl]][[1]]
      zooomrv$zoomylim <- zooomrv$lastzoom[[nl]][[2]]
      zooomrv$lastzoom[[nl]] <- NULL
    }
  })

  observeEvent(input$regenbut, {
    edlinrv$regencnt = edlinrv$regencnt + 1
    print(edlinrv$regencnt)
  })
  observeEvent(input$homebut, {
    print("-homebut")
    zooomrv$zoomxlim <- homeImxlim
    zooomrv$zoomylim <- homeImylim
    zooomrv$lastzoom <- list()
  })
  observeEvent(input$zbrush, {
    if (zooomrv$enabled) {
      print("-obs zbrush")
      brush <- input$zbrush
      if (!is.null(brush)) {
        xdelt <- abs(brush$xmax-brush$xmin)
        ydelt <- abs(brush$ymax-brush$ymin)
        print(sprintf("zoom xdelt:%.1f ydelt:%1f",xdelt,ydelt))
        if (xdelt>5 & ydelt>5){  # too much zoom sucks
        # save values for zoom stack
          nl <- length(zooomrv$lastzoom)
          zooomrv$lastzoom[[nl + 1]] <- list(zooomrv$zoomxlim, zooomrv$zoomylim)
          zooomrv$zoomxlim <- c(brush$xmax, brush$xmin) #ugh
          zooomrv$zoomylim <- c(brush$ymin, brush$ymax)
          print(zooomrv$zoomxlim)
        } else {
          print("zoom rejected")
        }
      }
    }
  })

  ## Inspection Tabs ---
    output$statustxt = renderPrint({
    t <- Sys.time()
    attr(t, "tzone") <- "UTC"
    timemsg <- sprintf("The time is %s", format(t, "%Y-%m-%d %H:%M:%S %Z"))

    msg <- sprintf("Locs:%d Nodes:%d Links:%d Rooms:%d  Walls:%d Tracks:%d Pointlists:%d",
                     nrow(glbcirc$locdf), nrow(glb$fpln$ndf), nrow(glb$fpln$ldf), nrow(glb$fpln$rdf),
                     nrow(glb$fpln$wdf), numTracks(), numPointsDf())

    isectPsec <- isectStats$nIsect / max(0.01, isectStats$elap)

    print(timemsg)
    print(msg)
    print("")
    print("Last Cinter Calculation")
    print(sprintf("   circles:%d tracks:%d  segs:%d  hits:%d", isectStats$ncrc, isectStats$ntrk, isectStats$nseg, isectStats$nhit))
    print(sprintf("    isects:%.0f filts:%d pot:%d", isectStats$nIsect, isectStats$nIsectToFilt, isectStats$nbdftot))
    print(sprintf("    elap:%.1f secs  - isects:%.0f isects/sec:%.1f", isectStats$elap, isectStats$nIsect, isectPsec))

  })

  getMaintabDf <- function() {
    df <- data.frame()
    sels <- input$maintab
    if (sels == "Locs") df <- glbcirc$locdf
    if (sels == "Rooms") df <- glb$fpln$rdf
    if (sels == "Walls") df <- glb$fpln$wdf
    if (sels == "Beacons") df <- glb$fpln$bkdf
    if (sels == "Tracks") df <- curTrack()$tdf
    if (sels == "Points") df <- curPointsDf()
    if (sels == "Links") df <- glb$fpln$ndf
    if (sels == "Nodes") df <- glb$fpln$ndf
    if (sels == "AirFlow") df <- glb$airdf
    if (sels == "Electric") df <- glb$elcdf
    if (sels == "Blocks") df <- getBlockersDf()
    if (sels == "ninters") df <- calcCurTrackIntersectionsDf()
    if (sels == "Cnters") df <- glbcirc$cidf
    if (sels == "SumCnters") df <- glbcirc$sumcidf
    if (sels == "Prior") df <- bayes$pPrio
    if (sels == "Likelihood") df <- bayes$pLike
    if (sels == "Posterior") df <- bayes$pPost
    df
  }

  opts <- list(pageLength = 100)
  output$circsdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$nodesdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$linksdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$roomsdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$wallsdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$beaksdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$blockdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$interdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$cnterdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$smcntdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$airdfdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$elcdfdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$pointdf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$priordf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$likeldf <- renderDataTable({ getMaintabDf() }, options = opts)
  output$postrdf <- renderDataTable({ getMaintabDf() }, options = opts)

  maintabText <- function() {
    df <- getMaintabDf()
    msg <- sprintf("%s rows %d", input$maintab, nrow(df))
    msg
  }

  output$circstx <- renderText({ maintabText() })
  output$nodestx <- renderText({ maintabText() })
  output$linkstx <- renderText({ maintabText() })
  output$roomstx <- renderText({ maintabText() })
  output$wallstx <- renderText({ maintabText() })
  output$beakstx <- renderText({ maintabText() })
  output$blocktx <- renderText({ maintabText() })
  output$intertx <- renderText({ maintabText() })
  output$cntertx <- renderText({ maintabText() })
  output$smcnttx <- renderText({ maintabText() })
  output$airdftx <- renderText({ maintabText() })
  output$elcdftx <- renderText({ maintabText() })
  output$tracstx <- renderText({ maintabText() })
  output$pointtx <- renderText({ maintabText() })
  output$priortx <- renderText({ maintabText() })
  output$likeltx <- renderText({ maintabText() })
  output$postrtx <- renderText({ maintabText() })


  output$nptdfidx <- reactive({ edlinrv$nptslist })

  output$maintab <- renderText({ input$maintab })

## Bayes Ops ---

observeEvent(input$uniformprior,{
   initBayesProbs()
   edlinrv$regencnt = edlinrv$regencnt + 1
})

observeEvent(input$posttoprior,{
   bayes$pPrio <- bayes$pPost
   bayes$pLike <- bayes$pPost
   edlinrv$regencnt = edlinrv$regencnt + 1
})

observeEvent(input$calcpost, {
   calcPosteriors()
  })

## State Space Manipulation ---
  
  setupLocationStateSpace <- reactive({
   # req(input$locRTfilter)    
  #  req(input$locRoomfilter)    
    #print(input$locRTfilter)
    #print(input$locRoomfilter)
    glbcirc$locdf <- genprobcircs(glbcirc$homeImxlim, glbcirc$homeImylim, glbcirc$circdelt,hex=glbcirc$hexgrid)
    #print(sprintf("setup location state space circdelt:%.2f nang:%d",glbcirc$circdelt,glbcirc$nang))
    locRTfilter <- input$locRTfilter
    locRoomfilter <- input$locRoomfilter
    cdf <- glbcirc$locdf
    linrange <- glbcirc$filtXrange[1] <= cdf$x  & cdf$x <= glbcirc$filtXrange[2]  & glbcirc$filtYrange[1] <= cdf$y  & cdf$y <= glbcirc$filtYrange[2] 
    #print(linrange)
    if (sum(linrange)>0){
      cdf <- cdf[linrange,]
    }
    if ("outside" %in% locRTfilter) {
      cdf <- filterOutOutside(cdf, glb$fpln$wdf,
                                    c("OutsideBound"),
                                    c("SlotWall", "InBound", "InBoundNub1", "InBoundNub2", "InBound2"),
                                    glb$fpln$ptsdflist)
    }
    if ("rooms" %in% locRTfilter) {
      cdf <- filterInRooms(cdf, glb$fpln$rdf, glb$fpln$ptsdflist)
    } else if ("notrooms" %in% locRTfilter){
      cdf <- filterOutAllRooms(cdf, glb$fpln$rdf, glb$fpln$ptsdflist)
    }
    else {
      anames <- c("confrooms", "opendesk")
      lanames <- anames %in% locRTfilter
      cnames <- c("gates", "hopper", "moore", "zuse", "nixdorf", "leibnitz")
      lcnames <- cnames %in% locRoomfilter
      if (sum(lcnames) > 0) {
        rnames <- c("4004", "4005", "4006", "4007", "4012", "4014")
        cdf <- filterInRooms(cdf, glb$fpln$rdf, glb$fpln$ptsdflist, roomlist = rnames[lcnames])
      }
      else if (sum(lanames) > 0) {
        aanames <- c("ConfRoom", "OpenDeskArea")
        cdf <- filterInRooms(cdf, glb$fpln$rdf, glb$fpln$ptsdflist, roomtypelist = aanames[lanames])
      }
    }
    glbcirc$locdf$act <- FALSE
    glbcirc$locdf[cdf$idx,]$act <- TRUE # note we use cdf$idx here
    initBayesProbs()
    cdf <- glbcirc$locdf
    cdf <- cdf[cdf$act,]
    return(cdf)
  })

  getActiveLocsDf <- function() {
    cdf <- glbcirc$locdf
    cdf <- cdf[cdf$act,]
    return(cdf)
  }

  initBayesProbs <- function() {
    ppdf <- getActiveLocsDf()
    np <- nrow(ppdf)
    nang <- glbcirc$nang
    npzags <- rep(1:np, each = nang)
    #print(npzags)
    ppdf <- ppdf[npzags,]
    ppdf$prob <- NULL
    ppdf$mprob <- NULL
    ppdf$sel <- NULL
    ppdf$act <- NULL
    ppdf$aidx <- rep(1:nang, np)
    ppdf$ang <- 360 * (ppdf$aidx - 1) / nang
    ppdf$prob <- 1 / (np * nang)
    ppdf$mprob <- 1
    ppdf$ninters <- 0
    bayes$pPrio <- ppdf
    bayes$pLike <- ppdf
    bayes$pPost <- ppdf
    print("pLike initialized to Uniform")

    # cdf
  }

  ## Likelihood by Path Intersections ---
  isectStats <- reactiveValues(nIsects = 0, nIsectsOutFilt = 0, nbdftot = 0, elap = 0, ncrc = 0, ntrk = 0, nseg = 0, nhit = 0)


  calcFunIntersBySegDf <- function(bdf,sid,x1, x2, z1, z2) {
    # https://www.topcoder.com/community/data-science/data-science-tutorials/geometry-concepts-line-intersection-and-its-applications/

    idf <- cbind(sid=sid,bdf)
    nbdf <- nrow(idf)

    # 1 mm tolerance
    xmx <- max(x1, x2) + 0.001
    xmn <- min(x1, x2) - 0.001
    zmx <- max(z1, z2) + 0.001
    zmn <- min(z1, z2) - 0.001

    # optimization, we could skip this
    outx <- idf$xmx < xmn | xmx < idf$xmn
    outz <- idf$zmx < zmn | zmx < idf$zmn
    idf <- idf[!(outx | outz),]
    ntofilt <- sum(outx | outz)
    #print(sprintf("ntofilt:%d",ntofilt))

    nisect <- nrow(idf)

    A2 <- z2 - z1
    B2 <- x1 - x2
    C2 <- A2 * x1 + B2 * z1

    idf$det <- idf$A1 * B2 - A2 * idf$B1
    idf <- idf[idf$det != 0,]
    idf$xi <- (B2 * idf$C1 - idf$B1 * C2) / idf$det
    idf$zi <- (idf$A1 * C2 - A2 * idf$C1) / idf$det


    # the intersection has to be on both lines
    in1 <- (idf$xmn <= idf$xi & idf$xi <= idf$xmx) & (idf$zmn <= idf$zi & idf$zi <= idf$zmx)
    in2 <- (xmn <= idf$xi & idf$xi <= xmx) & (zmn <= idf$zi & idf$zi <= zmx)
    idf <- idf[in1 & in2,]
    # idf <- idf[ in1, ]

    #print(sprintf("nbdf:%d - nisect:%d",nbdf,nisect))
    isolate({
      isectStats$nIsect = isectStats$nIsect + nisect
      isectStats$nIsectToFilt = isectStats$nIsectToFilt + ntofilt
      isectStats$nbdftot = isectStats$nbdftot + nbdf
    })
    idf
  }
  getParametersDf <- reactive({
    nang <- input$anglesteps
    mclo <- input$montecarloiters
    gsiz <- input$gridsize
    hex <- as.numeric(input$hexgrid)
    itrk <- curTrackIdx()
    pdf <- data.frame(anglesteps = nang, montecarloiters=mclo,gridsize=gsiz,hexgrid=hex,curtrack=itrk)
    pdf
  })
  getSegmentedTrackDf <- reactive({  
    ctrk <- curTrack() ## Here it is!
    trktime <- input$tracktime
    tdf <- ctrk$tdf
    if (!is.null(trktime)) {
      tdf <- getTrackAtTime(trktime, ctrk)
    }
    orgx <- 0
    orgz <- 0
    nangstep <- input$anglesteps
    langstep <- nangstep-1
    tsdf <- data.frame()
    for (aid in 0:langstep) {
      ang <- 2*pi *aid/ nangstep
      s <- sin(ang)
      c <- cos(ang)
      xx <- c * tdf$pos.x - s * tdf$pos.z + orgx
      zz <- s * tdf$pos.x + c * tdf$pos.z + orgz
      x2 <- xx[1]
      z2 <- zz[1]
      for (i in 2:length(xx)) {
        x1 <- x2
        x2 <- xx[i]
        z1 <- z2
        z2 <- zz[i]
        sid <- i-1
        ts1df <- data.frame(aid=aid,sid=sid,x1=x1,z1=z1,x2=x2,z2=z2)
        tsdf <- rbind(tsdf, ts1df)
      }
    }
    tsdf
  })
  calcIntersByTrackDfxz <- function(tid, lid, aid, xv, zv) {
    bdf <- getBlockersDf()
    # loop initialization and loop
    x2 <- xv[1]
    z2 <- zv[1]
    idf <- data.frame()
    for (i in 2:length(xv)) {
      x1 <- x2
      x2 <- xv[i]
      z1 <- z2
      z2 <- zv[i]
      i1df <- calcFunIntersBySegDf(bdf,i-1,x1, x2, z1, z2)
      idf <- rbind(idf, i1df)
    }

    #print(sprintf("lid:%d tid:%d sid:%d nrow:%d",lid,tid,sid,nrow(idf)))
    if (nrow(idf) == 0) return(data.frame())
    idf <- cbind(lid = lid, tid = tid, aid = aid, idf)
    idf
  }
  calcCurTrackIntersectionsDf <- reactive({
    ctrk <- curTrack()  
    trktime <- input$tracktime
    tdf <- ctrk$tdf
    if (!is.null(trktime)){
       tdf <- getTrackAtTime(trktime,ctrk)
    }
    xv <- ctrk$org.x + ctrk$tdf$pos.x
    zv <- ctrk$org.z + ctrk$tdf$pos.z
    calcIntersByTrackDfxz(curTrackIdx(), 0, 0, xv, zv)
  })

  removeDoors <- reactive({ "opendoors" %in% input$calcopts })

  getBlockerSegDf <- function(lldf) {
    n <- nrow(lldf)
    nm1 <- n - 1
    x1 <- lldf$x[1:nm1]
    x2 <- lldf$x[2:n]
    z1 <- lldf$z[1:nm1]
    z2 <- lldf$z[2:n]
    c1 <- as.character(lldf$class[1:nm1])
    c2 <- as.character(lldf$class[2:n])
    r1df <- data.frame(x1 = x1, z1 = z1, x2 = x2, z2 = z2, c1 = c1, c2 = c2, stringsAsFactors = F)
    return(r1df)
  }

  getBlockersDf <- reactive({

    lstdf <- list()
    #walls
    wdf <- glb$fpln$wdf
    if (!is.na(wdf) && nrow(wdf) > 0) {
      #print(sprintf("bg - walls:%d", nrow(wdf)))
      for (i in 1:nrow(wdf)) {
        ptsdflistidx <- wdf$ptidx[[i]]
        lstdf[[length(lstdf) + 1]] <- getBlockerSegDf(glb$fpln$ptsdflist[[ptsdflistidx]])
      }
    }
    #rooms
    rdf <- glb$fpln$rdf
    if (!is.na(rdf) && nrow(rdf) > 0) {
      #print(sprintf("bg - rooms:%d", nrow(rdf)))
      for (i in 1:nrow(rdf)) {
        ptsdflistidx <- rdf$ptidx[i]
        lstdf[[length(lstdf) + 1]] <- getBlockerSegDf(glb$fpln$ptsdflist[[ptsdflistidx]])
      }
    }
    bdf <- dplyr::bind_rows(lstdf)
    nbef <- nrow(bdf)

    if (removeDoors()) {
      idx <- grepl("w$", bdf$c1) & grepl("^w", bdf$c2)
      #print(sum(idx))
      bdf <- bdf[idx,]
      naft <- nrow(bdf)
      #print(sprintf("Blockers reduced from %d to %d by doors", nbef, naft))
    } else {
      #print(sprintf("Blockers %d - no door reduction ", nbef))
    }

    # add variables for intersection calculations
    bdf$A1 <- bdf$z2 - bdf$z1
    bdf$B1 <- bdf$x1 - bdf$x2
    bdf$C1 <- bdf$A1 * bdf$x1 + bdf$B1 * bdf$z1

    # millimeter tolerance on both sides
    bdf$xmn <- pmin(bdf$x1, bdf$x2) - 0.001
    bdf$xmx <- pmax(bdf$x1, bdf$x2) + 0.001
    bdf$zmn <- pmin(bdf$z1, bdf$z2) - 0.001
    bdf$zmx <- pmax(bdf$z1, bdf$z2) + 0.001

    bdf <- cbind(idx = 1:nrow(bdf), bdf)
  })
  
  penalizeVals <- function(ival) {
    firstpen <- 2
    incpen <- 2
    rval <- 1
    if (ival != 0) {
      rval <- firstpen + ival * incpen
    }
    rval
  }
  calcLocProbsFromInters <- function() {

    isectvek <- list()
    ldf <- glbcirc$locdf
    ldf <- ldf[glbcirc$locdf$act,]
    cidf <- glbcirc$cidf
    for (i in 1:nrow(ldf)) {
      idx <- ldf$idx[i]
      isectvek[[i]] <- sum(cidf$lid == idx)
    }
    isectvek <- unlist(isectvek)
    #print(isectvek)
    probvek <- 1 / sapply(isectvek, penalizeVals)
    #print(round(probvek,4))
    glbcirc$locdf$prob <- 0
    glbcirc$locdf$prob[ldf$idx] <- probvek
    mprob <- max(glbcirc$locdf$prob)
    glbcirc$locdf$mprob <- glbcirc$locdf$prob / mprob
  }

  calcLikelihoodFromInters <- function() {
    # calculate likelihood
    print("calcLikelihoodFromInters")
   
    sumcidf <- glbcirc$sumcidf 

    ilikedf <- sumcidf %>% mutate(idx = lid, aidx = aid + 1) %>%
                           ungroup(lid, aid) %>%
                           mutate(lid = NULL, aid = NULL)

    bldf <- bayes$pLike
    bldf$ninters <- NULL

    pbdf <- left_join(bldf, ilikedf, by = c("idx", "aidx")) %>%
                  mutate(ninters = ifelse(is.na(ninters), 0, ninters)) %>%
                  arrange(idx, aidx)
    probvek <- 1 / sapply(pbdf$ninters, penalizeVals)
    sumprob <- sum(probvek)
    probvek <- probvek / sumprob
    mprob <- max(probvek)
    pbdf$prob <- probvek
    pbdf$mprob <- probvek / mprob
    bayes$pLike <- pbdf
  }
  calcPosteriors <- function() {
    # calculate likelihood
    print("Calculating Posteriors")
    probvek <- bayes$pPrio$prob*bayes$pLike$prob
    # normalize
    sumprob <- sum(probvek)
    probvek <- probvek/sumprob
    mprob <- max(probvek)
    bayes$pPost$prob <- probvek
    bayes$pPost$mprob <- probvek / mprob
  }

  findIntersByLocAng <- function(tid, lid, aid, trk, orgx, orgz, ang) {
    tdf <- trk$tdf
    idx <- trk$t.min <= tdf$t & tdf$t <= trk$t.max
    nptb <- nrow(tdf)
    tdf <- tdf[idx,]
    npta <- nrow(tdf)
    s <- sin(ang)
    c <- cos(ang)
    xx <- c * tdf$pos.x - s * tdf$pos.z + orgx
    zz <- s * tdf$pos.x + c * tdf$pos.z + orgz
    #  print(sprintf("renderTrack nptbef:%d nptaft:%d -  org: %2f %2f %2f",nptb,npta,orgx,orgz,ang))
    calcIntersByTrackDfxz(tid, lid, aid, xx, zz)
  }

  createRunDir <- function() {
    if (!file.exists("Runs")) {
      dir.create("Runs")
    }
    timeofday <- Sys.time()
    stod <- sprintf(format(timeofday, "Runs/%Y%m%dT%H%M%S"))
    dir.create(stod)
    stod
  }
  writeIsectInputFiles <- function(pdf,ldf, bdf, tdf, tsdf) {
    ldf <- ldf[, c("idx","x", "y")]
    colnames(ldf) <- c("idx","x","z")
    dname <- createRunDir()
    fname <- sprintf("%s/%s", dname, "pdf.csv")
    write.csv(pdf, fname, row.names = F)
    fname <- sprintf("%s/%s", dname, "ldf.csv")
    write.csv(ldf, fname, row.names=F)
    fname <- sprintf("%s/%s", dname, "bdf.csv")
    write.csv(bdf, fname, row.names = F)
    fname <- sprintf("%s/%s", dname, "tdf.csv")
    tdf <- tdf[, c("t", "pos.x", "pos.z")]
    write.csv(tdf, fname, row.names = F)
    fname <- sprintf("%s/%s", dname, "tsdf.csv")
    write.csv(tsdf, fname, row.names = F)
    n0 <- nrow(pdf)
    n1 <- nrow(ldf)
    n2 <- nrow(bdf)
    n3 <- nrow(tdf)
    n4 <- nrow(tsdf)
    print(sprintf("writeIsectInputFiles - rows - pdf:%d ldf:%d bdf:%d tdf:%d tsdf:%d",n0,n1,n2,n3,n4))
    return(dname)
  }
  writeIsectOutputFiles <- function(dname,ltxt,cidf) {
    fname <- sprintf("%s/%s", dname, "log.txt")
    writeLines(unlist(ltxt),fname)
    fname <- sprintf("%s/%s", dname, "cidf.csv")
    write.csv(cidf, fname, row.names = F)
    n1 <- length(ltxt)
    n2 <- nrow(cidf)
    print(sprintf("writeIsectOutputFiles - rows - ltxt:%d cidf:%d",n1,n2))
  }
  ctiloglist <- list()
  initctilog <- function() {
    ctiloglist <<- list()
  }
  ctilog <- function(msg, printit = T) {
    # appending a space seems to be the best way to deal with failure
    ctiloglist[length(ctiloglist)+1] <<- paste0(msg," ") 
    if (printit) {
      print(msg)
    }
  }

  calcTrackIntersectionsInBatchCpp <- function(dname) {
    starttime <- Sys.time()
    print("Starting batchcpp - Going over the top")

    ofname <- "out.txt"
    ername <- "err.txt"
    args <- "./"
    if ("writerinters" %in% input$calcopts){
      args <- paste(args,"writeinters")
    }
    cmd <- "cintercpu1.exe"
    savedir <- getwd()
    setwd(dname)
    print(dname)
    res <- system2(cmd,args=args,stdout=ofname,stderr=ername)
    print(sprintf("Back res:%d",res))

    cidf <- read.csv("rdf.csv")
    sumcidf <- read.csv("grdf.csv")
    setwd(savedir) # restore original directory
    elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
    msg1 <- sprintf("calcTrackIntersectionsInBatchCpp finished -  elap:%.1f secs",elap)
    ctilog(msg1)

    glbcirc$cidf <- cidf
    glbcirc$sumcidf <-  sumcidf
    rv <- list()
    rv$ok <- TRUE
    rv$cidf <- cidf
    rv$sumcidf <- sumcidf
    return(rv)
  }

  calcTrackIntersectionsInR <- function(pdf,cdf,bdf,tdf,tsdf) {
    starttime <- Sys.time()
    trk <- curTrack()  
    trktime <- input$tracktime
    if (!is.null(trktime)){
       trk$tdf <- getTrackAtTime(trktime,trk)
    }
    tid <- curTrackIdx()
    selall <- "selcircsall" %in% input$debehave
    nang <- glbcirc$nang
    cidf <- data.frame()
    for (i in 1:nrow(cdf)) {
      xorg <- cdf$x[i]
      yorg <- cdf$y[i]
      lid <- cdf$idx[i]
      if (selall | cdf$sel[i]) {
        for (aid in 0:(nang - 1)) {
          ang <- aid * 2 * pi / nang
          c1idf <- findIntersByLocAng(tid, lid, aid, trk, xorg, yorg, ang)
          cidf <- rbind(cidf, c1idf)
        }
      }
    }
    glbcirc$cidf <- cidf
    elap <- as.numeric((Sys.time() - starttime)[1], units = "secs")
    ncrc <- nrow(cdf)
    ntrk <- glbcirc$nang * ncrc
    nseg <- ntrk * (length(trk$tdf) - 1)
    nhit <- nrow(cidf)
    isectPsec <- isectStats$nIsect / max(0.01, elap)
    isectStats$elap <- elap
    isectStats$ncrc <- ncrc
    isectStats$ntrk <- ntrk
    isectStats$nseg <- nseg
    isectStats$nhit <- nhit
    msg1 <- sprintf("calcTrackIntersections finished - circles:%d tracks:%d  segs:%d  hits:%d", isectStats$ncrc, isectStats$ntrk, isectStats$nseg, isectStats$nhit)
    msg2 <- sprintf("    isects:%.0f filts:%d pot:%d", isectStats$nIsect, isectStats$nIsectToFilt, isectStats$nbdftot)
    msg3 <- sprintf("    elap:%.1f secs  - isects:%.0f isects/sec:%.1f", isectStats$elap, isectStats$nIsect, isectPsec)
    ctilog(msg1)
    ctilog(msg2)
    ctilog(msg3)
    sumcidf <- cidf %>% group_by(lid, aid) %>%  summarize(ninters = n())    
    glbcirc$cidf <- cidf
    glbcirc$sumcidf <-  sumcidf
    rv <- list()
    rv$ok <- TRUE
    rv$cidf <- cidf
    rv$sumcidf <- sumcidf
    return(rv)
  }

  calcTrackIntersections <- function() {
    initctilog()
    starttime <- Sys.time()

    initIsectStats()
    cdf <- getActiveLocsDf()
    if (nrow(cdf) == 0){
      print("No locations selected - exiting without calculating intersections")
      return;
    }
    ctilog(sprintf("calcTrackIntersections - cdf has %d locs - starttime:%s", nrow(cdf), starttime))
    ctilog("locRTfilter:", printit = F)
    ctilog(paste(input$locRTfilter,sep=","),printit=F)
    ctilog("locRoomfilter:",printit=F)
    print(input$locRoomfilter)
    ctilog(paste(input$locRoomfilter, sep = ","), printit = F)
    ctilog(sprintf("anglesteps:%d",input$anglesteps), printit = F)
    ctilog(sprintf("montecarloiters:%d", input$montecarloiters), printit = F)

    # Write input files
    trk <- curTrack()  
    tdf <- trk$tdf
    bdf <- getBlockersDf()
    tsdf <- getSegmentedTrackDf()
    pdf <- getParametersDf()
    dname <- writeIsectInputFiles(pdf,cdf,bdf,tdf,tsdf)

    # Do interesection calculations - looping over cdf
    if (input$isectcalcmeth=="purer"){
      res <- calcTrackIntersectionsInR(pdf,cdf,bdf,tdf,tsdf)
    } else if (input$isectcalcmeth=="batchcpp") {
      res <- calcTrackIntersectionsInBatchCpp(dname)
    } else if (input$isectcalcmeth=="batchcuda") {
      print("Not yet implemented")
      res <- list()
      res$ok <- F
    } else if (input$isectcalcmeth=="callcpp"){
      print("Not yet implemented")
      res <- list()
      res$ok <- F
    } else if (input$isectcalcmeth=="callcuda"){
      print("Not yet implemented")
      res <- list()
      res$ok <- F
    }

    # Write output files
    if (res$ok){
      ltxt <- ctiloglist
      cidf <- glbcirc$cidf
      writeIsectOutputFiles(dname,ltxt,cidf)
    }
  }
  calcLikelihoodFromXYposition <- function(ddf){
    print(sprintf("xorg: %.1f  yorg: %.1f",ddf$x[1],ddf$y[1]))
    lkdf <- bayes$pLike
    distvek <- rep(0,nrow(lkdf))
    for(i in 1:nrow(lkdf)){
      xp <- lkdf$x[i]
      yp <- lkdf$y[i]
      dist <- max(0.5,sqrt((xp-ddf$x[1])*(xp-ddf$x[1]) + (yp-ddf$y[1])*(yp-ddf$y[1])))
      ddf$dist <- max(0.5,sqrt((xp-ddf$x)*(xp-ddf$x) + (yp-ddf$y)*(yp-ddf$y)))
      dist <- min(ddf$dist)
      prb <- 1/dist
      #print(sprintf("i:%d xp: %.1f  yp: %.1f  dist:%.2f   prb:%.3f",i,xp,yp,dist,prb))
      distvek[i] = dist
    }
    locorg <- input$beakdist
    locstd <- input$beakstdev
    probvek <- dnorm((distvek-locorg)/locstd)
    sumprob <- sum(probvek)
    probvek <- probvek / sumprob
    mprob <- max(probvek)
    lkdf$prob <- probvek
    lkdf$mprob <- probvek / mprob 
    bayes$pLike <- lkdf
  }
  calcLikelihoodFromLoc <- function(){
    print("calcLikelihoodFromLoc")
    idx <- edlinrv$lastcidx
    locdf <- glbcirc$locdf
    ddf <- data.frame( x=locdf$x[idx], y=locdf$y[idx])
    calcLikelihoodFromXYposition(ddf)
  }
  calcLikelihoodFromBeak <- function(){
    print("calcLikelihoodFromBeak")
    idx <- glb$lastBeakIdx
    bkdf <- glb$fpln$bkdf
    ddf <- data.frame( x=bkdf$x[idx], y=bkdf$y[idx])
    calcLikelihoodFromXYposition(ddf)
  }
  observeEvent(input$addtrack, {
   calcTrackIntersections()
    #calcLocProbsFromInters() - don't really want this anymore
   calcLikelihoodFromInters()
   calcPosteriors()
  })
  observeEvent(input$addbeacon, {
   calcLikelihoodFromBeak()
   calcPosteriors()
  })
  observeEvent(input$addroom, {
   calcLikelihoodFromLoc()
   calcPosteriors()
  })
  observeEvent(input$addloc, {
   calcLikelihoodFromLoc()
   calcPosteriors()
  })

  initIsectStats <- function() {
    isectStats$nIsect <- 0
    isectStats$nIsectToFilt <- 0
    isectStats$nbdftot <- 0
  }

## Floor Plan Output ---

  output$floorplan = renderPlot({
    selout <- T
    print("-renderPlot")

    initIsectStats()

    edlinrv$regencnt
    selected <- c(input$fpvis,input$byvis,input$airvis,input$elcvis)
    print(selected)

    # Now do the plot
    fpp <- list()
    fpp$imm <- list()
    if ("bitmap" %in% selected) {
      fpp$imm$img <- readPNG(zooomrv$fname)
    }
    if ("grid" %in% selected) {
      fpp$imm$grid <- TRUE
    }

    fpp$imm$xlim <- zooomrv$xlim
    fpp$imm$ylim <- zooomrv$ylim
    fpp$imm$zoomxlim <- zooomrv$zoomxlim
    fpp$imm$zoomylim <- zooomrv$zoomylim
    fpp$imm$framewid <- 1
    fpp$imm$framecolor <- "green"
    if ("nodes" %in% selected) {
      fpp$nodes <- list()
      fpp$nodes$ndf <- glb$fpln$ndf
      fpp$nodes$color <- "blue"
    }
    if ("links" %in% selected) {
      fpp$links <- list()
      fpp$links$ldf <- glb$fpln$ldf
      fpp$links$color <- "pink"
      fpp$links$width <- 1
    }
    if ("tracks" %in% selected) {
      if (input$trkvis == "sel-track") {
        fpp$tracks <- glb$tracks
        fpp$tracks$track <- glb$tracks$track[as.numeric(input$trackidx)] # don't replace, needs list
        fpp$tracks$width <- 2
        fpp$tracks$tracktime <- input$tracktime
      } else if (input$trkvis == "tracks") {
        fpp$tracks <- glb$tracks
        fpp$tracks$width <- 1
      } else if (input$trkvis == "pctrak") {
        fpp$tracks <- glb$tracks
        fpp$tracks$track <- glb$tracks$track[as.numeric(input$trackidx)] # don't replace, needs list
        fpp$tracks$width <- 1
      }
    }
    if ("rooms" %in% selected) {
      if (selout) print("rooms selected")
      fpp$rooms$rdf <- glb$fpln$rdf
      fpp$rooms$ptsdflist <- glb$fpln$ptsdflist
      fpp$rooms$width <- 2
      fpp$rooms$color <- "purple"
    }
    if ("roomnums" %in% selected) {
      if (selout) print("roomnums selected")
      fpp$roomnums$rdf <- glb$fpln$rdf
      fpp$roomnums$ptsdflist <- glb$fpln$ptsdflist
      fpp$roomnums$cex <- 0.9
      fpp$roomnums$color <- "purple"
    }
    if ("roomnames" %in% selected) {
      if (selout) print("roomnames selected")
      fpp$roomnames$rdf <- glb$fpln$rdf
      fpp$roomnames$ptsdflist <- glb$fpln$ptsdflist
      fpp$roomnames$cex <- 0.9
      fpp$roomnames$color <- "purple"
    }
    if ("walls" %in% selected) {
      if (selout) print("walls selected")
      fpp$walls$wdf <- glb$fpln$wdf
      fpp$walls$ptsdflist <- glb$fpln$ptsdflist
      fpp$walls$width <- 2
      fpp$walls$color <- "blue"
    }
    if ("beacons" %in% selected) {
      if (selout) print("beacons selected")
      fpp$beacons$bkdf <- glb$fpln$bkdf
      fpp$beacons$width <- 2
      fpp$beacons$color <- "blue"
    }
    if ("blockers" %in% selected) {
      if (selout) print("blockers selected") 
      fpp$blockers$bdf <- getBlockersDf()
      fpp$blockers$width <- 2
      fpp$blockers$color <- "darkblue"
    }
    if ("ninters" %in% selected) {
      if (selout) print("ninters selected")
      fpp$ninters$idf <- calcCurTrackIntersectionsDf()
      fpp$ninters$width <- 2
      fpp$ninters$color <- "darkgreen"
    }
    if ("binters" %in% selected) {
      if (selout) print("binters selected")
      fpp$cinters$idf <- glbcirc$cidf
      fpp$cinters$width <- 2
      fpp$cinters$color <- "darkgreen"
    }
    if ("edlin" %in% selected) {
      if (selout) print("edlin selected")
      fpp$edlin <- list()
      fpp$edlin$pts <- edlinrv$pts
      fpp$edlin$color <- "blue"
      fpp$edlin$width <- 1
    }
#    if (sum((c("locmarks", "pcwt") %in% selected)) > 0) {
    if ("locmarks" %in% selected){
      if (selout) print("locmarks selected")     
      fpp$pcircs <- list()
      fpp$pcircs$ldf <- getActiveLocsDf()
      fpp$pcircs$rad <- input$gridsize * 0.9 / 2
      fpp$pcircs$color <- "pink"
      fpp$pcircs$ltracks <- ("ltracks" %in% selected)
      fpp$pcircs$track <- curTrack()
      fpp$pcircs$anglesteps <- glbcirc$nang
      fpp$pcircs$selcircsall <- "selcircsall" %in% input$debehave
    }
    if ("airflow" %in% selected) {
      #pcircdf <- data.frame(x=c(0,1),y=c(0,1),prob=0.5)
      fpp$airflow$adf <- glb$airdf
      fpp$airflow$width <- 3
      fpp$airflow$color <- "darkgreen"
      fpp$airflow$colortxt <- "darkgreen"
      fpp$airflow$colorfill <- "blue"
      fpp$airflow$cex <- 1.0
      fpp$airflow$donames <- "afnames" %in% selected
      fpp$airflow$dointakes <- "afintakes" %in% selected
      fpp$airflow$domconds <- "afmduct" %in% selected
      fpp$airflow$dofconds <- "affduct" %in% selected
      fpp$airflow$dovents <- "afvents" %in% selected
      fpp$airflow$dovavs <- "afvavs" %in% selected
    }
    if ("electric" %in% selected) {
      #pcircdf <- data.frame(x=c(0,1),y=c(0,1),prob=0.5)
      fpp$electric$edf <- glb$elcdf
      fpp$electric$width <- 3
      fpp$electric$color <- "red"
      fpp$electric$colortxt <- "darkred"
      fpp$electric$colorfill <- "yellow"
      fpp$electric$cex <- 1.0
      fpp$electric$donames <- "elnames" %in% selected
      fpp$electric$dosource <- "elsource" %in% selected
      fpp$electric$domconds <- "elmcond" %in% selected
      fpp$electric$dofconds <- "elfcond" %in% selected
      fpp$electric$dojuncs <- "eljuncs" %in% selected
    }
    if ("bayes" %in% selected) {
      if (selout) print("bayes selected")     
      fpp$bayes <- list()
      fpp$bayes$bcdf <- getDisplayBayesDf()
      fpp$bayes$ldf <- getActiveLocsDf()
      fpp$bayes$rad <- input$gridsize * 0.9 / 2
      fpp$bayes$color <- getDisplayBayesClr()
      fpp$bayes$btracks <- ("btracks" %in% selected)
      fpp$bayes$maxisect <- input$maxisect
      fpp$bayes$track <- curTrack()
      fpp$bayes$tracktime <- input$tracktime
      fpp$bayes$anglesteps <- glbcirc$nang
      fpp$bayes$selcircsall <- "selcircsall" %in% input$debehave
    }
    if (selout) print("making FloorPlot")
    makeFloorPlot(fpp)
  }, height = heit)

  output$plotclick <- renderText({
    x <- edlinrv$lastclickx
    y <- edlinrv$lastclicky
    np <- length(edlinrv$pts)
    cidx <- edlinrv$lastcidx
    msg <- sprintf("x: %7.3f   y: %7.3f  np: %d  - cidx:%d", x, y, np, cidx)
    msg
  })

})
shinyApp(u, s)