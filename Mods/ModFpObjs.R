# Room functions ---
addRoomToJsonList <- function(roomlist, newroom) {
  if (is.null(roomlist)) {
    roomlist <- list(list = list())
  }
  nl <- length(roomlist$list)
  roomlist$list[nl + 1] <- list(newroom)
  return(roomlist)
}

getCurUtcTimeStr <- function() {
  t <- Sys.time()
  attr(t, "tzone") <- "UTC"
  format(t, "%Y-%m-%d %H:%M:%S %Z")
}
getCurUtcTimeStrNoSpaces <- function() {
  t <- Sys.time()
  attr(t, "tzone") <- "UTC"
  format(t, "%Y%m%dT%H%M%S%Z")
}
makeNewRoom <- function(roomnum, roomname = "",roomtype="OpenArea") {

  newroom <- list()
  newroom[["num"]] <- roomnum
  newroom[["name"]] <- roomname
  newroom[["roomtype"]] <- roomname
  newroom[["r-created"]] <- getCurUtcTimeStr()
  #  newroom[["pts"]] <- list(list("list"=list()))
  newroom[["pts"]] <- list()
  return(newroom)
}

addPtToRoom <- function(room, pt, ptclass) {
  nl <- length(room[["pts"]]$list)
  room[["pts"]]$list[nl + 1] <- list(list("class" = ptclass, "pt" = pt))
  return(room)
}

# Wall functions ---
addWallToJsonList <- function(walllist, newwall) {
  if (is.null(walllist)) {
    walllist <- list(list = list())
  }
  nl <- length(walllist$list)
  walllist$list[nl + 1] <- list(newwall)
  return(walllist)
}

makeNewWall <- function(wallname) {
  t <- Sys.time()
  attr(t, "tzone") <- "UTC"
  newwall <- list()
  newwall[["name"]] <- wallname
  newwall[["r-created"]] <- format(t, "%Y-%m-%d %H:%M:%S %Z")
  newwall[["pts"]] <- list()
  return(newwall)
}

addPtToWall <- function(wall, pt, ptclass) {
  nl <- length(wall[["pts"]]$list)
  wall[["pts"]]$list[nl + 1] <- list(list("class" = ptclass, "pt" = pt))
  return(wall)
}

getWallFromWdf <- function(wdf, seekname) {
  idx <- which(wdf$name == seekname)
  if (is.null(idx)) return(NULL)
  rv <- list()
  rv$name <- wdf$name[[idx]]
  rv$ptidx <- wdf$ptidx[[idx]]
  return(rv)
}
