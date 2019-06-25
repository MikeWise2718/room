# filter the positions out and convert to csv

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

df2 <- readPosFromLog("birdblobs/hlbirdlog-20171020T115841UTC")
plot(df1$pos.x,df1$pos.z,type="l",col="red",xlim=c(-20,30),ylim=c(-10,35))
lines(df2$pos.x,df2$pos.z,col="blue")