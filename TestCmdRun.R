dname <- "D:/R-Experiments/Room/Runs/20171228T175905"
ofname <- "out.txt"
ername <- "err.txt"
args <- "./"
setwd(dname)
cmd <- "dir"
cmd <- "cintercpu1.exe"
res <- system2(cmd,args=args,stdout=ofname,stderr=ername)
print(res)