rm(list=ls())

args = (commandArgs(trailingOnly=TRUE))
if(length(args) == 2){
  temp = args[1]
  folder = args[2]
} else {
  cat('usage: Rscript hw4.R <template spectrum> <data directory.\n', file=stderr())
  stop()
}
if (require("astro")) {
  print("loaded package astro")
} else {
  print("failed to load package astro")
}
dir = getwd()
cb58 = read.fitstab(paste(dir,"/",temp,sep = ""))
setwd(paste(dir,"/",folder,sep = ""))
cb58[,2] = scale(cb58[,2])
files=list.files()
distance = c()
shift = c()
for (i in 1:length(files)) {
  spec = read.fitstab(files[i])
  spec[,1] = scale(spec[,1])
  spec[spec[,4]!=0,1]=NA
  dis = c()
  for(j in 1:(dim(spec)[1]-dim(cb58)[1]+1)){
    cal = spec[j:(j+dim(cb58)[1]-1),1]
    dis[j] = sqrt(sum((cal-cb58[,2])^2,na.rm = T))/sum(!is.na(cal))
  }
  distance[i] = min(dis,na.rm = T)
  shift[i] = which(dis == min(dis,na.rm = T))
}
files = files[order(distance)]
distance = distance[order(distance)]
shift = shift[order(distance)]
data = data.frame(distance=distance,spectrumID=files,i=shift)
setwd(dir)
name = paste(folder,".csv",sep = "")
write.csv(data[1:100,],name,row.names = F)


