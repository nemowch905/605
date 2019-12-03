rm(list=ls())
library("MASS")
library("plotrix")
library("astro")

args = (commandArgs)
dir = getwd()
cb58 = read.fitstab(paste(dir,"/cB58_Lyman_break.fit",sep = ""))
setwd(paste(dir,"/data",sep = ""))
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
write.csv(data,file = "hw2.csv",row.names = F)


