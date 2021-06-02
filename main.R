library(ggplot2)
library(reshape2)
library(maptools)
library(png)
library(Matrix)
library(igraph)
library(GISTools)
library(Rfast)
library(rasterVis)
library(classInt)
library(dismo)
library(centiserve)


# Prepared data at voronoi level for Senegal
v <- read.csv("Data/vorInd.csv")


#########################################
##### Networks creation + distance matrix
#########################################


### Communication network
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

for(i in months){
  assign(paste("net",i,sep="_"),read.csv(paste("Data/resNetwork/res",i,".csv",sep="")))
}

net_01M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_01)){
  net_01M[net_01$Origin[i],net_01$Dest[i]] <- net_01$NUMCALLAV[i]
}
net_02M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_02)){
  net_02M[net_02$Origin[i],net_02$Dest[i]] <- net_02$NUMCALLAV[i]
}
net_03M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_03)){
  net_03M[net_03$Origin[i],net_03$Dest[i]] <- net_03$NUMCALLAV[i]
}
net_04M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_04)){
  net_04M[net_04$Origin[i],net_04$Dest[i]] <- net_04$NUMCALLAV[i]
}
net_05M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_05)){
  net_05M[net_05$Origin[i],net_05$Dest[i]] <- net_05$NUMCALLAV[i]
}
net_06M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_06)){
  net_06M[net_06$Origin[i],net_06$Dest[i]] <- net_06$NUMCALLAV[i]
}
net_07M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_07)){
  net_07M[net_07$Origin[i],net_07$Dest[i]] <- net_07$NUMCALLAV[i]
}
net_08M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_08)){
  net_08M[net_08$Origin[i],net_08$Dest[i]] <- net_08$NUMCALLAV[i]
}
net_09M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_09)){
  net_09M[net_09$Origin[i],net_09$Dest[i]] <- net_09$NUMCALLAV[i]
}
net_10M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_10)){
  net_10M[net_10$Origin[i],net_10$Dest[i]] <- net_10$NUMCALLAV[i]
}
net_11M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_11)){
  net_11M[net_11$Origin[i],net_11$Dest[i]] <- net_11$NUMCALLAV[i]
}
net_12M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(net_12)){
  net_12M[net_12$Origin[i],net_12$Dest[i]] <- net_12$NUMCALLAV[i]
}

netM <- net_01M+net_02M+net_03M+net_04M+net_05M+net_06M+net_07M+net_08M+net_09M+net_10M+net_11M+net_12M
netM <- netM/12
netG <- graph_from_adjacency_matrix(netM,mode="directed",weighted=T)


### Mobility network
mob <- read.csv(paste("Data/resMobility/res",months[1],".csv",sep=""))
mob_01M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_01M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[2],".csv",sep=""))
mob_02M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_02M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[3],".csv",sep=""))
mob_03M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_03M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[4],".csv",sep=""))
mob_04M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_04M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[5],".csv",sep=""))
mob_05M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_05M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[6],".csv",sep=""))
mob_06M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_06M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[7],".csv",sep=""))
mob_07M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_07M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[8],".csv",sep=""))
mob_08M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_08M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[9],".csv",sep=""))
mob_09M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_09M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[10],".csv",sep=""))
mob_10M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_10M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[11],".csv",sep=""))
mob_11M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_11M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}
mob <- read.csv(paste("Data/resMobility/res",months[12],".csv",sep=""))
mob_12M <- matrix(0,nrow=1666,ncol=1666)
for(i in 1:nrow(mob)){
  mob_12M[mob$homeSite[i],mob$callSite[i]] <- (5*mob$NUMUSERSWDAV[i]+2*mob$NUMUSERSWEAV[i])/7
}

mobM <- mob_01M+mob_02M+mob_03M+mob_04M+mob_05M+mob_06M+mob_07M+mob_08M+mob_09M+mob_10M+mob_11M+mob_12M
mobM <- mobM/12
mobG <- graph_from_adjacency_matrix(mobM,mode="directed",weighted=T)


### Distance matrix
sites <- read.csv("Data/sitesLoc.csv")

test <- rowSums(netM)
refNZ <- which(test != 0)
sitesNZ <- sites[refNZ,]
row.names(sitesNZ) <- 1:1587

coordsSites <- SpatialPoints(sitesNZ[, c('long', 'lat')])

dist <- pointDistance(coordsSites,lonlat = T)/1000
dist[is.na(dist)] <- 0
dist <- dist+t(dist)
write.csv(dist,"dist.csv")

lay = cbind(sitesNZ$long,sitesNZ$lat)


### Is rural ?

senCommune <- readOGR("Data/Senegal_Communes_552.shp")
senCommune$CODE552 <- as.numeric(as.character(senCommune$CODE552))

inters <- intersect(coordsSites,senCommune)

refRural <- which(inters$rural == 1)
refRural <- refRural[-which(v$dens[refRural]>1000)]


#########################
##### Centrality measures
#########################



display.brewer.all(colorblindFriendly = TRUE)
pal <- brewer.pal(n = 12, name = "Paired")

netMNZ <- netM[refNZ,refNZ]

# All & Rural vs All

hist(v$elec,main="all",breaks=20,col="gray85",ylim=c(0,350))
hist(v$elec[isRural],main="rural",breaks=20,col="gray85",ylim=c(0,350))
hist(v$elec[!isRural],main="urban",breaks=20,col="gray85",ylim=c(0,350))

s <- 0.0001 # Selects aggregation threshold in km

#test <- graph_from_adjacency_matrix(dist,weighted = T)
#testViz <- subgraph.edges(graph=test, eids=which(E(test)$weight<s),delete.vertices=F)
#comp <- components(testViz)
#cut <- comp$membership

#colC <- rep(0,1587)
#for(i in 1:1666){
#  if(comp$csize[cut[i]] == 1){
#    colC[i] <- "gray70"
#  }else{
#    colC[i] <- pal[(cut[i] %% 12) + 1]
#  }
#}
#plot(senOutline)
#plot(coordsSites,pch=20,col=alpha(colC,0.6),add=T)
#plot(testViz,
#     layout=lay,
#     vertex.size=7,vertex.color=alpha(colC,),vertex.label.cex = 0.2,vertex.label="",
#     edge.lty=0,
#     edge.arrow.mode=0,
#     add=T,rescale=F)

corTableFull <- function(s){
  test <- graph_from_adjacency_matrix(dist,weighted = T)
  testViz <- subgraph.edges(graph=test, eids=which(E(test)$weight<s),delete.vertices=F)
  comp <- components(testViz)
  cut <- comp$membership
  len <- comp$no
  test <- data.frame(ref = 1:1587, cut = cut)
  isRural <- rep(F,len)
  for(j in 1:len){
    if(all(which(test$cut == j) %in% refRural)){
      isRural[j] <- T
    }
  }
  length(which(isRural))
  lonC <- rep(0,len)
  latC <- rep(0,len)
  for(i in 1:len){
    lonC[i] <- mean(sitesNZ$long[which(cut == i)])
    latC[i] <- mean(sitesNZ$lat[which(cut == i)])
  }
  coordsC <- SpatialPoints(cbind(lonC,latC))
  newDist <- pointDistance(coordsC,lonlat = T)/1000
  newDist[is.na(newDist)] <- 0
  newDist <- newDist+t(newDist)
  M <- matrix(NA,nrow=len,ncol = 1587)
  for(j in 1:len){
    if(length(which(cut == j)) > 1){
      M[j,] <- colSums(netMNZ[which(cut == j),])
    }else{
      M[j,] <- netMNZ[which(cut == j),]
    }
  }
  MM <- matrix(NA,nrow=len,ncol=len)
  for(j in 1:len){
    if(length(which(cut == j)) > 1){
      MM[,j] <- rowSums(M[,which(cut == j)])
    }else{
      MM[,j] <- M[,which(cut == j)]
    }
  }
  MMG <- graph_from_adjacency_matrix(MM,mode="directed",weighted=T)
  wealthbuff <- aggregate(v$combined*v$pop,by=list(cut),FUN="sum")
  elecRatebuff <- aggregate(v$elec*v$pop,by=list(cut),FUN="sum")
  popbuff <- aggregate(v$pop,by=list(cut),FUN="sum")
  resultbuff <- socioFeat(MMG,newDist)
  resultbuff$wealth <- wealthbuff$x/popbuff$x
  resultbuff$elecRate <- elecRatebuff$x/popbuff$x
  mean <- mean(resultbuff$elecRate)
  sd <- sd(resultbuff$elecRate)
  mean2 <- mean(resultbuff$elecRate[isRural])
  sd2 <- sd(resultbuff$elecRate[isRural])
  temp <- partCorC(resultbuff)
  temp2 <- partCorC(resultbuff[isRural,])
  return(list(temp,temp2,mean,sd,mean2,sd2))
}

test <- corTableFull(0.0001)
View(test[[1]])
View(test[[2]])

betwF <- rep(NA,17)
levF <- rep(NA,17)
socDivF <- rep(NA,17)
spaDivF <- rep(NA,17)
distaF <- rep(NA,17)
inOutF <- rep(NA,17)
betwFb <- rep(NA,17)
levFb <- rep(NA,17)
socDivFb <- rep(NA,17)
spaDivFb <- rep(NA,17)
distaFb <- rep(NA,17)
inOutFb <- rep(NA,17)
meanCF <- rep(NA,17)
sdCF <- rep(NA,17)
betwR <- rep(NA,17)
levR <- rep(NA,17)
socDivR <- rep(NA,17)
spaDivR <- rep(NA,17)
distaR <- rep(NA,17)
inOutR <- rep(NA,17)
betwRb <- rep(NA,17)
levRb <- rep(NA,17)
socDivRb <- rep(NA,17)
spaDivRb <- rep(NA,17)
distaRb <- rep(NA,17)
inOutRb <- rep(NA,17)
meanCR <- rep(NA,17)
sdCR <- rep(NA,17)

for(i in 1:17){
  resG <- corTableFull(c(0.0001,1:16)[i])
  res <- resG[[1]]
  res2 <- resG[[2]]
  meanCF[i] <- resG[[3]]
  sdCF[i] <- resG[[4]]
  meanCR[i] <- resG[[5]]
  sdCR[i] <- resG[[6]]
  betwF[i] <- res[2,1]
  levF[i] <- res[3,1]
  socDivF[i] <- res[4,1]
  spaDivF[i] <- res[5,1]
  distaF[i] <- res[6,1]
  inOutF[i] <- res[7,1]
  betwFb[i] <- res[2,3]
  levFb[i] <- res[3,3]
  socDivFb[i] <- res[4,3]
  spaDivFb[i] <- res[5,3]
  distaFb[i] <- res[6,3]
  inOutFb[i] <- res[7,3]
  betwR[i] <- res2[2,1]
  levR[i] <- res2[3,1]
  socDivR[i] <- res2[4,1]
  spaDivR[i] <- res2[5,1]
  distaR[i] <- res2[6,1]
  inOutR[i] <- res2[7,1]
  betwRb[i] <- res2[2,3]
  levRb[i] <- res2[3,3]
  socDivRb[i] <- res2[4,3]
  spaDivRb[i] <- res2[5,3]
  distaRb[i] <- res2[6,3]
  inOutRb[i] <- res2[7,3]
}

display.brewer.all(colorblindFriendly = TRUE)
pal <- brewer.pal(n = 6, name = "Dark2")

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,distaF,col=pal[5],lwd=2)
lines(0:16,betwF,col=pal[1],lwd=2)
lines(0:16,levF,col=pal[2],lwd=2)
lines(0:16,socDivF,col=pal[3],lwd=2)
lines(0:16,spaDivF,col=pal[4],lwd=2)
lines(0:16,inOutF,col=pal[6],lwd=2)
lines(0:16,sdCF/meanCF)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,distaFb,col=pal[5],lwd=2)
lines(0:16,betwFb,col=pal[1],lwd=2)
lines(0:16,levFb,col=pal[2],lwd=2)
lines(0:16,socDivFb,col=pal[3],lwd=2)
lines(0:16,spaDivFb,col=pal[4],lwd=2)
lines(0:16,inOutFb,col=pal[6],lwd=2)
lines(0:16,sdCF/meanCF)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,distaR,col=pal[5],lwd=2)
lines(0:16,betwR,col=pal[1],lwd=2)
lines(0:16,levR,col=pal[2],lwd=2)
lines(0:16,socDivR,col=pal[3],lwd=2)
lines(0:16,spaDivR,col=pal[4],lwd=2)
lines(0:16,inOutR,col=pal[6],lwd=2)
lines(0:16,sdCR/meanCR)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,distaRb,col=pal[5],lwd=2)
lines(0:16,betwRb,col=pal[1],lwd=2)
lines(0:16,levRb,col=pal[2],lwd=2)
lines(0:16,socDivRb,col=pal[3],lwd=2)
lines(0:16,spaDivRb,col=pal[4],lwd=2)
lines(0:16,inOutRb,col=pal[6],lwd=2)
lines(0:16,sdCR/meanCR)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))


# Rural vs Rural

s <- 15

corTable <- function(s){

test <- graph_from_adjacency_matrix(dist,weighted = T)
testViz <- subgraph.edges(graph=test, eids=which(E(test)$weight<s),delete.vertices=F)
comp <- components(testViz)
cut <- comp$membership
len <- comp$no

test <- data.frame(ref = 1:1587, cut = cut)
isRural <- rep(F,len)
for(j in 1:len){
  if(all(which(test$cut == j) %in% refRural)){
    isRural[j] <- T
  }
}
length(which(isRural))

lonC <- rep(0,len)
latC <- rep(0,len)
for(i in 1:len){
  lonC[i] <- mean(sitesNZ$long[which(cut == i)])
  latC[i] <- mean(sitesNZ$lat[which(cut == i)])
}

coordsC <- SpatialPoints(cbind(lonC,latC))
newDist <- pointDistance(coordsC,lonlat = T)/1000
newDist[is.na(newDist)] <- 0
newDist <- newDist+t(newDist)

M <- matrix(NA,nrow=len,ncol = 1587)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    M[j,] <- colSums(netMNZ[which(cut == j),])
  }else{
    M[j,] <- netMNZ[which(cut == j),]
  }
}
MM <- matrix(NA,nrow=len,ncol=len)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    MM[,j] <- rowSums(M[,which(cut == j)])
  }else{
    MM[,j] <- M[,which(cut == j)]
  }
}
MM <- MM[isRural,isRural]
newDist <- newDist[isRural,isRural]
MMG <- graph_from_adjacency_matrix(MM,mode="directed",weighted=T)
wealthbuff <- aggregate(v$combined*v$pop,by=list(cut),FUN="sum")
wealthbuff <- wealthbuff[isRural,]
elecRatebuff <- aggregate(v$elec*v$pop,by=list(cut),FUN="sum")
elecRatebuff <- elecRatebuff[isRural,]
popbuff <- aggregate(v$pop,by=list(cut),FUN="sum")
popbuff <- popbuff[isRural,]
resultbuff <- socioFeat(MMG,newDist)
resultbuff$wealth <- wealthbuff$x/popbuff$x
resultbuff$elecRate <- elecRatebuff$x/popbuff$x
mean <- mean(resultbuff$elecRate)
sd <- sd(resultbuff$elecRate)

temp <- partCorC(resultbuff)

return(list(temp,mean,sd))
}

deg <- rep(NA,17)
betw <- rep(NA,17)
lev <- rep(NA,17)
socDiv <- rep(NA,17)
spaDiv <- rep(NA,17)
dista <- rep(NA,17)
inOut <- rep(NA,17)
degb <- rep(NA,17)
betwb <- rep(NA,17)
levb <- rep(NA,17)
socDivb <- rep(NA,17)
spaDivb <- rep(NA,17)
distab <- rep(NA,17)
inOutb <- rep(NA,17)
meanC <- rep(NA,17)
sdC <- rep(NA,17)

for(i in 1:17){
  resG <- corTable(c(0.0001,1:16)[i])
  res <- resG[[1]]
  meanC[i] <- resG[[2]]
  sdC[i] <- resG[[3]]
  deg[i] <- res[1,1]
  betw[i] <- res[2,1]
  lev[i] <- res[3,1]
  socDiv[i] <- res[4,1]
  spaDiv[i] <- res[5,1]
  dista[i] <- res[6,1]
  inOut[i] <- res[7,1]
  degb[i] <- res[1,3]
  betwb[i] <- res[2,3]
  levb[i] <- res[3,3]
  socDivb[i] <- res[4,3]
  spaDivb[i] <- res[5,3]
  distab[i] <- res[6,3]
  inOutb[i] <- res[7,3]
}

display.brewer.all(colorblindFriendly = TRUE)
pal <- brewer.pal(n = 6, name = "Dark2")

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,dista,col=pal[5],lwd=2)
lines(0:16,betw,col=pal[1],lwd=2)
lines(0:16,lev,col=pal[2],lwd=2)
lines(0:16,socDiv,col=pal[3],lwd=2)
lines(0:16,spaDiv,col=pal[4],lwd=2)
lines(0:16,inOut,col=pal[6],lwd=2)
lines(0:16,sdC/meanC)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))

plot(0:16,deg,pch="",ylim=c(-1,1))
lines(c(0,16),c(0,0),lty=2)
lines(0:16,distab,col=pal[5],lwd=2)
lines(0:16,betwb,col=pal[1],lwd=2)
lines(0:16,levb,col=pal[2],lwd=2)
lines(0:16,socDivb,col=pal[3],lwd=2)
lines(0:16,spaDivb,col=pal[4],lwd=2)
lines(0:16,inOutb,col=pal[6],lwd=2)
lines(0:16,sdC/meanC)
legend("bottomright",c("Betweenness","Leverage","Soc. diversity","Spat. diversity","Avg. distance","In/Out","Sample sd/mean"),
       lwd=rep(2,7),col=c(pal[1:6],1),lty=rep(1,7),pch=rep(NA,7))

partCorC <- function(resultbuff){
  resultbuff <- na.omit(resultbuff)
  res <- data.frame(coefF=rep(NA,7),pValF=rep(NA,7),coefP=rep(NA,7),pValP=rep(NA,7))
  test <- lm(elecRate ~ wealth, data = resultbuff)
  test1 <- lm(degree ~ wealth, data = resultbuff)
  test2 <- lm(between ~ wealth, data = resultbuff)
  test3 <- lm(leverage ~ wealth, data = resultbuff)
  test4 <- lm(socDiv ~ wealth, data = resultbuff)
  test5 <- lm(spaDiv ~ wealth, data = resultbuff)
  test6 <- lm(dist ~ wealth, data = resultbuff)
  test7 <- lm(rInOut ~ wealth, data = resultbuff)
  res[1,1] <- cor.test(resultbuff$degree,resultbuff$elecRate,method="spearman")$estimate
  res[1,2] <- cor.test(resultbuff$degree,resultbuff$elecRate,method="spearman")$p.value
  res[2,1] <- cor.test(resultbuff$between,resultbuff$elecRate,method="spearman")$estimate
  res[2,2] <- cor.test(resultbuff$between,resultbuff$elecRate,method="spearman")$p.value
  res[3,1] <- cor.test(resultbuff$leverage,resultbuff$elecRate,method="spearman")$estimate
  res[3,2] <- cor.test(resultbuff$leverage,resultbuff$elecRate,method="spearman")$p.value
  res[4,1] <- cor.test(resultbuff$socDiv,resultbuff$elecRate,method="spearman")$estimate
  res[4,2] <- cor.test(resultbuff$socDiv,resultbuff$elecRate,method="spearman")$p.value
  res[5,1] <- cor.test(resultbuff$spaDiv,resultbuff$elecRate,method="spearman")$estimate
  res[5,2] <- cor.test(resultbuff$spaDiv,resultbuff$elecRate,method="spearman")$p.value
  res[6,1] <- cor.test(resultbuff$dist,resultbuff$elecRate,method="spearman")$estimate
  res[6,2] <- cor.test(resultbuff$dist,resultbuff$elecRate,method="spearman")$p.value
  res[7,1] <- cor.test(resultbuff$rInOut,resultbuff$elecRate,method="spearman")$estimate
  res[7,2] <- cor.test(resultbuff$rInOut,resultbuff$elecRate,method="spearman")$p.value
  res[1,3] <- cor.test(test1$residuals,test$residuals,method="spearman")$estimate
  res[1,4] <- cor.test(test1$residuals,test$residuals,method="spearman")$p.value
  res[2,3] <- cor.test(test2$residuals,test$residuals,method="spearman")$estimate
  res[2,4] <- cor.test(test2$residuals,test$residuals,method="spearman")$p.value
  res[3,3] <- cor.test(test3$residuals,test$residuals,method="spearman")$estimate
  res[3,4] <- cor.test(test3$residuals,test$residuals,method="spearman")$p.value
  res[4,3] <- cor.test(test4$residuals,test$residuals,method="spearman")$estimate
  res[4,4] <- cor.test(test4$residuals,test$residuals,method="spearman")$p.value
  res[5,3] <- cor.test(test5$residuals,test$residuals,method="spearman")$estimate
  res[5,4] <- cor.test(test5$residuals,test$residuals,method="spearman")$p.value
  res[6,3] <- cor.test(test6$residuals,test$residuals,method="spearman")$estimate
  res[6,4] <- cor.test(test6$residuals,test$residuals,method="spearman")$p.value
  res[7,3] <- cor.test(test7$residuals,test$residuals,method="spearman")$estimate
  res[7,4] <- cor.test(test7$residuals,test$residuals,method="spearman")$p.value
  return(res)
}


#####################
##### In/Out mobility
#####################

s <- 10

test <- graph_from_adjacency_matrix(dist,weighted = T)
testViz <- subgraph.edges(graph=test, eids=which(E(test)$weight<s),delete.vertices=F)
comp <- components(testViz)
cut <- comp$membership
len <- comp$no

test <- data.frame(ref = 1:1666, cut = cut)
isRural <- rep(F,len)
for(j in 1:len){
  if(all(which(test$cut == j) %in% refRural)){
    isRural[j] <- T
  }
}
length(which(isRural))

M <- matrix(NA,nrow=len,ncol = 1666)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    M[j,] <- colSums(mobM[which(cut == j),])
  }else{
    M[j,] <- mobM[which(cut == j),]
  }
}
MM <- matrix(NA,nrow=len,ncol=len)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    MM[,j] <- rowSums(M[,which(cut == j)])
  }else{
    MM[,j] <- M[,which(cut == j)]
  }
}
rInOut <- rep(NA,nrow(MM))
for(j in 1:nrow(MM)){
  if(sum(MM[j,]) > 0){
    rInOut[j] <- sum(MM[,j])/sum(MM[j,])
  }
}

elecRatebuff <- aggregate(voronoiC$elecRate*voronoiC$pop,by=list(cut),FUN="sum")
popbuff <- aggregate(voronoiC$pop,by=list(cut),FUN="sum")
elecRate <- elecRatebuff$x/popbuff$x

cor(rInOut,elecRate,method="spearman",use="pairwise.complete.obs")
cor(rInOut[isRural],elecRate[isRural],method="spearman",use="pairwise.complete.obs")

MM <- MM[isRural,isRural]
rInOut <- rep(NA,nrow(MM))
for(j in 1:nrow(MM)){
  if(sum(MM[j,]) > 0){
    rInOut[j] <- sum(MM[,j])/sum(MM[j,])
  }
}

cor(rInOut,elecRate[isRural],method="spearman",use="pairwise.complete.obs")



rInOut2 <- rep(NA,nrow(mobM))
for(j in 1:nrow(mobM)){
  if(sum(mobM[j,]) > 0){
    rInOut2[j] <- sum(mobM[,j])/sum(mobM[j,])
  }
}

cor(rInOut,voronoiC$elecRate,method="spearman",use="pairwise.complete.obs")

plot(rInOut,rInOut2)
plot(elecR)


##################################
####### GRAVITATION MODELS #######
##################################


distG <- graph_from_adjacency_matrix(dist,weighted = T)

#distGViz <- subgraph.edges(graph=distG,eids=which(E(distG)$weight<20),delete.vertices=F)
#plot(senOutline)
#plot(distGViz, layout = lay,vertex.size=1,vertex.label="",
#     rescale=F,add=T)
#hist(E(distGViz)$weight)

mobMViz <- mobM[refNZ,refNZ]
mobMViz[which(dist > 250, arr.ind = TRUE)] <- 0
mobMViz[which(mobMViz < 50, arr.ind = TRUE)] <- 0
mobGViz <- graph_from_adjacency_matrix(mobMViz,weighted = T)

#mobGViz <- subgraph.edges(graph=mobGViz,eids=which(E(distG)$weight<150),delete.vertices=F)
#mobGViz <- subgraph.edges(graph=mobGViz,eids=which(E(mobGViz)$weight>50),delete.vertices=F)

s <- 5

hc <- hclust(as.dist(dist), method="complete")
cut <- cutree(hc, h=s)
len <- max(cut)

test <- data.frame(ref = 1:1587, cut = cut)
isRural <- rep(F,len)
for(j in 1:len){
  if(all(which(test$cut == j) %in% refRural)){
    isRural[j] <- T
  }
}
length(which(isRural))

colC <- rep(0,1587)
for(i in 1:1587){
  if(length(which(cut == cut[i])) == 1){
    colC[i] <- "gray70"
  }else{
    colC[i] <- pal[(cut[i] %% 12) + 1]
  }
}
plot(senOutline)
plot(mobGViz,layout=lay,vertex.size=1,vertex.label="",vertex.shape='none',
     edge.arrow.size=0.1,edge.width=1,edge.color="black",edge.arrow.mode=0,
     add=T,rescale=F)
plot(coordsSites,pch=20,col=alpha(colC,0.8),add=T)

#plot(mobGViz,layout=lay,vertex.size=1,vertex.label="",edge.arrow.size=0.1,edge.width=1,edge.color="black",add=T)

dev.off()

lonC <- rep(0,len)
latC <- rep(0,len)
for(i in 1:len){
  lonC[i] <- mean(lay[which(cut == i),1])
  latC[i] <- mean(lay[which(cut == i),2])
}

coordsC <- SpatialPoints(cbind(lonC,latC))
newDist <- pointDistance(coordsC,lonlat = T)/1000
newDist[is.na(newDist)] <- 0
newDist <- newDist+t(newDist)

hist(communeC$elec)

M <- matrix(NA,nrow=len,ncol = 1587)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    M[j,] <- colSums(mobMViz[which(cut == j),])
  }else{
    M[j,] <- mobMViz[which(cut == j),]
  }
}
MM <- matrix(NA,nrow=len,ncol=len)
for(j in 1:len){
  if(length(which(cut == j)) > 1){
    MM[,j] <- rowSums(M[,which(cut == j)])
  }else{
    MM[,j] <- M[,which(cut == j)]
  }
}

habbuff <- aggregate(v$accommod*v$pop,by=list(cut),FUN="sum")
educbuff <- aggregate(v$education*v$pop,by=list(cut),FUN="sum")
employbuff <- aggregate(v$employ*v$pop,by=list(cut),FUN="sum")
combinedbuff <- aggregate(v$combined*v$pop,by=list(cut),FUN="sum")
elecRatebuff <- aggregate(v$elec*v$pop,by=list(cut),FUN="sum")
elecNewbuff <- aggregate(test2*v$pop,by=list(cut),FUN="sum")
elecOldbuff <- aggregate(test*v$pop,by=list(cut),FUN="sum")
popbuff <- aggregate(v$pop,by=list(cut),FUN="sum")
#areabuff <- aggregate(voronoiC$area,by=list(cut),FUN="sum")

plot(1:1587,test2-test)

mean(test2-test)
mean(v$night-test2)
mean(v$night-test)

View(v@data)

pop <- popbuff$x
hab <- habbuff$x/pop
edu <- educbuff$x/pop
emp <- employbuff$x/pop
com <- combinedbuff$x/pop

ele <- elecRatebuff$x/pop
ele <- elecNewbuff$V1/pop
ele <- elecOldbuff$V1/pop

head(elecOldbuff)

# Proper data.frame (full network)
n <- length(hab)
grav <- data.frame(popO = pop, eleO = ele, habO = hab, eduO = edu, empO = emp, comO = com)
grav$id <- 1:n
temp <- MM
temp <- melt(temp)
colnames(temp) <- c("O","D","tot")
temp$idG <- 1:(n*n)
temp <- merge(temp,grav,by.x="O",by.y="id",all.x=T)
colnames(grav) <- c("popD","eleD","habD","eduD","empD","comD","id")
temp <- merge(temp,grav,by.x="D",by.y="id",all.x=T)
grav <- temp[order(temp$idG),]
rownames(grav) <- 1:(n*n)
temp <- melt(newDist)
grav$dis <- temp$value

gravNZ <- grav[-which(grav$tot == 0),]
gravNZ <- gravNZ[-which(gravNZ$dis == 0),]
#gravNZ <- gravNZ[-which(gravNZ$eleO == 0),]
#gravNZ <- gravNZ[-which(gravNZ$eleD == 0),]


# Gravitation (full network)

popPop <- glm(tot ~ log(popO)+log(popD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popPop),gravNZ$tot)^2
summary(popPop)

#
popEle <- glm(tot ~ log(popO)+log(eleD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popEle),gravNZ$tot)^2
summary(popEle)

popHab <- glm(tot ~ log(popO)+log(habD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popHab),gravNZ$tot)^2
summary(popHab)

popEdu <- glm(tot ~ log(popO)+log(eduD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popEdu),gravNZ$tot)^2
summary(popEdu)

popEmp <- glm(tot ~ log(popO)+log(empD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popEmp),gravNZ$tot)^2
summary(popEmp)

popCom <- glm(tot ~ log(popO)+log(comD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
cor(fitted(popCom),gravNZ$tot)^2
summary(popCom)

#
popEduEle <- glm(tot ~ log(popO)+log(eduD)+log(eleD)+log(dis), na.action = na.exclude,
                 family = poisson(link = "log"), data = gravNZ)
summary(popEduEle)
cor(fitted(popEduEle),gravNZ$tot)^2

popHabEle <- glm(tot ~ log(popO)+log(habD)+log(eleD)+log(dis), na.action = na.exclude,
                 family = poisson(link = "log"), data = gravNZ)
summary(popHabEle)
cor(fitted(popHabEle),gravNZ$tot)^2


#
habPop <- glm(tot ~ log(habO)+log(popD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
summary(habPop)
cor(fitted(habPop),gravNZ$tot)^2

eduPop <- glm(tot ~ log(eduO)+log(popD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
summary(eduPop)
cor(fitted(eduPop),gravNZ$tot)^2

empPop <- glm(tot ~ log(empO)+log(popD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
summary(empPop)
cor(fitted(empPop),gravNZ$tot)^2

comPop <- glm(tot ~ log(comO)+log(popD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)
summary(comPop)
cor(fitted(comPop),gravNZ$tot)^2

#

popHabPopEle <- glm(tot ~ log(popO)+log(habO)+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                    family = poisson(link = "log"), data = gravNZ)

test <- exp(popHabPopEle$coef[1])*gravNZ$popO^popHabPopEle$coef[2]*gravNZ$habO^popHabPopEle$coef[3]*gravNZ$popD^popHabPopEle$coef[4]*gravNZ$eleD^popHabPopEle$coef[5]*gravNZ$dis^popHabPopEle$coef[6]
summary(popHabPopEle)
cor(test,gravNZ$tot)^2

popEduPopEle <- glm(tot ~ log(popO)+log(eduO)+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                    family = poisson(link = "log"), data = gravNZ)
summary(popEduPopEle)
cor(fitted(popEduPopEle),gravNZ$tot)^2

popComPopEle <- glm(tot ~ log(popO)+log(comO)+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                    family = poisson(link = "log"), data = gravNZ)
summary(popComPopEle)
cor(fitted(popComPopEle),gravNZ$tot)^2

#constrained
gravNZ$OCat <- as.character(gravNZ$O)

constPopEle <- glm(tot ~ OCat+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                   family = poisson(link = "log"), data = gravNZ)
cor(fitted(constPopEle),gravNZ$tot)^2
summary(constPopEle)

constPopEle$coef[818:820]


# All to rural

refAR <- 1:len
refAR <- refAR[isRural]
gravNZAR <- gravNZ[which(gravNZ$D %in% refAR),]

popPopAR <- glm(tot ~ log(popO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
cor(fitted(popPopAR),gravNZAR$tot)^2

#
popEleAR <- glm(tot ~ log(popO)+log(eleD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
cor(fitted(popEleAR),gravNZAR$tot)^2

popHabAR <- glm(tot ~ log(popO)+log(habD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
cor(fitted(popHabAR),gravNZAR$tot)^2

popEduAR <- glm(tot ~ log(popO)+log(eduD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
cor(fitted(popEduAR),gravNZAR$tot)^2

popEmpAR <- glm(tot ~ log(popO)+log(empD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
cor(fitted(popEmpAR),gravNZAR$tot)^2

popComAR <- glm(tot ~ log(popO)+log(comD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
summary(popComAR)
cor(fitted(popComAR),gravNZAR$tot)^2

popEduEleAR <- glm(tot ~ log(popO)+log(eduD)+log(eleD)+log(dis), na.action = na.exclude,
                   family = poisson(link = "log"), data = gravNZAR)
summary(popEduEleAR)
cor(fitted(popEduEleAR),gravNZAR$tot)^2

#
habPopAR <- glm(tot ~ log(habO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
summary(habPopAR)
cor(fitted(habPopAR),gravNZAR$tot)^2

eduPopAR <- glm(tot ~ log(eduO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
summary(eduPopAR)
cor(fitted(eduPopAR),gravNZAR$tot)^2

empPopAR <- glm(tot ~ log(empO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
summary(empPopAR)
cor(fitted(empPopAR),gravNZAR$tot)^2

comPopAR <- glm(tot ~ log(comO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZAR)
summary(comPopAR)
cor(fitted(comPopAR),gravNZAR$tot)^2

#
popEduPopEleAR <- glm(tot ~ log(popO)+log(eduO)+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                      family = poisson(link = "log"), data = gravNZAR)
summary(popEduPopEleAR)
cor(fitted(popEduPopEleAR),gravNZAR$tot)^2

constPopEleAR <- glm(tot ~ OCat+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                     family = poisson(link = "log"), data = gravNZAR)
cor(fitted(constPopEleAR),gravNZAR$tot)^2

popPopAR$coeff
popEleAR$coeff
popHabAR$coeff
popEduAR$coeff
popEmpAR$coeff
habEleAR$coeff
eduEleAR$coeff
empEleAR$coeff

# Rural to rural

gravNZRR <- gravNZAR[which(gravNZAR$O %in% refAR),]

popPopRR <- glm(tot ~ log(popO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
cor(fitted(popPopRR),gravNZRR$tot)^2

#
popEleRR <- glm(tot ~ log(popO)+log(eleD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
cor(fitted(popEleRR),gravNZRR$tot)^2

popHabRR <- glm(tot ~ log(popO)+log(habD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
cor(fitted(popHabRR),gravNZRR$tot)^2

popEduRR <- glm(tot ~ log(popO)+log(eduD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
cor(fitted(popEduRR),gravNZRR$tot)^2

popEmpRR <- glm(tot ~ log(popO)+log(empD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
cor(fitted(popEmpRR),gravNZRR$tot)^2

popComRR <- glm(tot ~ log(popO)+log(comD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
summary(popComRR)
cor(fitted(popComRR),gravNZRR$tot)^2

popEduEleRR <- glm(tot ~ log(popO)+log(eduD)+log(eleD)+log(dis), na.action = na.exclude,
                   family = poisson(link = "log"), data = gravNZRR)
summary(popEduEleRR)
cor(fitted(popEduEleRR),gravNZRR$tot)^2

#
habPopRR <- glm(tot ~ log(habO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
summary(habPopRR)
cor(fitted(habPopRR),gravNZRR$tot)^2

eduPopRR <- glm(tot ~ log(eduO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
summary(eduPopRR)
cor(fitted(eduPopRR),gravNZRR$tot)^2

empPopRR <- glm(tot ~ log(empO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
summary(empPopRR)
cor(fitted(empPopRR),gravNZRR$tot)^2

comPopRR <- glm(tot ~ log(comO)+log(popD)+log(dis), na.action = na.exclude,
                family = poisson(link = "log"), data = gravNZRR)
summary(comPopRR)
cor(fitted(comPopRR),gravNZRR$tot)^2

popEduPopEleRR <- glm(tot ~ log(popO)+log(eduO)+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                      family = poisson(link = "log"), data = gravNZRR)
summary(popEduPopEleRR)
cor(fitted(popEduPopEleRR),gravNZRR$tot)^2

constPopEleRR <- glm(tot ~ OCat+log(popD)+log(eleD)+log(dis), na.action = na.exclude,
                     family = poisson(link = "log"), data = gravNZRR)
cor(fitted(constPopEleRR),gravNZRR$tot)^2

popPopRR$coeff
popEleRR$coeff
popHabRR$coeff
popEduRR$coeff
popEmpRR$coeff
habEleRR$coeff
eduEleRR$coeff
empEleRR$coeff

cor(voronoiC$employ,voronoiC$elecRate)

length(which(isRural))

constPopEleRR$coefficients[524:526]
length(constPopEleRR$coefficients)


eleEle <- glm(tot ~ log(eleO)+log(eleD)+log(dis), na.action = na.exclude,
              family = poisson(link = "log"), data = gravNZ)

test <- exp(eleEle$coef[1])*gravNZ$habO^eleEle$coef[2]*gravNZ$eleD^eleEle$coef[3]*gravNZ$dis^eleEle$coef[4]
cor(test,gravNZ$tot)


#############
### Functions
#############

### Computes desired features of a network
socioFeat <- function(g,dis){
  M <- as_adjacency_matrix(g,attr="weight")
  deg <- degree(g,mode="all",normalized=F)
  nClustFeat <- data.frame(degree = deg)
  gg <- subgraph.edges(graph=g, eids=which(E(g)$weight > 1),delete.vertices=F)
  nClustFeat$between <- betweenness(gg,directed=F,weights=rep(1,length(E(gg))),normalized=F)
  print(paste(length(E(gg))/length(E(g))*100,"% nodes",sep=" "))
  nClustFeat$leverage <- leverage(g,mode="all")
  sumdist <- dis*M
  Dso <- rep(0,nrow(M))
  Dsp <- rep(0,nrow(M))
  for(j in 1:nrow(M)){
    s <- sum(M[j,]+M[,j])-2*M[j,j]
    sd <- sum(sumdist[j,]+sumdist[,j])#-2*sumdist[j,j]
    temp <- M[j,]+M[,j]
    temp[j] <- 0
    temp <- temp[which(temp>0)]
    tempM <- sumdist[j,]+sumdist[,j]
    #tempM[j] <- 0
    tempM <- tempM[which(tempM>0)]
    if(s>0 & length(temp)>1){
      Dso[j] <- -sum(temp/s*log(temp/s))/log(length(temp))
    }
    if(sd>0 & length(tempM)>1){
      Dsp[j] <- -sum(tempM/sd*log(tempM/sd))/log(length(tempM))
    }
  }
  nClustFeat$socDiv <- Dso
  nClustFeat$spaDiv <- Dsp
  d <- rep(0,nrow(M))
  for(j in 1:nrow(M)){
    d[j] <- sum(sumdist[,j]+sumdist[j,])/(sum(M[,j]+M[j,])-2*M[j,j])
  }
  nClustFeat$dist <- d
  rInOut <- rep(NA,nrow(M))
  for(j in 1:nrow(M)){
    if(sum(M[j,]) > 0){
      rInOut[j] <- sum(M[,j])/sum(M[j,])
    }
  }
  nClustFeat$rInOut <- rInOut
  return(nClustFeat)
}

### Sub-function for partial correlations
partCorC <- function(resultbuff){
  resultbuff <- na.omit(resultbuff)
  res <- data.frame(coefF=rep(NA,7),pValF=rep(NA,7),coefP=rep(NA,7),pValP=rep(NA,7))
  test <- lm(elecRate ~ wealth, data = resultbuff)
  test1 <- lm(degree ~ wealth, data = resultbuff)
  test2 <- lm(between ~ wealth, data = resultbuff)
  test3 <- lm(leverage ~ wealth, data = resultbuff)
  test4 <- lm(socDiv ~ wealth, data = resultbuff)
  test5 <- lm(spaDiv ~ wealth, data = resultbuff)
  test6 <- lm(dist ~ wealth, data = resultbuff)
  test7 <- lm(rInOut ~ wealth, data = resultbuff)
  res[1,1] <- cor.test(resultbuff$degree,resultbuff$elecRate,method="spearman")$estimate
  res[1,2] <- cor.test(resultbuff$degree,resultbuff$elecRate,method="spearman")$p.value
  res[2,1] <- cor.test(resultbuff$between,resultbuff$elecRate,method="spearman")$estimate
  res[2,2] <- cor.test(resultbuff$between,resultbuff$elecRate,method="spearman")$p.value
  res[3,1] <- cor.test(resultbuff$leverage,resultbuff$elecRate,method="spearman")$estimate
  res[3,2] <- cor.test(resultbuff$leverage,resultbuff$elecRate,method="spearman")$p.value
  res[4,1] <- cor.test(resultbuff$socDiv,resultbuff$elecRate,method="spearman")$estimate
  res[4,2] <- cor.test(resultbuff$socDiv,resultbuff$elecRate,method="spearman")$p.value
  res[5,1] <- cor.test(resultbuff$spaDiv,resultbuff$elecRate,method="spearman")$estimate
  res[5,2] <- cor.test(resultbuff$spaDiv,resultbuff$elecRate,method="spearman")$p.value
  res[6,1] <- cor.test(resultbuff$dist,resultbuff$elecRate,method="spearman")$estimate
  res[6,2] <- cor.test(resultbuff$dist,resultbuff$elecRate,method="spearman")$p.value
  res[7,1] <- cor.test(resultbuff$rInOut,resultbuff$elecRate,method="spearman")$estimate
  res[7,2] <- cor.test(resultbuff$rInOut,resultbuff$elecRate,method="spearman")$p.value
  res[1,3] <- cor.test(test1$residuals,test$residuals,method="spearman")$estimate
  res[1,4] <- cor.test(test1$residuals,test$residuals,method="spearman")$p.value
  res[2,3] <- cor.test(test2$residuals,test$residuals,method="spearman")$estimate
  res[2,4] <- cor.test(test2$residuals,test$residuals,method="spearman")$p.value
  res[3,3] <- cor.test(test3$residuals,test$residuals,method="spearman")$estimate
  res[3,4] <- cor.test(test3$residuals,test$residuals,method="spearman")$p.value
  res[4,3] <- cor.test(test4$residuals,test$residuals,method="spearman")$estimate
  res[4,4] <- cor.test(test4$residuals,test$residuals,method="spearman")$p.value
  res[5,3] <- cor.test(test5$residuals,test$residuals,method="spearman")$estimate
  res[5,4] <- cor.test(test5$residuals,test$residuals,method="spearman")$p.value
  res[6,3] <- cor.test(test6$residuals,test$residuals,method="spearman")$estimate
  res[6,4] <- cor.test(test6$residuals,test$residuals,method="spearman")$p.value
  res[7,3] <- cor.test(test7$residuals,test$residuals,method="spearman")$estimate
  res[7,4] <- cor.test(test7$residuals,test$residuals,method="spearman")$p.value
  return(res)
}

### Several plots in a matrix
multiplot <- function(...,plotlist=NULL,file,cols=1,layout=NULL) {
  library(grid)
  plots <- c(list(...),plotlist)
  numPlots = length(plots)
  if (is.null(layout)){
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
