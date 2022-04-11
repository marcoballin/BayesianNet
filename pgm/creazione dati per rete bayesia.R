library(raster)
library(geoR)
setwd("D:/GitHub/BayesianNet")
# decido quanto ? grande il raster
rast_righe=10
rast_col=10
# simulo il processo stocastico con dei dati parametri
sim2 <- grf(rast_col*rast_righe, grid = "reg", cov.pars = c(1, .25))
# genero un raster vuoto e poi inserisco i valori
r_obs = raster(nrows = rast_righe, ncols = rast_col, xmn = 0, xmx = rast_col, ymn = 0, ymx = rast_righe)
values(r_obs)=sim2$data
plot(r_obs)
# prendo le coordinate centrraili dei pixel  e calcolo la matrice di distanze
coord=coordinates(r_obs)
dist<-as.matrix(dist(coord,"euclidean",diag=F))

# seleziono nella matrice i pixel che distano meno i 1,43 (pixel adiacenti nel seno della regina)
d=(dist=1 & dist!=0)

# costruisco una matrice di dati che ha per ciascun pixel il suo valore e quelli dei pixel vicini
X=NULL
for(j in 3:8){
for(i in 3:8){
  X=rbind(X,t(c(r_obs[i,j],r_obs[i,j-1],r_obs[i,j+1],r_obs[i-1,j],r_obs[i+1,j])))
}
}
X=as.data.frame(X)
X$d=1
Z=X
d=(dist=2 & dist!=0)

X=NULL
# costruisco una matrice di dati che ha per ciascun pixel il suo valore e quelli dei pixel vicini

for(j in 3:8){
  for(i in 3:8){
    X=rbind(X,t(c(r_obs[i,j],r_obs[i,j-2],r_obs[i,j+2],r_obs[i-2,j],r_obs[i+2,j])))
  }
}
X=as.data.frame(X)
X$d=2

Z=rbind(Z,X)
setwd("D:/Lavoro/MEA")
write.csv2(Z, file="prima_prova.csv")
# X contiene per ciascun pixel il valore di X e quello dei vicini seondo la mossa della torre
# vengono persi i bordi
# ? possibile che si debba lavorare con valori discreti

