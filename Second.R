
library(ggplot2)
library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

matrix.frm  <- read.csv("taxa.csv")
attach(matrix.frm)
matrix.frm
head(matrix.frm)

env <- matrix.frm[,1:2]
str(env)

taxa <- select(matrix.frm, Acari:Polyplectopus)
set.seed(1)
taxa.mds <- metaMDS(taxa, distance = "bray", k = 2,trymax=100)  #using all the defaults
taxa.mds


# Plot --------------------------------------------------------------------


par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))
plot(taxa.mds,type="n")

ordihull(taxa.mds,group=env$Locality,show.groups="UpStream",lty=5,lwd=2, col="grey40")
ordihull(taxa.mds,group=env$Locality,show.groups="DownStream",lty=5,lwd=2, col="red3")

points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$Locality=="UpStream"),
       pch = 21,col="black", bg="gray45")

points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$Locality=="DownStream"),
       pch = 22,col="black", bg="red")

X1 <- grconvertX(0.7,"npc") # controla la ubicaci?n en el eje "X"
Y1 <- grconvertY(1,"npc") # controla la ubicaci?n en el eje "y"

legend(X1, Y1, pch=c(21,22), col="black",
       pt.bg = c("gray45","red"), 
       legend = c("UpStream", "DownStream"),
       box.col="white",bty="n",cex=1)


x.leg2<-grconvertX(0.2,"npc")
y.leg2<-grconvertY(0.95,"npc")

text(x.leg2, y.leg2, "Stress = 0.25", cex=1, font=1)

