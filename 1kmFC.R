





library(dplyr)
library(ggplot2)
library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

OneKMFC.frm  <- read.csv("taxa1km.csv")
attach(OneKMFC.frm)
OneKMFC.frm
head(OneKMFC.frm)

env <- OneKMFC.frm[,1:3]
str(env)

taxa <- select(OneKMFC.frm, Acari:Polyplectopus)
set.seed(1)
taxa.mds <- metaMDS(taxa, distance = "bray", k = 2,trymax=100)  #using all the defaults
taxa.mds


# Plot --------------------------------------------------------------------


par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))
plot(taxa.mds,type="n")

ordihull(taxa.mds,group=env$FC1Km,show.groups="First",lty=5,lwd=2, col="grey40")
ordihull(taxa.mds,group=env$FC1Km,show.groups="Second",lty=5,lwd=2, col="red3")
ordihull(taxa.mds,group=env$FC1Km,show.groups="Third",lty=5,lwd=2, col="blue")
ordihull(taxa.mds,group=env$FC1Km,show.groups="Fourth",lty=5,lwd=2, col="green")


points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$FC1Km=="First"),
       pch = 21,col="black", bg="gray45")

points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$FC1Km=="Second"),
       pch = 22,col="black", bg="red")

points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$FC1Km=="Third"),
       pch = 22,col="black", bg="blue")

points(taxa.mds, display = "sites", cex=1.5, 
       select=which(env$FC1Km=="Fourth"),
       pch = 22,col="black", bg="green")

X1 <- grconvertX(0.9,"npc") # controla la ubicaci?n en el eje "X"
Y1 <- grconvertY(1,"npc") # controla la ubicaci?n en el eje "y"

legend(X1, Y1, pch=c(21,22,23,24), col="black",
       pt.bg = c("gray45","red","blue","green"), 
       legend = c("First", "Second","Third","Fourth"),
       box.col="white",bty="n",cex=1)



x.leg2<-grconvertX(0.2,"npc")
y.leg2<-grconvertY(0.95,"npc")

text(x.leg2, y.leg2, "Stress = 0.25", cex=1, font=1)




#install.packages("labdsv")
#install.packages("indicspecies")
install.packages("labdsv")
library("indicspecies")
library("labdsv")

library(vegan)
library(MASS)
library(labdsv)
library(cluster)
library(indicspecies)

ind_species<-multipatt(taxa,OneKMFC.frm$FC1Km,max.order=2,
                       duleg=TRUE,func="IndVal.g",control=how(nperm=5000))
ind_species
summary(ind_species)




(sim <- with(OneKMFC.frm, simper(taxa, FC1Km)))
summary(sim)
