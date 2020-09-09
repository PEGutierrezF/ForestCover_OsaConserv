



#--------------------------------------------
# nMDS 
# 09 Sep 2020
#PEGF
#--------------------------------------------
#

library(ggplot2)
library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

matrix.frm  <- read.csv("taxa.csv")
attach(matrix.frm)
matrix.frm
head(matrix.frm)

taxa <- select(matrix.frm, Acari:Polyplectopus)
set.seed(1)
taxa.mds <- metaMDS(taxa, distance = "bray", k = 2,trymax=100)  #using all the defaults
taxa.mds


Locality <- select(matrix.frm, Locality)
Site <- select(matrix.frm, ï..Sites)
Locality
data.scores <- as.data.frame(scores(taxa.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores

data.scores$ï..Sites <- unlist(ï..Sites)  #   create a column of site names
data.scores$Locality <- unlist(Locality)  
head(data.scores) #look at the data



species.scores <- as.data.frame(scores(taxa.mds, "species"))
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

data.scores$Locality <- factor(data.scores$Locality,
                             levels = c("UpStream","DownStream"), ordered = TRUE)




# Plot --------------------------------------------------------------------

p <- ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2, label = "*"),size=7, alpha=0.5) # add the species labels
p

p1 <- p + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,
                                          colour=Locality),size=5) + # add the point markers
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) + 
  scale_shape_manual(values=c(15, 17))
p1 
