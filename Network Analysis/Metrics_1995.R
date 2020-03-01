library(plyr)
packages <- c("igraph","visNetwork","dplyr","ggplot")
install.packages(packages)

library(igraph)
require(igraph)
library(tidyverse)
library(haven)
install.packages("tibble")
library("tibble")

install.packages("network")
library(network)
library(sna)

install.packages("sand") 
install_sand_packages()
library(sand)

install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library(Rgraphviz)
install.packages("visNetwork")
library(visNetwork)

#Reading Data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

BACI1995<-read.csv("../Output/Combined/BACI1995_agg.csv",sep = ",")
colnames(BACI1995)
head(BACI1995)
summary(BACI1995)

#Calculating Metrics for USA, CAN and MEX. 
##Centrality measures.

BACI1995<-read.csv("../Output/Combined/BACI1995_agg.csv",sep = ",")
edge_list <- tibble(from = BACI1995$Importer, to = BACI1995$Exporter)
node_list <- tibble(id = unique(BACI1995$Importer))
g<-graph.data.frame(edge_list, directed = T)
E(g)$weight<-BACI1995$VoT
V(g)

##Degree Centrality
DC<- centr_degree(g, mode = "all")
DC$res
BACI1995<-merge(BACI1995,DC)
a<- filter(BACI1995, BACI1995$Importer=="USA")
b<- filter(BACI1995, BACI1995$Importer=="CAN")
c<- filter(BACI1995, BACI1995$Importer=="MEX")
head(BACI1995)
##Eigen Vector centrality. 
EVC<-as.data.frame(eigen_centrality(g))
EVC<-EVC$vector
BACI1995<-merge(BACI1995,EVC)
head(BACI1995)
BACI1995<- filter(BACI1995, BACI1995$Importer=="CAN")
head(BACI1995)
#BACI1995$xxx <- NULL
##Betweenness Centrality. 
###Betweenness centrality measures are aimed at summarizing the extent to which a vertex is located ‘between’ other pairs of vertices.
g1<- get.adjacency(g,sparse=FALSE)
BC<- betweenness(g1,gmode="digraph", diag=FALSE, 
               tmaxdev=FALSE, cmode="directed", geodist.precomp=NULL, 
               rescale=FALSE, ignore.eval=TRUE)
BC<- as.data.frame(BC)
head(BC)
#Closeness centrality
#Closeness centrality measures attempt to capture the notion that a vertex is ‘cen- tral’ if it 
#is ‘close’ to many other vertices. 
CC<- closeness(g1,g=100, gmode="digraph", diag=FALSE, 
               tmaxdev=FALSE, cmode="directed", geodist.precomp=NULL, 
               rescale=FALSE, ignore.eval=TRUE)
CC
CC<- as.data.frame(CC)
head(CC)
BCCC<- cbind(BC,CC)

for (i in seq_along(V(g))) {
  if (V(g)[i] %in% c(V(g)["CAN"], V(g)["USA"], V(g)["MEX"])) {
    print(i)
  }
}
#USA-193
#CAN-34
#MEX-120
BCCC<- BCCC[c(193,34,120),]
BCCC$Countries<-rownames(BCCC)

#Density and notions of Relative frequencies. 
graph.density(g)

reciprocity(g, mode="default")
reciprocity(g, mode="ratio")

assortativity.degree(g)

transitivity(g)
transitivity(g, "local", vids=c(1,34))

