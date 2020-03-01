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
#USA-193
#CAN-34
#MEX-120
BCCC<- BCCC[c(193,34,120),]
BCCC$Countries<-rownames(BCCC)

#Calculating Vertex and edge characteristics. 
#Degree dv of a vertex v, in a network graph G = (V,E),counts the number of edges in E incident upon v.  
#The collection { fd }d≥0 is called the degree distribution of G, 
#and is simply a rescaling of the set of degree frequencies, formed from the original degree sequence

library(sand)
edge_list <- tibble(from = BACI1995$Importer, to = BACI1995$Exporter)
node_list <- tibble(id = (BACI1995$Exporter))
g<-graph.data.frame(edge_list, directed = T)
g
E(g)
E(g)$weight<-BACI1995$VoT
V(g)
ecount(g)
vcount(g)

hist(degree(g))
hist(degree(g),col="lightblue",
     xlab="Vertex Degree",ylab="Frequency",main="")   

#Calculating Vertex Strength.

hist(graph.strength(g), col="pink",
     xlab="Vertex Strength", ylab="Frequency", main="")

#Calculating Vertex degree distribution of International trade in 1995. 
d.g <- degree(g)
hist(d.g,col="blue",
     xlab="Degree", ylab="Frequency", 
     main="Degree Distribution")

#Calculating log. Vertex degree distribution of International trade in 1995.
dd.g <- degree.distribution(g)
d <- 1:max(d.g)-1
ind <- (dd.g != 0)
plot(d[ind], dd.g[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"), main="Log-Log Degree Distribution")

#Calculating log. Vertex degree distribution of International trade in 1995.
a.nn.deg.g <- graph.knn(g,V(g))$knn 
plot(d.g, a.nn.deg.g, log="xy",col="goldenrod", 
     xlab=c("Log Vertex Degree"), 
     ylab=c("Log Average Neighbor Degree"))

#Density and notions of Relative frequencies. 
ego.instr <- induced.subgraph(g, neighborhood(g, 1, 1)[[1]])
ego.admin <- induced.subgraph(g, neighborhood(g, 1, 34)[[1]])
graph.density(g)
graph.density(ego.instr) 
graph.density(ego.admin)

reciprocity(g, mode="default")
reciprocity(g, mode="ratio")

assortativity.degree(g)

transitivity(g)
transitivity(g, "local", vids=c(1,34))

#Connectivity, cuts and flows.
is.connected(g)
comps <- decompose.graph(g)
table(sapply(comps, vcount))
g.gc <- decompose.graph(g)
vertex.connectivity(g.gc)

