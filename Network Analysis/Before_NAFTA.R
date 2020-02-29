library(plyr)
install.packages("sand") 
library(sand)
install_sand_packages()

packages <- c("igraph","visNetwork","dplyr","ggplot")
install.packages(packages)
install.packages("igraph")

library(igraph)
library(tidyverse)
library(haven)
require(igraph)

#Reading Data

getwd()
setwd("~/Documents/MBD/Term 2/Social Network Analysis/Group thesis/Datasets/BACI_HS92")
BACI1995<-read.csv("BACI1995_agg.csv",sep = ",")
colnames(BACI1995)
head(BACI1995)
summary(BACI1995)

#Visulaizaing the Global trade network. 
edge_list <- tibble(from = BACI1995$Importer, to = BACI1995$Exporter)
node_list <- tibble(id = (BACI1995$Exporter))
g<-graph.data.frame(edge_list, directed = T)
g
E(g)
V(g)
plot(g)
#Edges needs to be weigthed by edge attribute (Vo) and filtered
#Plotting weighted graph based on value of trade
E(g)$weight<-BACI1995$VoT
V(g)
plot(g)
#Simplified
plot(simplify(g))

#only map canada, mexico and  United States based on VoT 
E(g)[from("USA")]
E(g)[from("CAN")]
E(g)[from("MEX")]

##Isolating Trade of CANADA in 1995 based on VoT 
edge_list <- tibble(from =BACI1995$Exporter, to =BACI1995$Importer)
node_list <- tibble(id = (BACI1995$Exporter))
g<-graph.data.frame(edge_list,directed = T)
E(g)[from("CAN")]
E(g)$weight<-BACI1995$VoT
plot(g)
plot(make_ego_graph(g,1,"CAN")[[1]])

##Isolate Trade of USA in 1995 based on VoT 
edge_list <- tibble(from =BACI1995$Exporter, to =BACI1995$Importer)
node_list <- tibble(id = (BACI1995$Exporter))
g<-graph.data.frame(edge_list,directed = T)
E(g)[from("USA")]
E(g)$weight<-BACI1995$VoT
g
plot(g)
plot(make_ego_graph(g,3,"USA")[[1]])

##Isolate Trade of USA in 1995
edge_list <- tibble(from =BACI1995$Exporter, to =BACI1995$Importer)
node_list <- tibble(id = (BACI1995$Exporter))
g<-graph.data.frame(edge_list,directed = T)
E(g)[from("MEX")]
E(g)$weight<-BACI1995$VoT
g
plot(g)
plot(make_ego_graph(g,3,"MEX")[[1]])

####Visulaize Countries that have trade agreements
str(BACI1995) #starting from full dataset
which( colnames(BACI1995)=="agree_pta_goods")
which( colnames(BACI1995)=="agree_fta")

BACI1995$total_trade_agreement <-(BACI1995$agree_pta_goods+
                                            BACI1995$agree_pta_services+BACI1995$agree_cu+
                                            BACI1995$agree_eia+BACI1995$agree_fta
                                          +BACI1995$agree_psa)
BACI1995$total_trade_agreement
head(BACI1995)
#the above sum of the total trade agreement they have with eachother
#using this to create edges

BACI1995$any_trade_agreement<-BACI1995$total_trade_agreement>0
#the above T/F if countries have any trade agreement
#using this to weight edges
BACI1995$any_trade_agreement
head(BACI1995)

#subsetting on countries that have any agreements with eachother
BACI1995_TA<-BACI1995[BACI1995$any_trade_agreement==TRUE,]
BACI1995_TA$any_trade_agreement

#MAPPING directed/ undirected graph for those with agreements
edge_list <- tibble(from = BACI1995_TA$Exporter, to = BACI1995_TA$Importer)
node_list <- tibble(id = (BACI1995_TA$Importer))
agr_g<-graph.data.frame(edge_list,directed = T)
agr_g
E(agr_g)  #Needs to be weigthed by edge attribute and filtered
V(agr_g)
plot(agr_g)
plot(simplify(agr_g))ol

#Hypothesis 1: How are economic indicators (GDP, trade def,unemployement, etc) 
#related to network metrics (closeness, centrality, strength) ?

#Calculating Vertex degree of International trade 1995. 
#Degree dv of a vertex v, in a network graph G = (V,E),counts the number of edges in E incident upon v.  
#The collection { fd }d≥0 is called the degree distribution of G, 
#and is simply a rescaling of the set of degree frequencies, formed from the original degree sequence
install.packages("network")
library(network)
library(sna)

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

#Calculating Vertex strength of International trade.

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

#Centrality measures.

A <- get.adjacency(g, sparse=FALSE)

g <- network::as.network.matrix(A)
sna::gplot.target(g, degree(g), main="Degree", 
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col=c("blue", rep("red", 32), "yellow"), 
                  edge.col="darkgray")

l <- layout.kamada.kawai(g)
plot(g, layout=l, main="Importer", vertex.label="",
     vertex.size=10 * sqrt(hub.score(BACI1995$VoT)))

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

average.path.length(g.gc)
diameter(g.gc)

vertex.connectivity(g.gc)


#Visualizing the global tarde network. 

setwd("~/Documents/MBD/Term 2/Social Network Analysis/Group thesis/Datasets/BACI_HS92")
BACI1995<-read.csv("BACI1995_agg.csv",sep = ",")
colnames(BACI1995)
head(BACI1995)

#Visulaizaing the Global trade network. 


#check<-read.csv("routes_edges.csv")
#nodesvalue<-read.csv("airport_nodes.csv")
#edges <- data.frame(from = check$ï..source, to = check$destination,value=check$distance)
#nodes<- data.frame(id= nodesvalue$airport, value=nodesvalue$value)

install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library(Rgraphviz)
install.packages("visNetwork")
library(visNetwork)


getwd()
setwd("~/Documents/MBD/Term 2/Social Network Analysis/Group thesis/Datasets/BACI_HS92")
BACI1995<-read.csv("BACI1995_agg.csv",sep = ",")
edges<- data.frame(from =  BACI1995$Importer, to = BACI1995$Exporter, value=BACI1995$gdp_wdi_const_o)
nodes<- data.frame(id = unique(BACI1995$Importer))

graph_plot<-visNetwork(nodes, edges, background = "white" ,) %>%
        visOptions(highlightNearest = TRUE, collapse = TRUE,autoResize = TRUE ,) %>%
        visEdges(arrows = "to", dashes = TRUE,smooth = FALSE ,shadow=TRUE, 
                 color = c(color=rgb(0,0,0, 0.3),highlight="orange",hover=rgb(1,0.60,0, 0.5))) %>%
        visNodes( shape="dot",font = list(color="black",background= "white", size= 30) ,shadow = TRUE ,
                  color= c(background=rgb(0.0,0.8,0.6, 0.9) ,border=rgb(0,0,0, 1),highlight="white" ),
                  mass= 2 ) %>%
        visEvents(type = "once",startStabilizing = "function() {
                  this.moveTo({scale:1})}") %>% visIgraphLayout() %>%
        visPhysics(stabilization = TRUE)

graph_plot

visNetwork(nodes, edges, background = "white" ,)

nodes_temp <- BACI1995[,c('Importer', 'VoT')]

head(nodes_temp)

class(nodes_temp$VoT)

value <- aggregate(nodes_temp, by = list(nodes_temp$Importer), sum)
