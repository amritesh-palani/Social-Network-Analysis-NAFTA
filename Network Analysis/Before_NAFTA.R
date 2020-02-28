library(plyr)

install.packages("sand") 
library(sand)
install_sand_packages()

#Reading Data

packages <- c("igraph","visNetwork","dplyr","ggplot")
install.packages(packages)
install.packages("igraph")

library(igraph)
library(tidyverse)
library(haven)

require(igraph)

library("igraphdata")

#Hypothesis 1: How are economic indicators (GDP, trade def,unemployement, etc) 
#related to network metrics (closeness, centrality, strength) ?

getwd()
setwd("~/Documents/MBD/Term 2/Social Network Analysis/Group thesis/Datasets/BACI_HS92")
BACI1995<-read.csv("BACI1995_agg.csv",sep = ",")
colnames(BACI1995)
head(BACI1995)
summary(BACI1995)

#list.vertex.attributes(BACI1995)
#plot(BACI1995)

library(tidyverse)
edge_list <- tibble(from = BACI1995$Exporter, to = BACI1995$Importer, BACI1995$VoT> 10^15)
node_list <- tibble(id = (BACI1995$Importer))

g<-graph.data.frame(edge_list,directed = T)
g
E(g)  #40000 edges needs to be weigthed by edge attribute and filtered
V(g)
plot(g)
#Simplified
plot(simplify(g))

#only map where canada, mexico and  United States 
E(g)[from("CAN")]
E(g)[from("USA")]
E(g)[from("MEX")]

##Trade from Canada to US in 1995 
plot(make_ego_graph(g,3,"USA")[[1]])

CA_US<-split(BACI1995,BACI1995$Exporter["CAN"])
edge_list <- tibble(from = BACI1995$Exporter, to = BACI1995$Importer)
node_list <- tibble(id = (CA_US$CAN))
g<-graph.data.frame(edge_list,directed = T)
plot(g)
plot(make_ego_graph(g,3,"CAN")[[1]])


dim(release1972)

edge_list <- tibble(from = BACI1995$`1971`$Exporter, to = BACI1995r$Importer)
node_list <- tibble(id = (BACI1995$Importer))

####Countries that have trade agreements
str(release1971_2016) #starting from full dataset
which( colnames(BACI1995)=="agree_pta_goods")
which( colnames(BACI1995)=="agree_fta")


BACI1995$total_trade_agreement <-(BACI1995$agree_pta_goods+
                                            BACI1995$agree_pta_services+BACI1995$agree_cu+
                                            BACI1995$agree_eia+BACI1995$agree_fta
                                          +BACI1995$agree_psa)
BACI1995$total_trade_agreement
#the above sum of the total trade agreement they have with eachother
#using this to create edges

BACI1995$any_trade_agreement<-BACI1995$total_trade_agreement>0
#the above T/F if countries have any trade agreement
#using this to weight edges
BACI1995$any_trade_agreement

#subsetting on countries that have any agreements with eachother
BACI1995<-release1971_2016[BACI1995$any_trade_agreement==TRUE,]
BACI1995$any_trade_agreement

#MAPPING undirected graph for those with agreements
edge_list <- tibble(from = BACI1995$Exporter, to = BACI1995$Importer)
node_list <- tibble(id = (BACI1995$Importer))
agr_g<-graph.data.frame(edge_list,directed = F)
agr_g
E(agr_g)  #40000 edges for 1971, needs to be weigthed by edge attribute and filtered
V(agr_g)
plot(agr_g)
plot(simplify(agr_g))


