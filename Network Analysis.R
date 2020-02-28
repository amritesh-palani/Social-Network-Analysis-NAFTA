library(igraph)
library(igraphdata)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(tidyverse)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

BACI_Grav_95_99<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_Grav_95_99.csv")
library(tidyverse)
edge_list <- tibble(from = BACI_Grav_95_99$Exporter, to = BACI_Grav_95_99$Importer)
node_list <- tibble(id = (BACI_Grav_95_99$Exporter))

BACI_Grav_9599_Network<-graph.data.frame(edge_list,directed = F)
BACI_Grav_9599_Network
E(BACI_Grav_9599_Network)  
V(BACI_Grav_9599_Network)

plot(BACI_Grav_9599_Network)
class(BACI_Grav_9599_Network)

