library(igraph)
library(igraphdata)
library(visNetwork)
library(dplyr)
library(ggplot)

#Creating Edges and nodes 1971-1993... directed graph    
#(working with one year for similicity)
release71_93_peryear<-split(release1971_1993,release1971_1993$year)


library(tidyverse)
edge_list <- tibble(from = release71_93_peryear$`1971`$country_o, to = release71_93_peryear$`1971`$country_d)
node_list <- tibble(id = (release71_93_peryear$`1971`$country_o))

g<-graph.data.frame(edge_list,directed = T)
g
E(g)  #40000 edges for 1971, needs to be weigthed by edge attribute and filtered
V(g)

plot(g)
#simplify the graph

plot(simplify(g))
#only map where canada, mexico and  United States 
E(g)[from("Canada")]
E(g)[from("Mexico")]
E(g)[from("United States")]
##all have 200 edges


#add node attribute and edge attribute ####





