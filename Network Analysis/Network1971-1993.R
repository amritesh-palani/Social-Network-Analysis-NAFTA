library(igraph)
library(igraphdata)
library(visNetwork)
library(dplyr)
library(ggplot)
library(tidyverse)

#Creating Edges and nodes 1971-1993... directed graph    
#(working with one year for similicity)
release71_93_peryear<-split(release1971_1993,release1971_1993$year)
lapply(release71_93_peryear, dim)

library(tidyverse)
edge_list <- tibble(from = release71_93_peryear$`1971`$country_o, to = release71_93_peryear$`1971`$country_d)
node_list <- tibble(id = (release71_93_peryear$`1971`$country_o))
#change year to see below figures

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
##all have the same number of edges
plot(make_ego_graph(g,3,"Canada")[[1]])
CA_US_MX<-split(CA_US_MX_1971_2016,CA_US_MX_1971_2016$year)
edge_list <- tibble(from = CA_US_MX$`1971`$country_o, to = CA_US_MX$`1971`$country_d)
node_list <- tibble(id = (CA_US_MX$`1971`$country_o))
g<-graph.data.frame(edge_list,directed = T)
plot(g)
make_ego_graph(g,3,"Canada")[[1]]

#add node attribute and edge attribute ####
release1971=release1971_1993[release1971_2016$year==1971, ]

dim(release1972)

edge_list <- tibble(from = release71_93_peryear$`1971`$country_o, to = release71_93_peryear$`1971`$country_d)
node_list <- tibble(id = (release71_93_peryear$`1971`$country_o))


####Countries that have trade agreements
str(release1971_2016) #starting from full dataset
which( colnames(release1971_2016)=="agree_pta_goods")
which( colnames(release1971_2016)=="agree_fta")


release1971_2016$total_trade_agreement <-(release1971_2016$agree_pta_goods+
                                                release1971_2016$agree_pta_services+release1971_2016$agree_cu+
                                                release1971_2016$agree_eia+release1971_2016$agree_fta
                                              +release1971_2016$agree_psa)
release1971_2016$total_trade_agreement
#the above sum of the total trade agreement they have with eachother
#using this to create edges

release1971_2016$any_trade_agreement<-release1971_2016$total_trade_agreement>0
#the above T/F if countries have any trade agreement
#using this to weight edges
release1971_2016$any_trade_agreement

#subsetting on countries that have any agreements with eachother
release1971_2016_agree<-release1971_2016[release1971_2016$any_trade_agreement==TRUE,]
release1971_2016_agree$any_trade_agreement
release_peryear<-split(release1971_2016_agree,release1971_2016_agree$year)

#MAPPING undirected graph for those with agreements
edge_agr <- tibble(from = release_peryear$`1971`$country_o, to = release_peryear$`1971`$country_d)
node_agr <- tibble(id = (release_peryear$`1971`$country_o))
agr_g<-graph.data.frame(edge_list,directed = F)
agr_g
E(agr_g)  #40000 edges for 1971, needs to be weigthed by edge attribute and filtered
V(agr_g)
plot(agr_g)
plot(simplify(agr_g))





