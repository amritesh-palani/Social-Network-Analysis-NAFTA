library(igraph)
library(igraphdata)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(magrittr)


#1---1995 to 1999####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
BACI_Grav_95_99 <- read.csv("../Output/Combined/BACI_Grav_95_99.csv")
view(BACI_Grav_95_99)
BACI_Grav_95_99$X<- NULL

#Create dataframe for edgelist and node list.
edges_p1<-tibble(from= BACI_Grav_95_99$Exporter, to= BACI_Grav_95_99$Importer)
nodes_p1<-tibble(id = (unique(BACI_Grav_95_99["Exporter"])))



net_95_99<- graph.data.frame(edges_p1,directed=F)
E(net_95_99) #26705 Edges, large igraph
V(net_95_99) #212 Nodes

net_95_99

#Setting Edge attributes ####
E(net_95_99)$VoT  <- c(BACI_Grav_95_99$VoT)
E(net_95_99)$Quantity  <- c(BACI_Grav_95_99$Quantity)
E(net_95_99)$distance  <- c(BACI_Grav_95_99$distance)
E(net_95_99)$contiguity  <- c(BACI_Grav_95_99$contiguity)
E(net_95_99)$agree_pta_goods  <- c(BACI_Grav_95_99$agree_pta_goods)
E(net_95_99)$agree_pta_services  <- c(BACI_Grav_95_99$agree_pta_services)
E(net_95_99)$agree_cu  <- c(BACI_Grav_95_99$agree_cu)
E(net_95_99)$agree_eia  <- c(BACI_Grav_95_99$agree_eia)
E(net_95_99)$agree_fta  <- c(BACI_Grav_95_99$agree_fta)
E(net_95_99)$agree_psa  <- c(BACI_Grav_95_99$agree_psa)
E(net_95_99)$agree_pta  <- c(BACI_Grav_95_99$agree_pta)
E(net_95_99)$VoT_Proportion  <- c(BACI_Grav_95_99$VoT_Proportion)
summary(net_95_99)


#Setting Node attributes ####
#To assign an attribue to each node we need to restructure our dataframe to have the list of nodes
#and the respective attributes in each row.

head(BACI_Grav_95_99)
Exporter_attributes<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Exporter)%>%
    summarise(landlocked_o=max(landlocked_o,na.rm=TRUE),
              island_o=max(island_o,na.rm=TRUE), 
              gdp_wdi_const_o=mean(gdp_wdi_const_d,na.rm=TRUE),
              gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
              pop_o=mean(pop_o,na.rm=TRUE),
              polity_o=max(polity_o,na.rm=TRUE))
Exporter_attributes$country<-Exporter_attributes$`BACI_Grav_95_99$Exporter`
Exporter_attributes$`BACI_Grav_95_99$Exporter`<-NULL
Exporter_attributes<-Exporter_attributes[,c(7, 1:6)]#reorder

Importer_attributes<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Importer)%>%
  summarise(landlocked_d=max(landlocked_d,na.rm=TRUE),
            island_d=max(island_d,na.rm=TRUE), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE),
            polity_d=max(polity_d,na.rm=TRUE))
Importer_attributes$country<-Importer_attributes$`BACI_Grav_95_99$Importer`
Importer_attributes$`BACI_Grav_95_99$Importer`<-NULL
Importer_attributes<-Importer_attributes[,c(7, 1:6)]#reorder

#the node attribute Dataframe####
node_attribute<-merge(x=Exporter_attributes, y=Importer_attributes, by= 'country')

node_attribute$landlocked<- apply(node_attribute[,names(node_attribute) %in% c("landlocked_o", "landlocked_d")], 1, max, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("landlocked_o", "landlocked_d")]<- NULL 

node_attribute$island<- apply(node_attribute[,names(node_attribute) %in% c("island_o", "island_d")], 1, max, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("island_o", "island_d")]<- NULL  


node_attribute$gdp_wdi_const<- apply(node_attribute[,names(node_attribute) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")], 1, mean, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("gdp_wdi_const_o", "gdp_wdi_const_d")]<- NULL  


node_attribute$gdp_wdi_cap_const<- apply(node_attribute[,names(node_attribute) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")], 1, mean, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("gdp_wdi_cap_const_o", "gdp_wdi_cap_const_d")]<- NULL  

node_attribute$pop<- apply(node_attribute[,names(node_attribute) %in% c("pop_o", "pop_d")], 1, mean, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("pop_o", "pop_d")]<- NULL  

node_attribute$polity<- apply(node_attribute[,names(node_attribute) %in% c("polity_o", "polity_d")], 1, mean, na.rm=TRUE)
node_attribute[,names(node_attribute) %in% c("polity_o", "polity_d")]<- NULL 

head(node_attribute)

#Add continent as node attribute 
continent<-read.csv("../Input/country-and-continent-codes-list.csv")

node_attribute<- merge(x=node_attribute, y=continent, by.x = 'country', by.y = 'Three_Letter_Country_Code', all.x = FALSE, all.y = FALSE)
row(which(node_attribute$landlocked=NULL))
rm(node_attribute)

V(net_95_99)$landlocked  <- c(node_attribute$landlocked)
V(net_95_99)$island  <- c(node_attribute$island)
V(net_95_99)$gdp_wdi_const  <- c(node_attribute$gdp_wdi_const)
V(net_95_99)$gdp_wdi_cap_const  <- c(node_attribute$gdp_wdi_cap_const)
V(net_95_99)$pop  <- c(node_attribute$pop)
V(net_95_99)$polity  <- c(node_attribute$polity)
V(net_95_99)$continent  <- c(node_attribute$region)
V(net_95_99)$sub_region  <- c(node_attribute$sub_region)



#Network From Canada, Mexico and United States####

#Ego network
#Map####

#Network Metrics