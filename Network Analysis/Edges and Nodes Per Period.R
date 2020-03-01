library(tidyverse)
library(data.table)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#1---1995 to 1999####
BACI_Grav_95_99 <- read.csv("../Output/Combined/BACI_Grav_95_99.csv")
str(BACI_Grav_95_99)
BACI_Grav_95_99$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_95_99<-BACI_Grav_95_99[(BACI_Grav_95_99$Exporter!='N/A' & BACI_Grav_95_99$Importer!='N/A'),]


edges_p1<-BACI_Grav_95_99[,which(names(BACI_Grav_95_99)%in%c('Exporter','Importer'))]
edges_p1$Source<-edges_p1$Exporter
edges_p1$Exporter<-NULL
edges_p1$Target<-edges_p1$Importer
edges_p1$Importer<-NULL

nodes_p1<-as.data.frame(unique(BACI_Grav_95_99["Exporter"]))
write.csv(edges_p1, file = "../Output/Network Data/edges_p1.csv", row.names = FALSE) 
write.csv(nodes_p1, file = "../Output/Network Data/nodes_p1.csv", row.names = FALSE)


#Adding Export Attributes
Exporter_attributes_p1<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))

Exporter_attributes_p1$country<-Exporter_attributes_p1$'BACI_Grav_95_99$Exporter'
Exporter_attributes_p1$'BACI_Grav_95_99$Exporter'<-NULL
Exporter_attributes_p1<-Exporter_attributes_p1[,c(6, 1:5)]#reorder


#Adding Import Attributes
Importer_attributes_p1<-BACI_Grav_95_99%>%group_by(BACI_Grav_95_99$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))

Importer_attributes_p1$country<-Importer_attributes_p1$'BACI_Grav_95_99$Importer'
Importer_attributes_p1$'BACI_Grav_95_99$Importer'<-NULL
Importer_attributes_p1<-Importer_attributes_p1[,c(6, 1:5)]#reorder

#2---2000 to 2004####
BACI_Grav_00_04 <- read.csv("../Output/Combined/BACI_Grav_00_04.csv")
str(BACI_Grav_00_04)
BACI_Grav_00_04$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_00_04<-BACI_Grav_00_04[(BACI_Grav_00_04$Exporter!='N/A' & BACI_Grav_00_04$Importer!='N/A'),]


edges_p2<-BACI_Grav_00_04[,which(names(BACI_Grav_00_04)%in%c('Exporter','Importer'))]
edges_p2$Source<-edges_p2$Exporter
edges_p2$Exporter<-NULL
edges_p2$Target<-edges_p2$Importer
edges_p2$Importer<-NULL
nodes_p2<-as.data.frame(unique(BACI_Grav_00_04["Exporter"]))

write.csv(edges_p2, file = "../Output/Network Data/edges_p2.csv", row.names = FALSE) 
write.csv(nodes_p2, file = "../Output/Network Data/nodes_p2.csv", row.names = FALSE)

#Adding Export Attributes
Exporter_attributes_p2<-BACI_Grav_00_04%>%group_by(BACI_Grav_00_04$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))

Exporter_attributes_p2$country<-Exporter_attributes_p2$'BACI_Grav_00_04$Exporter'
Exporter_attributes_p2$'BACI_Grav_00_04$Exporter'<-NULL
Exporter_attributes_p2<-Exporter_attributes_p2[,c(6, 1:5)]#reorder

#Adding Import Attributes
Importer_attributes_p2<-BACI_Grav_00_04%>%group_by(BACI_Grav_00_04$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))

Importer_attributes_p2$country<-Importer_attributes_p2$'BACI_Grav_00_04$Importer'
Importer_attributes_p2$'BACI_Grav_00_04$Importer'<-NULL
Importer_attributes_p2<-Importer_attributes_p2[,c(6, 1:5)]#reorder

#3---2005 to 2009####
BACI_Grav_05_09 <- read.csv("../Output/Combined/BACI_Grav_05_09.csv")
str(BACI_Grav_05_09)
BACI_Grav_05_09$X<- NULL

#we have one country which is labled as N/A remove it. 
BACI_Grav_05_09<-BACI_Grav_05_09[(BACI_Grav_05_09$Exporter!='N/A' & BACI_Grav_05_09$Importer!='N/A'),]


edges_p3<-BACI_Grav_05_09[,which(names(BACI_Grav_05_09)%in%c('Exporter','Importer'))]
edges_p3$Source<-edges_p3$Exporter
edges_p3$Exporter<-NULL
edges_p3$Target<-edges_p3$Importer
edges_p3$Importer<-NULL
nodes_p3<-as.data.frame(unique(BACI_Grav_05_09["Exporter"]))

write.csv(edges_p3, file = "../Output/Network Data/edges_p3.csv", row.names = FALSE) 
write.csv(nodes_p3, file = "../Output/Network Data/nodes_p3.csv", row.names = FALSE)

#Adding Export Attributes
Exporter_attributes_p3<-BACI_Grav_05_09%>%group_by(BACI_Grav_05_09$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))

Exporter_attributes_p3$country<-Exporter_attributes_p3$'BACI_Grav_05_09$Exporter'
Exporter_attributes_p3$'BACI_Grav_05_09$Exporter'<-NULL
Exporter_attributes_p3<-Exporter_attributes_p3[,c(6, 1:5)]#reorder

#Adding Import Attributes
Importer_attributes_p3<-BACI_Grav_05_09%>%group_by(BACI_Grav_05_09$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))

Importer_attributes_p3$country<-Importer_attributes_p3$'BACI_Grav_05_09$Importer'
Importer_attributes_p3$'BACI_Grav_05_09$Importer'<-NULL
Importer_attributes_p3<-Importer_attributes_p3[,c(6, 1:5)]#reorder

#4---2010 to 2014####
BACI_Grav_10_14 <- read.csv("../Output/Combined/BACI_Grav_10_14.csv")
str(BACI_Grav_10_14)
BACI_Grav_10_14$X<- NULL
#we have one country which is labled as N/A remove it. 
BACI_Grav_10_14<-BACI_Grav_10_14[(BACI_Grav_10_14$Exporter!='N/A' & BACI_Grav_10_14$Importer!='N/A'),]


edges_p4<-BACI_Grav_10_14[,which(names(BACI_Grav_10_14)%in%c('Exporter','Importer'))]
edges_p4$Source<-edges_p4$Exporter
edges_p4$Exporter<-NULL
edges_p4$Target<-edges_p4$Importer
edges_p4$Importer<-NULL
nodes_p4<-as.data.frame(unique(BACI_Grav_10_14["Exporter"]))

write.csv(edges_p4, file = "../Output/Network Data/edges_p4.csv", row.names = FALSE) 
write.csv(nodes_p4, file = "../Output/Network Data/nodes_p4.csv", row.names = FALSE)

#Adding Export Attributes
Exporter_attributes_p4<-BACI_Grav_10_14%>%group_by(BACI_Grav_10_14$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))

Exporter_attributes_p4$country<-Exporter_attributes_p4$'BACI_Grav_10_14$Exporter'
Exporter_attributes_p4$'BACI_Grav_10_14$Exporter'<-NULL
Exporter_attributes_p4<-Exporter_attributes_p4[,c(6, 1:5)]#reorder

#Adding Import Attributes
Importer_attributes_p4<-BACI_Grav_10_14%>%group_by(BACI_Grav_10_14$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))

Importer_attributes_p4$country<-Importer_attributes_p4$'BACI_Grav_10_14$Importer'
Importer_attributes_p4$'BACI_Grav_10_14$Importer'<-NULL
Importer_attributes_p4<-Importer_attributes_p4[,c(6, 1:5)]#reorder

#5---2015 to 2017####
BACI_Grav_15_17 <- read.csv("../Output/Combined/BACI_Grav_15_17.csv")
str(BACI_Grav_15_17)
BACI_Grav_15_17$X<- NULL

#we have one country which is labled as N/A remove it. 
BACI_Grav_15_17<-BACI_Grav_15_17[(BACI_Grav_15_17$Exporter!='N/A' & BACI_Grav_15_17$Importer!='N/A'),]


edges_p5<-BACI_Grav_15_17[,which(names(BACI_Grav_15_17)%in%c('Exporter','Importer'))]
edges_p5$Source<-edges_p5$Exporter
edges_p5$Exporter<-NULL
edges_p5$Target<-edges_p5$Importer
edges_p5$Importer<-NULL
nodes_p5<-as.data.frame(unique(BACI_Grav_15_17["Exporter"]))

write.csv(edges_p5, file = "../Output/Network Data/edges_p5.csv", row.names = FALSE) 
write.csv(nodes_p5, file = "../Output/Network Data/nodes_p5.csv", row.names = FALSE)

#Adding Export Attributes
Exporter_attributes_p5<-BACI_Grav_15_17%>%group_by(BACI_Grav_15_17$Exporter)%>%
  summarise(landlocked_o=ifelse(sum(landlocked_o,na.rm=TRUE)>=1, 1, 0),
            island_o=ifelse(sum(island_o,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_o=mean(gdp_wdi_const_o,na.rm=TRUE),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o,na.rm=TRUE),
            pop_o=mean(pop_o,na.rm=TRUE))

Exporter_attributes_p5$country<-Exporter_attributes_p5$'BACI_Grav_15_17$Exporter'
Exporter_attributes_p5$'BACI_Grav_15_17$Exporter'<-NULL
Exporter_attributes_p5<-Exporter_attributes_p5[,c(6, 1:5)]#reorder

#Adding Import Attributes
Importer_attributes_p5<-BACI_Grav_15_17%>%group_by(BACI_Grav_15_17$Importer)%>%
  summarise(landlocked_d=ifelse(sum(landlocked_d,na.rm=TRUE)>=1, 1, 0),
            island_d=ifelse(sum(island_d,na.rm=TRUE) >= 1, 1, 0), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d,na.rm=TRUE),
            gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d,na.rm=TRUE),
            pop_d=mean(pop_d,na.rm=TRUE))

Importer_attributes_p5$country<-Importer_attributes_p5$'BACI_Grav_15_17$Importer'
Importer_attributes_p5$'BACI_Grav_15_17$Importer'<-NULL
Importer_attributes_p5<-Importer_attributes_p5[,c(6, 1:5)]#reorder