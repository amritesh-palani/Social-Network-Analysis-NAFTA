library(tidyverse)
library(plyr)

#GRAVITY DATASET: Reformat for (1995 - 1999), (2000 - 2004), (2005 - 2010), (2010 - 2014) and (2015 - 2017) ####

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


#Load all trade dataset####
#from https://www.usitc.gov/data/gravity/dataset.htm
gravity1993_2004<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/release_1.0_1993_2004.csv")
gravity2005_2016<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/release_1.0_2005_2016.csv")
colnames(gravity1993_2004)
colnames(gravity1993_2004)==colnames(gravity1993_2004)
ncol(gravity1993_2004)


gravity1993_2016<-do.call("rbind", list(gravity1993_2004,gravity2005_2016))

#Spliting data to five periods####
str(gravity1993_2016$year)

gravity1995_1999<-gravity1993_2016[gravity1993_2016$year>=1995 & gravity1993_2016$year<=1999, ]
unique(gravity1995_1999$year)

gravity2000_2004<-gravity1993_2016[gravity1993_2016$year>=2000 & gravity1993_2016$year<=2004, ]
unique(gravity2000_2004$year)

gravity2005_2009<-gravity1993_2016[gravity1993_2016$year>=2005 & gravity1993_2016$year<=2009, ]
unique(gravity2005_2009$year)

gravity2010_2014<-gravity1993_2016[gravity1993_2016$year>=2010 & gravity1993_2016$year<=2014, ]
unique(gravity2010_2014$year)

gravity2015_2017<-gravity1993_2016[gravity1993_2016$year>=2015, ]
unique(gravity2015_2017$year)

#Creating Gravity Dataset for each Period Agregated.####

view(gravity1995_1999)
#KEY Attribues are going to be: Origin country and destination country profile, bilateral aggrements and 
#some features describing relationship between countries such as distance, language 
#Refer to dataset dictionary for more information below variables.

#Agregating 1995 to 1999####
detach(package:plyr)
gravity1995_1999_agg<-gravity1995_1999%>%group_by(gravity1995_1999$iso3_o,gravity1995_1999$iso3_d)%>%
  summarise(landlocked_o=max(landlocked_o),landlocked_d=max(landlocked_d),
            island_o=max(island_o), island_d=max(island_d), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d),gdp_wdi_const_o=mean(gdp_wdi_const_d),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o),gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d),
            distance= mean(distance),pop_d=mean(pop_d), pop_o=mean(pop_o),
            contiguity=max(contiguity), agree_pta_goods=max(agree_pta_goods),agree_pta_services=max(agree_pta_services),
            agree_cu=max(agree_cu),agree_eia=max(agree_eia),agree_fta=max(agree_fta),agree_psa=max(agree_psa),
            agree_pta=max(agree_pta),polity_o=max(polity_o),polity_d=max(polity_d), common_language=max(common_language))
str(gravity1995_1999_agg)

write.csv(gravity1995_1999_agg, file = "gravity1995_1999_agg.csv") 

#Agregating 2000 to 2004####
gravity2000_2004_agg<-gravity2000_2004%>%group_by(gravity2000_2004$iso3_o,gravity2000_2004$iso3_d)%>%
  summarise(landlocked_o=max(landlocked_o),landlocked_d=max(landlocked_d),
            island_o=max(island_o), island_d=max(island_d), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d),gdp_wdi_const_o=mean(gdp_wdi_const_d),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o),gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d),
            distance= mean(distance),pop_d=mean(pop_d), pop_o=mean(pop_o),
            contiguity=max(contiguity), agree_pta_goods=max(agree_pta_goods),agree_pta_services=max(agree_pta_services),
            agree_cu=max(agree_cu),agree_eia=max(agree_eia),agree_fta=max(agree_fta),agree_psa=max(agree_psa),
            agree_pta=max(agree_pta),polity_o=max(polity_o),polity_d=max(polity_d), common_language=max(common_language))
str(gravity2000_2004_agg)

write.csv(gravity2000_2004_agg, file = "gravity2000_2004_agg.csv") 

#Agregating 2005 to 2009####
gravity2005_2009_agg<-gravity2005_2009%>%group_by(gravity2005_2009$iso3_o,gravity2005_2009$iso3_d)%>%
  summarise(landlocked_o=max(landlocked_o),landlocked_d=max(landlocked_d),
            island_o=max(island_o), island_d=max(island_d), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d),gdp_wdi_const_o=mean(gdp_wdi_const_d),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o),gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d),
            distance= mean(distance),pop_d=mean(pop_d), pop_o=mean(pop_o),
            contiguity=max(contiguity), agree_pta_goods=max(agree_pta_goods),agree_pta_services=max(agree_pta_services),
            agree_cu=max(agree_cu),agree_eia=max(agree_eia),agree_fta=max(agree_fta),agree_psa=max(agree_psa),
            agree_pta=max(agree_pta),polity_o=max(polity_o),polity_d=max(polity_d), common_language=max(common_language))
str(gravity2005_2009_agg)

write.csv(gravity2005_2009_agg, file = "gravity2005_2009_agg.csv") 

#Agregating 2010 to 2014####
gravity2010_2014_agg<-gravity2010_2014%>%group_by(gravity2010_2014$iso3_o,gravity2010_2014$iso3_d)%>%
  summarise(landlocked_o=max(landlocked_o),landlocked_d=max(landlocked_d),
            island_o=max(island_o), island_d=max(island_d), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d),gdp_wdi_const_o=mean(gdp_wdi_const_d),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o),gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d),
            distance= mean(distance),pop_d=mean(pop_d), pop_o=mean(pop_o),
            contiguity=max(contiguity), agree_pta_goods=max(agree_pta_goods),agree_pta_services=max(agree_pta_services),
            agree_cu=max(agree_cu),agree_eia=max(agree_eia),agree_fta=max(agree_fta),agree_psa=max(agree_psa),
            agree_pta=max(agree_pta),polity_o=max(polity_o),polity_d=max(polity_d), common_language=max(common_language))
str(gravity2010_2014_agg)

write.csv(gravity2010_2014_agg, file = "gravity2010_2014_agg.csv") 

#Agregating 2010 to 2014####
gravity2015_2017_agg<-gravity2015_2017%>%group_by(gravity2015_2017$iso3_o,gravity2015_2017$iso3_d)%>%
  summarise(landlocked_o=max(landlocked_o),landlocked_d=max(landlocked_d),
            island_o=max(island_o), island_d=max(island_d), 
            gdp_wdi_const_d=mean(gdp_wdi_const_d),gdp_wdi_const_o=mean(gdp_wdi_const_d),
            gdp_wdi_cap_const_o=mean(gdp_wdi_cap_const_o),gdp_wdi_cap_const_d=mean(gdp_wdi_cap_const_d),
            distance= mean(distance),pop_d=mean(pop_d), pop_o=mean(pop_o),
            contiguity=max(contiguity), agree_pta_goods=max(agree_pta_goods),agree_pta_services=max(agree_pta_services),
            agree_cu=max(agree_cu),agree_eia=max(agree_eia),agree_fta=max(agree_fta),agree_psa=max(agree_psa),
            agree_pta=max(agree_pta),polity_o=max(polity_o),polity_d=max(polity_d), common_language=max(common_language))
str(gravity2015_2017_agg)

write.csv(gravity2015_2017_agg, file = "gravity2015_2017_agg.csv") 
