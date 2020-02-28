library(tidyverse)
library(plyr)
library(ff)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

gravity1995_1999_agg <- read.csv("../Output/Gravity/gravity1995_1999_agg.csv")
gravity2000_2004_agg <- read.csv("../Output/Gravity/gravity2000_2004_agg.csv")
gravity2005_2009_agg <- read.csv("../Output/Gravity/gravity2005_2009_agg.csv")
gravity2010_2014_agg <- read.csv("../Output/Gravity/gravity2010_2014_agg.csv")
gravity2015_2017_agg <- read.csv("../Output/Gravity/gravity2015_2017_agg.csv")

BACI95_99_agg <- read.csv("../Output/Gravity/BACI95_99_agg.csv")
BACI00_04_agg<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI00_04_agg.csv")
BACI05_09_agg<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI05_09_agg.csv")
BACI10_14_agg<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI10_14_agg.csv")
BACI15_17_agg<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI15_17_agg.csv")


#1995 to 1999####
detach(package:plyr)
BACI_Grav_95_99<-merge(x=BACI95_99_agg, y=gravity1995_1999_agg, by.x = c("Exporter","Importer"), 
                       by.y = c("gravity1995_1999.iso3_o","gravity1995_1999.iso3_d"), all.x = TRUE)
head(BACI_Grav_95_99)
BACI_Grav_95_99$X.x<- NULL  #this shows which row of x matched which row of Y 
BACI_Grav_95_99$X.y<- NULL

#Adding Columns 
BACI_Grav_95_99$Trade_to_GDP_ratio_o <- with(BACI_Grav_95_99, BACI_Grav_95_99$VoT/BACI_Grav_95_99$gdp_wdi_const_o)
BACI_Grav_95_99$Trade_to_GDP_ratio_d <- with(BACI_Grav_95_99, BACI_Grav_95_99$VoT/BACI_Grav_95_99$gdp_wdi_const_d)


write.csv(BACI_Grav_95_99, file = "./Output/BACIBACI_Grav_95_99.csv")

#2000 to 2004####
BACI_Grav_00_04<-merge(x=BACI00_04_agg, y=gravity2000_2004_agg, by.x = c("Exporter","Importer"), 
                       by.y = c("gravity2000_2004.iso3_o","gravity2000_2004.iso3_d"), all.x = TRUE)
head(BACI_Grav_00_04)
BACI_Grav_00_04$X.x<- NULL  #this shows which row of x matched which row of Y 
BACI_Grav_00_04$X.y<- NULL

BACI_Grav_00_04$Trade_to_GDP_ratio_o <- with(BACI_Grav_00_04, BACI_Grav_00_04$VoT/BACI_Grav_00_04$gdp_wdi_const_o)
BACI_Grav_00_04$Trade_to_GDP_ratio_d <- with(BACI_Grav_00_04, BACI_Grav_00_04$VoT/BACI_Grav_00_04$gdp_wdi_const_d)

write.csv(BACI_Grav_00_04, file = "./Output/BACI_Grav_00_04.csv")

#2005 to 2009####
BACI_Grav_05_09<-merge(x=BACI05_09_agg, y=gravity2005_2009_agg, by.x = c("Exporter","Importer"), 
                       by.y = c("gravity2005_2009.iso3_o","gravity2005_2009.iso3_d"), all.x = TRUE)
head(BACI_Grav_05_09)
BACI_Grav_05_09$X.x<- NULL  #this shows which row of x matched which row of Y 
BACI_Grav_05_09$X.y<- NULL

BACI_Grav_05_09$Trade_to_GDP_ratio_o <- with(BACI_Grav_05_09, BACI_Grav_05_09$VoT/BACI_Grav_05_09$gdp_wdi_const_o)
BACI_Grav_05_09$Trade_to_GDP_ratio_d <- with(BACI_Grav_05_09, BACI_Grav_05_09$VoT/BACI_Grav_05_09$gdp_wdi_const_d)

write.csv(BACI_Grav_05_09, file = "./Output/BACI_Grav_05_09.csv")


#2010 to 2014####
BACI_Grav_10_14<-merge(x=BACI10_14_agg, y=gravity2010_2014_agg, by.x = c("Exporter","Importer"), 
                       by.y = c("gravity2010_2014.iso3_o","gravity2010_2014.iso3_d"), all.x = TRUE)
head(BACI_Grav_10_14)
BACI_Grav_10_14$X.x<- NULL  #this shows which row of x matched which row of Y 
BACI_Grav_10_14$X.y<- NULL

BACI_Grav_10_14$Trade_to_GDP_ratio_o <- with(BACI_Grav_10_14, BACI_Grav_10_14$VoT/BACI_Grav_10_14$gdp_wdi_const_o)
BACI_Grav_10_14$Trade_to_GDP_ratio_d <- with(BACI_Grav_10_14, BACI_Grav_10_14$VoT/BACI_Grav_10_14$gdp_wdi_const_d)

write.csv(BACI_Grav_10_14, file = "./Output/BACI_Grav_10_14.csv")


#2015 to 2017####
BACI_Grav_15_17<-merge(x=BACI15_17_agg, y=gravity2015_2017_agg, by.x = c("Exporter","Importer"), 
                       by.y = c("gravity2015_2017.iso3_o","gravity2015_2017.iso3_d"), all.x = TRUE)
head(BACI_Grav_15_17)
BACI_Grav_15_17$X.x<- NULL  #this shows which row of x matched which row of Y 
BACI_Grav_15_17$X.y<- NULL

BACI_Grav_15_17$Trade_to_GDP_ratio_o <- with(BACI_Grav_15_17, BACI_Grav_15_17$VoT/BACI_Grav_15_17$gdp_wdi_const_o)
BACI_Grav_15_17$Trade_to_GDP_ratio_d <- with(BACI_Grav_15_17, BACI_Grav_15_17$VoT/BACI_Grav_15_17$gdp_wdi_const_d)

write.csv(BACI_Grav_15_17, file = "./Output/BACI_Grav_15_17.csv")

