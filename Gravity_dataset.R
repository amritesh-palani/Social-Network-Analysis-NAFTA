library(tidyverse)
library(plyr)

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


#Load all trade dataset####
#from https://www.usitc.gov/data/gravity/dataset.htm

release1948_1977<-read.csv("C:/Users/sandr/Desktop/Social Network Analysis/Social-Network-Analysis-NAFTA/release_1.0_1948_1977.csv")
release1978_1992<-read.csv("C:/Users/sandr/Desktop/Social Network Analysis/Social-Network-Analysis-NAFTA/release_1.0_1978_1992.csv")
release1993_2004<-read.csv("C:/Users/sandr/Desktop/Social Network Analysis/Social-Network-Analysis-NAFTA/release_1.0_1993_2004.csv")
release2005_2016<-read.csv("C:/Users/sandr/Desktop/Social Network Analysis/Social-Network-Analysis-NAFTA/release_1.0_2005_2016.csv")
colnames(release1948_1977)==colnames(release1978_1992)
colnames(release1978_1992)==colnames(release1993_2004)
colnames(release1993_2004)==colnames(release2005_2016)
colnames(release1948_1977)

release1948_2016<-do.call("rbind", list(release1948_1977,release1978_1992,release1993_2004,release2005_2016))
#Dataset for 22 years before and 22 after NAFTA came into effect in 1994####
release1971_2016<-release1948_2016[release1948_2016$year>=1971, ]

#NAs
total_na<-transpose(total_na)
total_na<-do.call(rbind,total_na)

#Before NAFTA came into effect
release1971_1993<-release1971_2016[release1971_2016$year<=1993, ]

#Before NAFTA came into effect
release1994_2016<-release1971_2016[release1971_2016$year>=1994, ]

#subset data for Canada, USA  and Mexico.####
#only trade relation of the three countries with the rest of the world
#three countries as exporters
CA_US_MX_1971_2016<-release1971_2016[release1971_2016$country_o %in% c("Canada", "United States","Mexico"), ]

nrow(release1971_2016)
total_na<-release1971_2016 %>%summarise_all(funs(sum(is.na(.))))




