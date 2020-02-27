library(tidyverse)
library(plyr)
#Load Data for 1995
BACI1995<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_HS92/BACI_HS92_1995.csv",sep = ";")
colnames(BACI1995)
str(BACI1995)

# Rename Columns####
names(BACI1995)[names(BACI1995)=="t"]<-"Year"
names(BACI1995)[names(BACI1995)=="k"]<-"Product Category"
names(BACI1995)[names(BACI1995)=="i"]<-"Exporter"
names(BACI1995)[names(BACI1995)=="j"]<-"Importer"
names(BACI1995)[names(BACI1995)=="v"]<-"VoT"
names(BACI1995)[names(BACI1995)=="q"]<-"Quantity"

# Update Datatype####
str(BACI1995)
BACI1995$Year<-as.factor(BACI1995$Year)
BACI1995$`Product Category`<-as.factor(BACI1995$`Product Category`)
BACI1995$Exporter<-as.character(BACI1995$Exporter)
BACI1995$Importer<-as.character(BACI1995$Importer)
BACI1995$VoT<-as.numeric(BACI1995$VoT)
BACI1995$Quantity<-as.numeric(BACI1995$Quantity)

# Decode Country.Code for Importer and Exporter####
country_code<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_code/country_codes_BACI.csv",sep = ";")
product_category<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_code/hs92_6d.csv",sep = ";")
head(country_code)
head(product_category)

BACI1995<-merge(BACI1995, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI1995$Importer<-BACI1995$ISO3.digit.Alpha
BACI1995$ISO3.digit.Alpha<-NULL

BACI1995<-merge(BACI1995, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI1995$Exporter<-BACI1995$ISO3.digit.Alpha
BACI1995$ISO3.digit.Alpha<-NULL
head(BACI1995)
str(BACI1995)

# Disaggregating the Product Category####
detach(package:plyr)
BACI1995_agg<-BACI1995%>%group_by(Year,Exporter,Importer)%>%summarise(VoT=sum(VoT),Quantity=sum(Quantity))
head(BACI1995_agg)
str(BACI1995_agg)


# Add Gravity Dataset ####
release1993_2004<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/release_1.0_1993_2004.csv")
release1993_2004_list<-split(release1993_2004,release1993_2004$year)
release1995<-release1993_2004_list$`1995`

colnames(release1995)
head(release1995$iso3_o)

# Add Attributes from Gravity dataset ####
BACI1995_agg<-merge(BACI1995_agg, 
                release1995[,names(release1995) %in% c("iso3_o", "region_o","gdp_wdi_cur_o",
                                                        "landlocked_o","island_o", "pop_o",
                                                        "iso3_d", "region_d","gdp_wdi_const_d","gdp_wdi_const_o","gdp_wdi_cap_const_o","gdp_wdi_cap_const_o",
                                                        "landlocked_d","island_d", "pop_d",
                                                        "contiguity", "agree_pta_goods","agree_pta_services",
                                                        "agree_cu","agree_eia","agree_fta","agree_psa","agree_pta","polity_o","polity_d")], 
                by.x=c("Importer","Exporter"), by.y=c("iso3_o","iso3_d"))
head(BACI1995_agg)

BACI1995_agg<-BACI1995_agg %>% mutate_if(is.factor, as.character)
write.csv(BACI1995_agg, file = "BACI1995_agg.csv") 

# Decode Country.Code for Importer and Exporter####
country_code<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_code/country_codes_BACI.csv",sep = ";")
product_category<-read.csv("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_code/hs92_6d.csv",sep = ";")
head(country_code)
head(product_category)

##########################Loading 1995 to 1999 Files####
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_HS92/BACI_HS92_1995-1999/")
getwd()
files95_99 = list.files(pattern="*.csv")
BACI95_99 = do.call(rbind, lapply(files95_99, function(x) read.csv(x, stringsAsFactors = FALSE, sep = ";")))
head(BACI95_99)

# Rename Columns####
names(BACI95_99)[names(BACI95_99)=="t"]<-"Year"
names(BACI95_99)[names(BACI95_99)=="k"]<-"Product Category"
names(BACI95_99)[names(BACI95_99)=="i"]<-"Exporter"
names(BACI95_99)[names(BACI95_99)=="j"]<-"Importer"
names(BACI95_99)[names(BACI95_99)=="v"]<-"VoT"
names(BACI95_99)[names(BACI95_99)=="q"]<-"Quantity"

head(BACI95_99)
str(BACI95_99)

#Remove Comma in Vot and Quantity and read as a number####
BACI95_99$VoT <- as.numeric(gsub(",","",BACI95_99$VoT))
BACI95_99$Quantity <- as.numeric(gsub(",",".",BACI95_99$Quantity))


# Update Datatype####
BACI95_99$Year<-as.factor(BACI95_99$Year)
BACI95_99$`Product Category`<-as.factor(BACI95_99$`Product Category`)
BACI95_99$Exporter<-as.character(BACI95_99$Exporter)
BACI95_99$Importer<-as.character(BACI95_99$Importer)
BACI95_99$VoT<-as.numeric(BACI95_99$VoT)
BACI95_99$Quantity<-as.numeric(BACI95_99$Quantity)
str(BACI95_99)


# Decode Country.Code for Importer and Exporter####
BACI95_99<-merge(BACI95_99, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI95_99$Importer<-BACI95_99$ISO3.digit.Alpha
BACI95_99$ISO3.digit.Alpha<-NULL

BACI95_99<-merge(BACI95_99, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI95_99$Exporter<-BACI95_99$ISO3.digit.Alpha
BACI95_99$ISO3.digit.Alpha<-NULL

str(BACI95_99)

# Aggregate the Product Category####
detach(package:plyr)
BACI95_99_agg<-BACI95_99%>%group_by(Year,Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI95_99_agg)
str(BACI95_99_agg)

  
