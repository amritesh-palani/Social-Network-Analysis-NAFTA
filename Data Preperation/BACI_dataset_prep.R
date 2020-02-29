library(tidyverse)
library(plyr)
library(ff)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()
country_code<-read.csv("../Input/country_codes_BACI.csv",sep = ";")


##########################1.  Loading 1995 to 1999 Files####
setwd("../Input/BACI/BACI_HS92_1995-1999/")
getwd()

files95_99 = list.files(pattern="*.csv")

library(data.table)
BACI95_99 = do.call(rbind, lapply(files95_99, fread))
str(BACI95_99)

# Rename Columns###
names(BACI95_99)[names(BACI95_99)=="t"]<-"Year"
names(BACI95_99)[names(BACI95_99)=="k"]<-"Product Category"
names(BACI95_99)[names(BACI95_99)=="i"]<-"Exporter"
names(BACI95_99)[names(BACI95_99)=="j"]<-"Importer"
names(BACI95_99)[names(BACI95_99)=="v"]<-"VoT"
names(BACI95_99)[names(BACI95_99)=="q"]<-"Quantity"

head(BACI95_99)
str(BACI95_99)

#Remove Comma in Vot and Quantity and read as a number###
BACI95_99$VoT <- as.numeric(gsub(",","",BACI95_99$VoT))
BACI95_99$Quantity <- as.numeric(gsub(",",".",BACI95_99$Quantity))


# Update Datatype###
BACI95_99$Year<-as.factor(BACI95_99$Year)
BACI95_99$`Product Category`<-as.factor(BACI95_99$`Product Category`)
BACI95_99$Exporter<-as.integer(BACI95_99$Exporter)
BACI95_99$Importer<-as.integer(BACI95_99$Importer)
BACI95_99$VoT<-as.numeric(BACI95_99$VoT)
BACI95_99$Quantity<-as.numeric(BACI95_99$Quantity)
str(BACI95_99)


# Decode Country.Code for Importer and Exporter###
BACI95_99<-merge(BACI95_99, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI95_99$Importer<-BACI95_99$ISO3.digit.Alpha
BACI95_99$ISO3.digit.Alpha<-NULL

BACI95_99<-merge(BACI95_99, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI95_99$Exporter<-BACI95_99$ISO3.digit.Alpha
BACI95_99$ISO3.digit.Alpha<-NULL

str(BACI95_99)

# Aggregate the Product Category###
detach(package:plyr)
BACI95_99_agg<-BACI95_99%>%group_by(Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI95_99_agg)
str(BACI95_99_agg)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

write.csv(BACI95_99_agg, file = "../Output/BACI/BACI95_99_agg.csv") 



##########################2. Loading 2000 to 2004 Files####
setwd("../Input/BACI/BACI_HS92_2000-2004")
getwd()

files00_04 = list.files(pattern="*.csv")
library(data.table)
BACI00_04 = do.call(rbind, lapply(files00_04, fread))
str(BACI00_04)

# Rename Columns###
names(BACI00_04)[names(BACI00_04)=="t"]<-"Year"
names(BACI00_04)[names(BACI00_04)=="k"]<-"Product Category"
names(BACI00_04)[names(BACI00_04)=="i"]<-"Exporter"
names(BACI00_04)[names(BACI00_04)=="j"]<-"Importer"
names(BACI00_04)[names(BACI00_04)=="v"]<-"VoT"
names(BACI00_04)[names(BACI00_04)=="q"]<-"Quantity"

head(BACI00_04)
str(BACI00_04)

#Remove Comma in VoT and Quantity and read as a number###
BACI00_04$VoT <- as.numeric(gsub(",","",BACI00_04$VoT))
BACI00_04$Quantity <- as.numeric(gsub(",",".",BACI00_04$Quantity))


# Update Datatype###
BACI00_04$Year<-as.factor(BACI00_04$Year)
BACI00_04$`Product Category`<-as.factor(BACI00_04$`Product Category`)
BACI00_04$Exporter<-as.integer(BACI00_04$Exporter)
BACI00_04$Importer<-as.integer(BACI00_04$Importer)
BACI00_04$VoT<-as.numeric(BACI00_04$VoT)
BACI00_04$Quantity<-as.numeric(BACI00_04$Quantity)
str(BACI00_04)


# Decode Country.Code for Importer and Exporter###
BACI00_04<-merge(BACI00_04, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI00_04$Importer<-BACI00_04$ISO3.digit.Alpha
BACI00_04$ISO3.digit.Alpha<-NULL

BACI00_04<-merge(BACI00_04, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI00_04$Exporter<-BACI00_04$ISO3.digit.Alpha
BACI00_04$ISO3.digit.Alpha<-NULL



# Aggregate the Product Category###
detach(package:plyr)
BACI00_04_agg<-BACI00_04%>%group_by(Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI00_04_agg)
str(BACI00_04_agg)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

write.csv(BACI00_04_agg, file = "../Output/BACI/BACI00_04_agg.csv") 


##########################3. Loading 2005 to 2009 Files####
setwd("../Input/BACI/BACI_HS92_2005-2009")
getwd()

files05_09 = list.files(pattern="*.csv")
library(data.table)
BACI05_09 = do.call(rbind, lapply(files05_09, fread))
str(BACI05_09)

# Rename Columns###
names(BACI05_09)[names(BACI05_09)=="t"]<-"Year"
names(BACI05_09)[names(BACI05_09)=="k"]<-"Product Category"
names(BACI05_09)[names(BACI05_09)=="i"]<-"Exporter"
names(BACI05_09)[names(BACI05_09)=="j"]<-"Importer"
names(BACI05_09)[names(BACI05_09)=="v"]<-"VoT"
names(BACI05_09)[names(BACI05_09)=="q"]<-"Quantity"

head(BACI05_09)
str(BACI05_09)

#Remove Comma in VoT and Quantity and read as a number###
BACI05_09$VoT <- as.numeric(gsub(",","",BACI05_09$VoT))
BACI05_09$Quantity <- as.numeric(gsub(",",".",BACI05_09$Quantity))


# Update Datatype###
BACI05_09$Year<-as.factor(BACI05_09$Year)
BACI05_09$`Product Category`<-as.factor(BACI05_09$`Product Category`)
BACI05_09$Exporter<-as.integer(BACI05_09$Exporter)
BACI05_09$Importer<-as.integer(BACI05_09$Importer)
BACI05_09$VoT<-as.numeric(BACI05_09$VoT)
BACI05_09$Quantity<-as.numeric(BACI05_09$Quantity)
str(BACI05_09)

# Aggregate the Product Category###
detach(package:plyr)
BACI05_09_agg<-BACI05_09%>%group_by(Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI05_09_agg)
str(BACI05_09_agg)

# Decode Country.Code for Importer and Exporter###
BACI05_09_agg<-merge(BACI05_09_agg, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI05_09_agg$Importer<-BACI05_09_agg$ISO3.digit.Alpha
BACI05_09_agg$ISO3.digit.Alpha<-NULL

BACI05_09_agg<-merge(BACI05_09_agg, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI05_09_agg$Exporter<-BACI05_09_agg$ISO3.digit.Alpha
BACI05_09_agg$ISO3.digit.Alpha<-NULL


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

write.csv(BACI05_09_agg, file = "../Output/BACI/BACI05_09_agg.csv") 


##########################3. Loading 2010 to 2014 Files####
setwd("../Input/BACI/BACI_HS92_2010-2014")
getwd()

files10_14 = list.files(pattern="*.csv")
library(data.table)
BACI10_14 = do.call(rbind, lapply(files10_14, fread))
str(BACI10_14)

# Rename Columns###
names(BACI10_14)[names(BACI10_14)=="t"]<-"Year"
names(BACI10_14)[names(BACI10_14)=="k"]<-"Product Category"
names(BACI10_14)[names(BACI10_14)=="i"]<-"Exporter"
names(BACI10_14)[names(BACI10_14)=="j"]<-"Importer"
names(BACI10_14)[names(BACI10_14)=="v"]<-"VoT"
names(BACI10_14)[names(BACI10_14)=="q"]<-"Quantity"

head(BACI10_14)
str(BACI10_14)

#Remove Comma in VoT and Quantity and read as a number###
BACI10_14$VoT <- as.numeric(gsub(",","",BACI10_14$VoT))
BACI10_14$Quantity <- as.numeric(gsub(",",".",BACI10_14$Quantity))


# Update Datatype###
BACI10_14$Year<-as.factor(BACI10_14$Year)
BACI10_14$`Product Category`<-as.factor(BACI10_14$`Product Category`)
BACI10_14$Exporter<-as.integer(BACI10_14$Exporter)
BACI10_14$Importer<-as.integer(BACI10_14$Importer)
BACI10_14$VoT<-as.numeric(BACI10_14$VoT)
BACI10_14$Quantity<-as.numeric(BACI10_14$Quantity)
str(BACI10_14)


# Decode Country.Code for Importer and Exporter###
BACI10_14<-merge(BACI10_14, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI10_14$Importer<-BACI10_14$ISO3.digit.Alpha
BACI10_14$ISO3.digit.Alpha<-NULL

BACI10_14<-merge(BACI10_14, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI10_14$Exporter<-BACI10_14$ISO3.digit.Alpha
BACI10_14$ISO3.digit.Alpha<-NULL


# Aggregate the Product Category###
detach(package:plyr)
BACI10_14_agg<-BACI10_14%>%group_by(Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI10_14_agg)
str(BACI10_14_agg)

write.csv(BACI10_14_agg, file = "../Output/BACI/BACI10_14_agg.csv") 


##########################4. Loading 2015 to 2017 Files####
setwd("../Input/BACI/BACI_HS92_2015-2017")
getwd()

files15_17 = list.files(pattern="*.csv")
library(data.table)
BACI15_17 = do.call(rbind, lapply(files15_17, fread))
str(BACI15_17)

# Rename Columns###
names(BACI15_17)[names(BACI15_17)=="t"]<-"Year"
names(BACI15_17)[names(BACI15_17)=="k"]<-"Product Category"
names(BACI15_17)[names(BACI15_17)=="i"]<-"Exporter"
names(BACI15_17)[names(BACI15_17)=="j"]<-"Importer"
names(BACI15_17)[names(BACI15_17)=="v"]<-"VoT"
names(BACI15_17)[names(BACI15_17)=="q"]<-"Quantity"

head(BACI15_17)
str(BACI15_17)

#Remove Comma in VoT and Quantity and read as a number###
BACI15_17$VoT <- as.numeric(gsub(",","",BACI15_17$VoT))
BACI15_17$Quantity <- as.numeric(gsub(",",".",BACI15_17$Quantity))


# Update Datatype###
BACI15_17$Year<-as.factor(BACI15_17$Year)
BACI15_17$`Product Category`<-as.factor(BACI15_17$`Product Category`)
BACI15_17$Exporter<-as.integer(BACI15_17$Exporter)
BACI15_17$Importer<-as.integer(BACI15_17$Importer)
BACI15_17$VoT<-as.numeric(BACI15_17$VoT)
BACI15_17$Quantity<-as.numeric(BACI15_17$Quantity)
str(BACI15_17)


# Decode Country.Code for Importer and Exporter###
BACI15_17<-merge(BACI15_17, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI15_17$Importer<-BACI15_17$ISO3.digit.Alpha
BACI15_17$ISO3.digit.Alpha<-NULL

BACI15_17<-merge(BACI15_17, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI15_17$Exporter<-BACI15_17$ISO3.digit.Alpha
BACI15_17$ISO3.digit.Alpha<-NULL


# Aggregate the Product Category###
detach(package:plyr)
BACI15_17_agg<-BACI15_17%>%group_by(Exporter,Importer)%>%summarise(VoT=mean(VoT),Quantity=mean(Quantity))
head(BACI15_17_agg)
str(BACI15_17_agg)

write.csv(BACI15_17_agg, file = "../Output/BACI/BACI15_17_agg.csv") 




#Load Data for 1995 ####
BACI1995 <- read.csv("../Input/BACI/BACI_HS92_1995-1999/BACI_HS92_1995.csv",sep = ";")
colnames(BACI1995)
str(BACI1995)

# Rename Columns###
names(BACI1995)[names(BACI1995)=="t"] <- "Year"
names(BACI1995)[names(BACI1995)=="k"] <- "Product Category"
names(BACI1995)[names(BACI1995)=="i"] <- "Exporter"
names(BACI1995)[names(BACI1995)=="j"] <- "Importer"
names(BACI1995)[names(BACI1995)=="v"] <- "VoT"
names(BACI1995)[names(BACI1995)=="q"] <- "Quantity"

# Update Datatype###
str(BACI1995)
BACI1995$Year <- as.factor(BACI1995$Year)
BACI1995$`Product Category` <- as.factor(BACI1995$`Product Category`)
BACI1995$Exporter <- as.character(BACI1995$Exporter)
BACI1995$Importer <- as.character(BACI1995$Importer)
BACI1995$VoT <- as.numeric(BACI1995$VoT)
BACI1995$Quantity <- as.numeric(BACI1995$Quantity)

# Decode Country.Code for Importer and Exporter###

BACI1995<-merge(BACI1995, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI1995$Importer<-BACI1995$ISO3.digit.Alpha
BACI1995$ISO3.digit.Alpha<-NULL

BACI1995<-merge(BACI1995, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI1995$Exporter<-BACI1995$ISO3.digit.Alpha
BACI1995$ISO3.digit.Alpha<-NULL
head(BACI1995)
str(BACI1995)

# Disaggregating the Product Category###
detach(package:plyr)
BACI1995_agg<-BACI1995%>%group_by(Year,Exporter,Importer)%>%summarise(VoT=sum(VoT),Quantity=sum(Quantity))
head(BACI1995_agg)
str(BACI1995_agg)


release1993_2004 <- read.csv("https://www.usitc.gov/documents/gravity/release_1.0_1993_2004.csv")
release1993_2004_list<-split(release1993_2004,release1993_2004$year)
release1995<-release1993_2004_list$`1995`

colnames(release1995)
head(release1995$iso3_o)

# Add Attributes from Gravity dataset ###
BACI_Grav_1995<-merge(BACI1995_agg, 
                    release1995[,names(release1995) %in% c("iso3_o", "region_o","gdp_wdi_cur_o",
                                                           "landlocked_o","island_o", "pop_o",
                                                           "iso3_d", "region_d","gdp_wdi_const_d","gdp_wdi_const_o","gdp_wdi_cap_const_o","gdp_wdi_cap_const_o",
                                                           "landlocked_d","island_d", "pop_d",
                                                           "contiguity", "agree_pta_goods","agree_pta_services",
                                                           "agree_cu","agree_eia","agree_fta","agree_psa","agree_pta","polity_o","polity_d")], 
                    by.x=c("Importer","Exporter"), by.y=c("iso3_o","iso3_d"))
head(BACI_Grav_1995)

#Trade value proportion per total of trade value for exporter ##
BACI_Grav_1995<-BACI_Grav_1995%>%group_by(Exporter) %>% mutate(VoT_Proportion = VoT/sum(VoT))

BACI_Grav_1995<-BACI_Grav_1995 %>% mutate_if(is.factor, as.character)
write.csv(BACI_Grav_1995, file = "../Output/BACI/BACI_Grav_1995.csv") 





#Load Data for 2016 ####
BACI2016 <- read.csv("../Input/BACI/BACI_HS92_2015-2017/BACI_HS92_2016.csv",sep = ";")
colnames(BACI2016)
str(BACI2016)

# Rename Columns###
names(BACI2016)[names(BACI2016)=="t"] <- "Year"
names(BACI2016)[names(BACI2016)=="k"] <- "Product Category"
names(BACI2016)[names(BACI2016)=="i"] <- "Exporter"
names(BACI2016)[names(BACI2016)=="j"] <- "Importer"
names(BACI2016)[names(BACI2016)=="v"] <- "VoT"
names(BACI2016)[names(BACI2016)=="q"] <- "Quantity"

# Update Datatype###
str(BACI2016)
BACI2016$Year <- as.factor(BACI2016$Year)
BACI2016$`Product Category` <- as.factor(BACI2016$`Product Category`)
BACI2016$Exporter <- as.character(BACI2016$Exporter)
BACI2016$Importer <- as.character(BACI2016$Importer)
BACI2016$VoT <- as.numeric(BACI2016$VoT)
BACI2016$Quantity <- as.numeric(BACI2016$Quantity)


# Decode Country.Code for Importer and Exporter###

BACI2016<-merge(BACI2016, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Importer", by.y="Country.Code")
BACI2016$Importer<-BACI2016$ISO3.digit.Alpha
BACI2016$ISO3.digit.Alpha<-NULL

BACI2016<-merge(BACI2016, country_code[,names(country_code) %in% c("Country.Code", "ISO3.digit.Alpha")], by.x="Exporter", by.y="Country.Code")
BACI2016$Exporter<-BACI2016$ISO3.digit.Alpha
BACI2016$ISO3.digit.Alpha<-NULL
head(BACI2016)
str(BACI2016)

# Disaggregating the Product Category###
detach(package:plyr)
BACI2016_agg<-BACI2016%>%group_by(Year,Exporter,Importer)%>%summarise(VoT=sum(VoT),Quantity=sum(Quantity))
head(BACI2016_agg)
str(BACI2016_agg)






release2005_2016 <- read.csv("https://www.usitc.gov/documents/gravity/release_1.0_2005_2016.csv")
release2005_2016_list<-split(release2005_2016,release2005_2016$year)
release2016<-release1993_2004_list$`2016`

colnames(release2016)
head(release2016$iso3_o)

# Add Attributes from Gravity dataset ###
BACI_Grav_2016<-merge(BACI2016_agg, 
                    release2016[,names(release2016) %in% c("iso3_o", "region_o","gdp_wdi_cur_o",
                                                           "landlocked_o","island_o", "pop_o",
                                                           "iso3_d", "region_d","gdp_wdi_const_d","gdp_wdi_const_o","gdp_wdi_cap_const_o","gdp_wdi_cap_const_o",
                                                           "landlocked_d","island_d", "pop_d",
                                                           "contiguity", "agree_pta_goods","agree_pta_services",
                                                           "agree_cu","agree_eia","agree_fta","agree_psa","agree_pta","polity_o","polity_d")], 
                    by.x=c("Importer","Exporter"), by.y=c("iso3_o","iso3_d"))
head(BACI_Grav_2016)

#Trade value proportion per total of trade value for exporter ##
BACI_Grav_2016<-BACI_Grav_2016%>%group_by(Exporter) %>% mutate(VoT_Proportion = VoT/sum(VoT))

BACI_Grav_2016<-BACI_Grav_2016 %>% mutate_if(is.factor, as.character)
write.csv(BACI_Grav_2016, file = "../Output/BACI/BACI_Grav_2016.csv") 







