library(tidyverse)
library(plyr)

my_files <- list.files("C:/Users/sandr/Desktop/SNA/Social-Network-Analysis-NAFTA/BACI_HS92/BACI_HS92_1995.csv")
BACI1995<-read.csv("C:/Users/sandr/Desktop/Social Network Analysis/Social-Network-Analysis-NAFTA/BACI_HS92/BACI_HS92_1995.csv",sep = ";")
head(BACI1995)
