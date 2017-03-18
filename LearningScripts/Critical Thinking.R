# Try to detect weaknesses in regards of this code. Not just the R coding. Obviously, it works properly. Instead, consider the rational behind these new formed categories.
# Tasks sequence:
# a) Read demographics to understand the meaning of 1st.Data.Base.System.
# c) Analyze the distribution of this feature in the dataset before recoding.
# b) Be critical about the decisions we have taken when reducing the categories using next code. 
# 
# We would like to receive a thoughtful report about it. 
# br
# F. 
# rm(list = ls())
library("dplyr")
library("entropy")
library("magrittr")
library("ggplot2")
library("ggthemes")
setwd("D:/R")
source("import_ISBSG.R")
source("filter_projects.R")
# Recode variable 1DBS
# Replace; Or space or / (and therefore is between []) followed by any character repeated every time
# ISBSG $ X1st.Data.Base.System <- sub ("[; /].*", ";", ISBSG $ X1st.Data.Base.System)
# Replace; Followed by any repeated character every time
# Removing only the string that follows;
# Of 104 levels is passed to 83 levels.
ISBSG$X1st.Data.Base.System <- sub("[;].*", ";", ISBSG$X1st.Data.Base.System)
# Categorization by expert
# Category ACCESS
ISBSG$X1st.Data.Base.System <- sub("ACCESS[; ].*", "ACCESS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("MS Access;", "ACCESS", ISBSG$X1st.Data.Base.System)
# ADABAS
ISBSG$X1st.Data.Base.System <- sub("ADABAS;", "ADABAS", ISBSG$X1st.Data.Base.System)
# Attain
ISBSG$X1st.Data.Base.System <- sub("Micosoft.*", "Attain", ISBSG$X1st.Data.Base.System)
# DB2
ISBSG$X1st.Data.Base.System <- sub("DB2[; /].*", "DB2", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IBM DB2", "DB2", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("UDB2", "DB2", ISBSG$X1st.Data.Base.System)
# Domino
ISBSG$X1st.Data.Base.System <- sub("Domino[ ].*", "Domino", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("LOTUS.*", "Domino", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("Notes.*", "Domino", ISBSG$X1st.Data.Base.System)
# Exchange
ISBSG$X1st.Data.Base.System <- sub("Exchange.*", "Exchange", ISBSG$X1st.Data.Base.System)
# Foxpro
ISBSG$X1st.Data.Base.System <- sub("FOXPRO;", "Foxpro", ISBSG$X1st.Data.Base.System)
# HIRDB
ISBSG$X1st.Data.Base.System <- sub("HIRDB;", "HIRDB", ISBSG$X1st.Data.Base.System)
# IMS
ISBSG$X1st.Data.Base.System <- sub("DB[/].*", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("DEDB;", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IDMS[; -].*", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IMS.*", "IMS", ISBSG$X1st.Data.Base.System)
# MS SQL
ISBSG$X1st.Data.Base.System <- sub("MS[- ]SQL[; ].*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("MSDE.*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("SQL Server[; ].*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("SQL;", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("VSE/.*", "MS SQL", ISBSG$X1st.Data.Base.System)
# NCR
ISBSG$X1st.Data.Base.System <- sub("NCR;", "NCR", ISBSG$X1st.Data.Base.System)
# ORACLE
ISBSG$X1st.Data.Base.System <- sub("Oracle.*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("Personal O.*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("RDB[; ].*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("CICS;", "ORACLE", ISBSG$X1st.Data.Base.System)
# SAS
ISBSG$X1st.Data.Base.System <- sub("SAS;", "SAS", ISBSG$X1st.Data.Base.System)
# Solid
ISBSG$X1st.Data.Base.System <- sub("Solid;", "Solid", ISBSG$X1st.Data.Base.System)
# SYBASE
ISBSG$X1st.Data.Base.System <- sub("SYBASE.*", "SYBASE", ISBSG$X1st.Data.Base.System)
# Unspecified
ISBSG$X1st.Data.Base.System <- sub("Yes", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("ISAM;", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("multiple;", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("VSAM[; ].*", "Unspecified", ISBSG$X1st.Data.Base.System)
# Watcom
ISBSG$X1st.Data.Base.System <- sub("WATCOM[; ].*", "Watcom", ISBSG$X1st.Data.Base.System)
# WGRES
ISBSG$X1st.Data.Base.System <- sub("WGRES;", "WGRES", ISBSG$X1st.Data.Base.System)

ISBSG$X1st.Data.Base.System <- as.factor(ISBSG$X1st.Data.Base.System)

source("Distribution.R")
results <- getDistribution(ISBSG,16)
drawDistribution(ISBSG,16)
drawDistribution(iris,3)
# 
# ISBSG <- ISBSG[complete.cases(ISBSG[,16]),]
# X1stDataBaseSystem <- group_by(ISBSG,X1st.Data.Base.System)
# X1stDataBaseSystem1 <- summarise(X1stDataBaseSystem,UsedTimes=n())
# X1stDataBaseSystem1 <- arrange(X1stDataBaseSystem1,desc(UsedTimes))
# X1stDataBaseSystem1 %>% mutate(mean=mean(UsedTimes),Standard_Deviation=sd(UsedTimes))
# X1stDataBaseSystem1$DataBase <- factor(X1stDataBaseSystem1$X1st.Data.Base.System, levels=unique(X1stDataBaseSystem1$X1st.Data.Base.System))
# drawPic <- ggplot(X1stDataBaseSystem1,aes(x=DataBase,y=UsedTimes))
# drawPic+ geom_bar(stat= 'identity', width = 0.5)+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
