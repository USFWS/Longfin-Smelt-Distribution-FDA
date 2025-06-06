
#LONGFIN SMELT DATA# 

#### FRONT MATTER ####
# AUTHOR: Vanessa Tobias

# Data provenance ####
# Bay Study data were provided by Kathy Hieb (CDFW) as a an Excel workbook file on November 22, 2016.
#  This version of the Bay Study dataset contains catch totals and CPUE summarized by tow.
#  It covers years 1980:2015.  The original Excel file contains a metadata tab with additional information.
#  There are several additional files with additional metadata (3 Word and 1 Powerpoint).  These datafiles 
#  should accompany this R code file in addition to the Excel file.

# Load Packages ####
library(readxl)  #read excel file
library(lubridate)  #work with dates
library(mgcv)  #GAMs
library(sm)
library(parallel)
library(reshape)
library(magrittr)
library(tidyr)
library(doBy)
library(mgcv)
library(RColorBrewer)
library(berryFunctions) #for classify()


#### READ IN DATA ####
mwt <- read_excel("./Data_Raw/1980-2015_LongfinSmelt_CatchCPUELFData_KH.xlsx", sheet=4)
ot <- read_excel("./Data_Raw/1980-2015_LongfinSmelt_CatchCPUELFData_KH.xlsx", sheet=3)

#### CLEAN UP DATA ####

# Make dataset names identical for merging ####
identical(names(mwt), names(ot))  #check if names are identical
names(mwt)[13:14] <- c("Sal", "Temp")
names(ot)[13:14] <- c("Sal", "Temp")
#mwt$evenodd <- NA
ot$towvolume <- NA
mwt$towarea <- NA
mwt$gear <- "mwt"
ot$gear <- "ot"
#We don't need them to be merged yet, but it will be nice later.


# Clean up names to play nicely with R ####
# catch0, catch1, catch2 = Age0 Catch, Age1 Catch, & Age2+ Catch
# cpue names are similar to catch names
# note that the position of tow area and towvolume are reversed between the mwt and ot datasets

names(mwt) <- c("year", "survey", "date", "station", "series",
                "bay", "chanshoal", "depth", "secchi", "time",
                "tide", "direction", "sal", "temp", "net",
                "tow", "towvolume", "alphacode", "catch0", "catch1", 
                "catch2", "catchtotal", "cpue0", "cpue1", "cpue2",
                "cpuetotal", "towarea", "gear")
names(ot) <- c("year", "survey", "date", "station", "series",
               "bay", "chanshoal", "depth", "secchi", "time",
               "tide", "direction", "sal", "temp", "net",
               "tow", "towarea", "alphacode", "catch0", "catch1", 
               "catch2", "catchtotal", "cpue0", "cpue1", "cpue2",
               "cpuetotal", "towvolume", "gear")

#### CREATE NEW VARIABLES ####
# Time Variables ####
mwt$doy <- yday(mwt$date)
ot$doy <- yday(ot$date)

mwt$month <- month(mwt$date)
ot$month <- month(ot$date)

# Location Variables ####
mwt$bay <- as.factor(mwt$bay)
ot$bay <- as.factor(ot$bay)

mwt$station <- as.factor(mwt$station)
ot$station <- as.factor(ot$station)

# Presence/Absence Variables ####
mwt$pa0 <- as.numeric(mwt$catch0 > 0)  #logical returns T/F; as.numeric(T)=1
mwt$pa1 <- as.numeric(mwt$catch1 > 0)
mwt$pa2 <- as.numeric(mwt$catch2 > 0)
ot$pa0 <- as.numeric(ot$catch0 > 0)  #logical returns T/F; as.numeric(T)=1
ot$pa1 <- as.numeric(ot$catch1 > 0)
ot$pa2 <- as.numeric(ot$catch2 > 0)

# # Seasonal Timeline graphs with CIs in different colors ####
# seasons.all <- data.frame(season = rep(c("Winter", "Spring", "Summer", "Fall"), 3),
#                           num = 1:12,
#                           age = rep(0:2, each = 4))

#### MERGE SEPARATE GEAR DATAFRAMES ####

#add catch for each year class separately
both <- merge(x = mwt, y = ot,
               by = c("year", "date", "station", "series", "bay", "chanshoal"),
               all = TRUE)
names(both)
names(both) <- gsub("\\.y", "\\.ot", names(both))
names(both) <- gsub("\\.x", "\\.mwt", names(both))
both$catch0.tot <- both$catch0.mwt + both$catch0.ot
both$catch1.tot <- both$catch1.mwt + both$catch1.ot
both$catch2.tot <- both$catch2.mwt + both$catch2.ot

#  Presence/Absence Variables ####
both$pa0.tot <- as.numeric(both$catch0.tot > 0)
both$pa1.tot <- as.numeric(both$catch1.tot > 0)
both$pa2.tot <- as.numeric(both$catch2.tot > 0)

#  Presence/Absence category variables ####
#create multicategory presence/absence variable for mlogit model
# mwt  ot  presence category
# 1    1   1 both
# 1    0   2 mwt
# 0    1   3 ot
# 0    0   0 neither
#age-0
both$pa.cat.0 <- NA
both$pa.cat.0 <- factor(both$pa.cat.0, levels = 0:3) # c("both", "mwt", "ot", "neither")) out of order
both$pa.cat.0[which(both$catch0.mwt > 0 & both$catch0.ot > 0)] <- "1"
both$pa.cat.0[which(both$catch0.mwt > 0 & both$catch0.ot == 0)] <- "2"
both$pa.cat.0[which(both$catch0.mwt == 0 & both$catch0.ot > 0)] <- "3"
both$pa.cat.0[which(both$pa0.tot == 0)] <- "0"
#age-1
both$pa.cat.1 <- NA
both$pa.cat.1 <- factor(both$pa.cat.1, levels = 0:3) #c("both", "mwt", "ot", "neither"))
both$pa.cat.1[which(both$pa1.tot == 0)] <- "0"
both$pa.cat.1[which(both$catch1.mwt > 0 & both$catch1.ot > 0)] <- "1"
both$pa.cat.1[which(both$catch1.mwt > 0 & both$catch1.ot == 0)] <- "2"
both$pa.cat.1[which(both$catch1.mwt == 0 & both$catch1.ot > 0)] <- "3"
#age-2
both$pa.cat.2 <- NA
both$pa.cat.2 <- factor(both$pa.cat.2, levels = 0:3) #c("both", "mwt", "ot", "neither"))
both$pa.cat.2[which(both$catch2.mwt > 0 & both$catch2.ot > 0)] <- "1"
both$pa.cat.2[which(both$catch2.mwt > 0 & both$catch2.ot == 0)] <- "2"
both$pa.cat.2[which(both$catch2.mwt == 0 & both$catch2.ot > 0)] <- "3"
both$pa.cat.2[which(both$pa2.tot == 0)] <- "0"

#  Create cohorts from age-class data ####

names(both)

both.co <- data.frame(both[,1:66]) %>% melt(id.vars = names(both)[1:63])
names(both.co)[c(64, 65)] <- c("age", "present")
class(both.co$present)
both.co$age <- as.numeric(as.character(car::recode(both.co$age, "'pa0.tot' = 0; 'pa1.tot' = 1; 'pa2.tot' = 2")))

#label cohorts
both.co$cohort <- NA
both.co$cohort[which(both.co$age == 0)] <- both.co$year[which(both.co$age == 0)]
both.co$cohort[which(both.co$age == 1)] <- both.co$year[which(both.co$age == 1)] - 1
both.co$cohort[which(both.co$age == 2)] <- both.co$year[which(both.co$age == 2)] - 2

#create months for 36-month "year"
both.co$month36 <- NA
both.co$month36[which(both.co$age == 0)] <- both.co$month.mwt[which(both.co$age == 0)]
both.co$month36[which(both.co$age == 1)] <- both.co$month.mwt[which(both.co$age == 1)] + 12
both.co$month36[which(both.co$age == 2)] <- both.co$month.mwt[which(both.co$age == 2)] + 24

#remove data from prior to 1987
both.co <- both.co[which(both.co$year > 1986),] #start of Series 2

#create months for the long-term trend (to use instead of years)
both.co$monthlt <- (both.co$year - 1987)*12 + both.co$month.mwt #month got duplicated in the dataset. It doens't matter whether it's month.mwt or month.ot


# #  Create cohorts with multicategory presence from age-class data ####
# both.cocat <- data.frame(both[,c(1:63, 67:69)]) %>% melt(id.vars = names(both)[1:63]) #ID vars + cat vars
# names(both.cocat)[c(64, 65)] <- c("age", "pres.cat")
# class(both.cocat$pres.cat)
# both.cocat$age <- as.numeric(as.character(car::recode(both.cocat$age, "'pa.cat.0' = 0; 'pa.cat.1' = 1; 'pa.cat.2' = 2")))
# #label cohorts
# both.cocat$cohort <- NA
# both.cocat$cohort[both.cocat$age == 0] <- both.cocat$year[both.cocat$age == 0]
# both.cocat$cohort[both.cocat$age == 1] <- both.cocat$year[both.cocat$age == 1] - 1
# both.cocat$cohort[both.cocat$age == 2] <- both.cocat$year[both.cocat$age == 2] - 2
# 
# #create months for 36-month "year"
# both.cocat$month36 <- NA
# both.cocat$month36[both.cocat$age == 0] <- both.cocat$month.mwt[both.cocat$age == 0]
# both.cocat$month36[both.cocat$age == 1] <- both.cocat$month.mwt[both.cocat$age == 1] + 12
# both.cocat$month36[both.cocat$age == 2] <- both.cocat$month.mwt[both.cocat$age == 2] + 24
# 
# #remove data from prior to 1987
# both.cocat <- both.cocat[which(both.cocat$year > 1986),] #start of Series 2
# 
# #create months for the long-term trend (to use instead of years)
# both.cocat$monthlt <- (both.cocat$year - 1987)*12 + both.cocat$month.mwt #month got duplicated in the dataset. It doens't matter whether it's month.mwt or month.ot


# PRESENCE ABSENCE DATA KEEPING GEARS SEPARATE ####

mwt.ot <- rbind(mwt, ot)
mwt.ot$month <- month(mwt.ot$date)

#create cohorts
mwt.ot.co <- data.frame(mwt.ot) %>% melt(id.vars = names(mwt.ot)[1:30]) #leave out the pa vars from the list of id.vars.
names(mwt.ot.co)[c(31, 32)] <- c("age", "present")
class(mwt.ot.co$present)
mwt.ot.co$age <- as.character(mwt.ot.co$age) #not sure if this is necessary
mwt.ot.co$age <- as.numeric(as.character(car::recode(mwt.ot.co$age, "'pa0' = 0; 'pa1' = 1; 'pa2' = 2"))) #different than the both.co code because there is not pa.tot

#label cohorts
mwt.ot.co$cohort <- NA
mwt.ot.co$cohort[mwt.ot.co$age == 0] <- mwt.ot.co$year[mwt.ot.co$age == 0]
mwt.ot.co$cohort[mwt.ot.co$age == 1] <- mwt.ot.co$year[mwt.ot.co$age == 1] - 1
mwt.ot.co$cohort[mwt.ot.co$age == 2] <- mwt.ot.co$year[mwt.ot.co$age == 2] - 2

#create months for 36-month "year"
mwt.ot.co$month36 <- NA
mwt.ot.co$month36[mwt.ot.co$age == 0] <- mwt.ot.co$month[mwt.ot.co$age == 0]
mwt.ot.co$month36[mwt.ot.co$age == 1] <- mwt.ot.co$month[mwt.ot.co$age == 1] + 12
mwt.ot.co$month36[mwt.ot.co$age == 2] <- mwt.ot.co$month[mwt.ot.co$age == 2] + 24

#remove data from prior to 1987
mwt.ot.co <- mwt.ot.co[which(mwt.ot.co$year > 1986),] #start of Series 2

#create months for the long-term trend (to use instead of years)
mwt.ot.co$monthlt <- (mwt.ot.co$year - 1987)*12 + mwt.ot.co$month

