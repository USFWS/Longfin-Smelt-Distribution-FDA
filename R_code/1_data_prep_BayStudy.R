
#LONGFIN SMELT DATA# 

#### FRONT MATTER ####
# AUTHOR: Vanessa Tobias




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

#### DOWNLOAD DATA ####
# Data and documentation are available at: https://filelib.wildlife.ca.gov/Public/BayStudy/CatchMatrices/
#  -  Download "BayStudy_Fish_Matrices_1980-2023.zip"
#  -  Extract all from the zip file
#  -  Save the Excel files to your data folder
#     -  Midwater Trawl data: BayStudy_MWT_YYYY-YYYY_FishMatrix_MMMMYYYY.xlsx
#     -  Otter Trawl data: BayStudy_OT_YYYY-YYYY_FishMatrix_MMMMYYYY.xlsx
#  - Columns containing Longfin Smelt catch by age class: 
#        LONSMEAge0	LONSMEAge1	LONSMEAge2+
# The datasets used in Tobias and Baxter 2025 were created on November 22, 2016.
# There may be minor differences in the version of the dataset that is currently
# available.



#### READ IN DATA ####
mwt <- read_excel("./Data_Raw/midwater_trawl.xlsx", sheet=2)
ot <- read_excel("./Data_Raw/otter_trawl.xlsx", sheet=2)

#### CLEAN UP DATA ####

mwt <- mwt[, c("Year", "Date", "Station", "Series", "Bay",
               "LONSMEAge0", "LONSMEAge1", "LONSMEAge2+")]
names(mwt) <- c("year", "date", "station", "series", "bay",
                "catch0", "catch1", "catch2")

ot <- ot[, c("Year", "Date", "Station", "Series", "Bay",
             "LONSMEAge0", "LONSMEAge1", "LONSMEAge2+")]
names(ot) <- c("year", "date", "station", "series", "bay",
               "catch0", "catch1", "catch2")

# Add gear labels
mwt$gear <- "mwt"
ot$gear <- "ot"

# Make dataset names identical for merging ####
identical(names(mwt), names(ot))  #check if names are identical

#### CREATE NEW VARIABLES ####
# Time Variables ####
# mwt$doy <- yday(mwt$date)
# ot$doy <- yday(ot$date)

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

# PRESENCE ABSENCE DATA KEEPING GEARS SEPARATE ####

mwt.ot <- rbind(mwt, ot)
mwt.ot$month <- month(mwt.ot$date)

#create cohorts
mwt.ot.co <- data.frame(mwt.ot) %>% melt(id.vars = names(mwt.ot)[1:13]) #leave out the pa vars from the list of id.vars.
names(mwt.ot.co)[c(12, 13)] <- c("age", "present")
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
#remove data after 2015
mwt.ot.co <- mwt.ot.co[which(mwt.ot.co$year < 2016),]


