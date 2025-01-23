# this code is used to arrange CCNPCKG Handness result
# copyright: Xue-Ru Fan @BNU, 5 Jan 2023

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "F:/CCNPdataArrange/Demographics"
setwd(filefolder)

#################### arrange raw data ##############################################################

# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "Handness_SWU.csv"
idfile <- "CCNPSWU_IDcode.csv"
rawdata <- read.csv(rawfile)
idcode <- read.csv(idfile)

raw <- merge(rawdata, idcode, by = "ID", all = T)

# modify participant ID and Session

raw$Participant <- str_pad(raw$Participant, 4, side = "left", "0")
raw$Session <- str_pad(raw$Session, 2, side = "left", "0")
raw <- raw[,-1]

######################## Arrange Handness result ###################################################

hand <- raw[, c(2:13)]

####################### here
# calculate handness
HandScore <- data.frame(matrix(ncol = 1, nrow = 0))
for (i in 1:nrow(hand)) {
  data <- as.character(hand[i,])
  if (sum(is.na(data[c(1:4,10,11)])*1) == 0) {
    if (sum(data[c(1:4,10,11)] == c("L","L","L","L","L","L")) == 6) {
      HandScore[i, 1] = "LL"
    } else if (sum(data[c(1:4,10,11)] == c("R","R","R","R","R","R")) == 6) {
      HandScore[i, 1] = "RR"
    } else if (sum(data[c(1:4,10,11)] == c("E","E","E","E","E","E")) == 6) {
        HandScore[i, 1] = "M"
    } else if (data[1] == "L") {
      HandScore[i, 1] = "ML"
    } else if (data[1] == "R") {
      HandScore[i, 1] = "MR"
    } else if (data[1] == "E") {
      HandScore[i, 1] = "M"
    }
  } else {
    next
  }
}
hand <- cbind(raw, HandScore)
colnames(hand)[c(2:13,15)] <- c(seq(1:12),"Handness_Annett")

# keep result of valid measures
hand <- hand[complete.cases(hand[, 15]), ]
hand <- hand[, c(14,1:13,15)]

# save a copy of handness raw result
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(hand, "CCNPCKG_Handness_raw.xlsx", rowNames = F)
  