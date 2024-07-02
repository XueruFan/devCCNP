# this code is used to arrange CCNPPEK Handness result into tsv file
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
rawfile <-  "BasicInfo_CAS.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1]

# pick out data from each wave
raw_ses2 <- rawdata[grep("_w2", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("_w3", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)

# modify participant ID and add column "Session"
# Session 1
raw_ses1$FID <- Replace(raw_ses1$FID, "_w1", "")
raw_ses1$FID <- str_pad(raw_ses1$FID, 4, side = "left", "0")
ses <- c("Session", rep("01", nrow(raw_ses1)-1))
raw_ses1 <- cbind(raw_ses1$FID, ses, raw_ses1[,2:ncol(raw_ses1)])
colnames(raw_ses1)[1] <- "ID"
# Session 2
raw_ses2$FID <- Replace(raw_ses2$FID, "_w2", "")
raw_ses2$FID <- str_pad(raw_ses2$FID, 4, side = "left", "0")
ses <- rep("02", nrow(raw_ses2))
raw_ses2 <- cbind(raw_ses2$FID, ses, raw_ses2[,2:ncol(raw_ses2)])
colnames(raw_ses2)[1] <- "ID"
# Session 3
raw_ses3$FID <- Replace(raw_ses3$FID, "_w3", "")
raw_ses3$FID <- str_pad(raw_ses3$FID, 4, side = "left", "0")
ses <- rep("03", nrow(raw_ses3))
raw_ses3 <- cbind(raw_ses3$FID, ses, raw_ses3[,2:ncol(raw_ses3)])
colnames(raw_ses3)[1] <- "ID"

# combine 3 sessions together
raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
raw[1,1] <- "Participant"

######################## Arrange Handness result ###################################################

hand <- raw[, c(1,2, 26:45)]

# calculate score for both hands
HandScore <- data.frame(matrix(ncol = 5, nrow = 0))
for (i in 2:nrow(hand)) {
  data <- as.numeric(hand[i,c(3:22)])
  data[is.na(data)] <- 0
  
  # when the measure is valid, calculate for each hand
  l6 <- sum(data[c(1,3,5,7,9,11)])
  r6 <- sum(data[c(2,4,6,8,10,12)])
  l4 <- sum(data[c(13,15,17,19)])
  r4 <- sum(data[c(14,16,18,20)])
  HandScore[i, 1] <- l6
  HandScore[i, 2] <- r6
  HandScore[i, 3] <- l4
  HandScore[i, 4] <- r4
  l <- l6 + l4
  r <- r6 + r4
  
  # apply handness class
  if (r == 20) {
    HandScore[i, 5] <- "RR"
  } else if (l == 20) {
    HandScore[i, 5] <- "LL"
  } else if (r6 == 12 & r4 < 8) {
    HandScore[i, 5] <- "R"
  } else if (l6 == 12 & l4 < 8) {
    HandScore[i, 5] <- "L"
  } else if (r6 < 12 & data[2] == 2) {
    HandScore[i, 5] <- "MR"
  } else if (r6 < 12 & data[2] == 1 & data[1] == 1) {
    HandScore[i, 5] <- "M"
  } else if (l6 < 12 & data[1] == 2) {
    HandScore[i, 5] <- "ML"
  } else {
    next
  }
}

hand <- cbind(hand, HandScore)
hand[1, c(23:27)] <- c("L6", "R6", "L4", "R4", "Handness_ChineseEHI")
colnames(hand) <- hand[1,]
hand <- hand[-1,]

# keep result of valid measures
hand_valid <- hand[complete.cases(hand[, 27]), ]

# save a copy of valid handness raw result
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(hand_valid[, c(1:22, 27)], "CCNPPEK_HandnessValid_raw.xlsx", rowNames = F)

# arrange invalid measures
hand_invalid <- setdiff(hand, hand_valid)

# 被试误解了答题方式，但是作答有效
HandScore <- data.frame(matrix(ncol = 5, nrow = 0))
for (i in 1:nrow(hand_invalid)) {
  data <- as.numeric(hand_invalid[i,c(3:22)])
  data[is.na(data)] <- 0
  
  for (m in 1:10) {
    a <- m * 2 - 1
    answer <- data[a] + data[a+1]
    if (answer == 1) {
      data[a] <- data[a] *2
      data[a+1] <- data[a+1] *2
    } else if (answer == 2) {
      data[a] <- data[a]
      data[a+1] <- data[a+1]
    } else {
      next
    }
  }
 
  # calculate for each hand
  l6 <- sum(data[c(1,3,5,7,9,11)])
  r6 <- sum(data[c(2,4,6,8,10,12)])
  l4 <- sum(data[c(13,15,17,19)])
  r4 <- sum(data[c(14,16,18,20)])
  HandScore[i, 1] <- l6
  HandScore[i, 2] <- r6
  HandScore[i, 3] <- l4
  HandScore[i, 4] <- r4
  l <- l6 + l4
  r <- r6 + r4
  
  # apply handness class
  if (r == 20) {
    HandScore[i, 5] <- "RR"
  } else if (l == 20) {
    HandScore[i, 5] <- "LL"
  } else if (r6 == 12 & r4 < 8) {
    HandScore[i, 5] <- "R"
  } else if (l6 == 12 & l4 < 8) {
    HandScore[i, 5] <- "L"
  } else if (r6 < 12 & data[2] == 2) {
    HandScore[i, 5] <- "MR"
  } else if (r6 < 12 & data[2] == 1 & data[1] == 1) {
    HandScore[i, 5] <- "M"
  } else if (l6 < 12 & data[1] == 2) {
    HandScore[i, 5] <- "ML"
  } else {
    next
  }
}

hand_invalid[, 23:27] <- HandScore

# save a copy of invalid handness raw result
write.xlsx(hand_invalid[, c(1:22, 27)], "CCNPPEK_HandnessInValid_raw.xlsx", rowNames = F)
  