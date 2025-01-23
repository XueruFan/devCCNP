# this code is used to arrange CCNPPEK Handness result
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
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表"
setwd(filefolder)

# load raw data file
setwd(file.path(filefolder, "source"))
rawfile <-  "CCNPPEK_Scale_A_Batch1234.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1:-2]

######################## Arrange Handness result ###################################################

hand <- rawdata[, c(1,2, 25:44)]

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
setwd(paste0(filefolder, "/raw/"))
write.xlsx(hand_valid[, c(1:22, 27)], "CCNPPEK_HandnessValid_Batch1234_raw.xlsx", rowNames = F)

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
write.xlsx(hand_invalid[, c(1:22, 27)], "CCNPPEK_HandnessInValid_Batch1234_raw.xlsx", rowNames = F)
  