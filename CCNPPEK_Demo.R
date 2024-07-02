# this code is used to arrange CCNPPEK Demography (Age, Sex and Handness) into tsv file
# copyright: Xue-Ru Fan @BNU, 28 Dec 2022

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

# load data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <- "Demographics_CAS.csv"
rawdata <- read.csv(rawfile)

# load handness file
setwd(paste0(filefolder, "/RawData"))
hand_valid_file <-  "CCNPPEK_HandnessValid_raw.xlsx"
hand_valid <- read.xlsx(hand_valid_file)
hand_invalid_file <-  "CCNPPEK_HandnessInValid_raw.xlsx"
hand_invalid <- read.xlsx(hand_invalid_file)
handness <- (rbind(hand_valid, hand_invalid))[, c(1,2,23)]

# modify participant ID, Session, Age and Sex
rawdata$ID <- str_pad(rawdata$ID, 4, side = "left", "0")
colnames(rawdata)[c(1,2,4)] <- c("Participant","Sex","Session")
rawdata$Session <- str_pad(rawdata$Session, 2, side = "left", "0")

rawdata$Sex[(rawdata$Sex) == 1] <- "Male"
rawdata$Sex[(rawdata$Sex) == 0] <- "Female"
rawdata$Race <- "Asian"
rawdata$BirthDate <- as.Date(as.character(rawdata$BirthDate),"%Y/%m/%d")
rawdata$ScanDate <- as.Date(as.character(rawdata$ScanDate),"%Y/%m/%d")
rawdata <- rawdata[, c("Participant", "Session", "Sex", "Age", "Race", "BirthDate", "ScanDate")]

raw <- merge(rawdata, handness, by = intersect(names(rawdata), names(handness)),
                 by.rawdata = by, by.handness = by, all = T)

# calculate age in years
raw$Age_InYears <- floor(as.numeric(raw$Age))

# statistics note: 去掉宣武被试P开头
xuanwu <- which(grepl("P", raw$Participant))
raw_ccnp <- raw[-xuanwu,]
raw_ccnp <- raw_ccnp[!is.na(raw_ccnp$Age), ] # 去掉没有年龄数据的被试
pek_w1 <- subset(raw_ccnp, Session == "01")
pek_w2 <- subset(raw_ccnp, Session == "02")
pek_w3 <- subset(raw_ccnp, Session == "03")
age_w1 <- cut(pek_w1$Age, breaks = seq(4, 22, by = 1), right = F, include.lowest = T)
age_w1 <- data.frame(table(age_w1))
age_w2 <- cut(pek_w2$Age, breaks = seq(4, 22, by = 1), right = F, include.lowest = T)
age_w2 <- data.frame(table(age_w2))
age_w3 <- cut(pek_w3$Age, breaks = seq(4, 22, by = 1), right = F, include.lowest = T)
age_w3 <- data.frame(table(age_w3))
pek_age <- data.frame(age_w1, age_w2$Freq, age_w3$Freq)
colnames(pek_age) <- c("AgeGroup", "PEK_Wave1", "PEK_Wave2", "PEK_Wave3")
enroll_f <- subset(pek_w1, Sex == "Female")
enroll_m <- subset(pek_w1, Sex == "Male")
enroll_under12 <- subset(pek_w1, Age < 12)

# save a copy of raw data
setwd(paste0(filefolder, "/RawData"))
write.xlsx(raw, "CCNPPEK_Demography_raw.xlsx", rowNames = F, colNames = T)
write.xlsx(pek_age, "CCNPPEK_AgeGroup.xlsx", rowNames = F, colNames = T)

# save finial result
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1:5,8)]
write.xlsx(result, "CCNPPEK_Demography.xlsx", rowNames = F, colNames = T)

# save each participant's data into tsv file
setwd(paste0(filefolder, "/DataArrange/Demography"))
for (i in 1:nrow(result)) {
  data <- result[i,]
  dataname <- paste0("CCNPPEK", result[i, 1], "_ses-", result[i, 2], "_task-Demography_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

# save a version for open sharing
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1,2,3,8,9)]
result <- result[!grepl("P", result$Participant),] # 去掉宣武被试
write.table(result, "CCNPPEK_Participant.tsv",  sep = "\t",  row.names = F, col.names = T, quote = F,)

# save a version for ccnc sharing 保留小数点后一位有效数字
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1,2,3,4,8)]
result$Age10 <- floor(as.numeric(result$Age)*10)
result$ScanAge <- result$Age10/10
result <- result[, c(1,2,3,7,5)]
write.table(result, "CCNPPEK_Demo4CCNC.tsv",  sep = "\t",  row.names = F, col.names = T, quote = F,)
