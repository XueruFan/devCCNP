# this code is used to arrange CCNPCKG Demography (Age, Sex and Handness) into tsv file
# copyright: Xue-Ru Fan @BNU, 6 jan 2023

# clear environment
rm(list=ls()) 

# load packages
library(openxlsx)
library(stringr)
library(tidyr)

# define environment variables
filefolder <- "F:/CCNPdataArrange/Demographics"
setwd(filefolder)

#################### arrange raw data ##############################################################

# load data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <- "Demographics_SWU.csv"
rawdata <- read.csv(rawfile)
idfile <- "CCNPSWU_IDcode.csv"
idcode <- read.csv(idfile)
scanfile <- "ScanDate_SWU.csv"
scandate <- read.csv(scanfile)

# modify
rawdata <- merge(rawdata, idcode, by = "ID", all = T)
colnames(rawdata)[3:5] <- c("1", "2", "3")
rawdata <- gather(rawdata, Session, Age, 3:5) # wide to long
rawdata <- rawdata[, -1]
scandate <- scandate[, -1]
rawdata <- merge(rawdata, scandate, by = intersect(names(rawdata), names(scandate)),
                 by.rawdata = by, by.scandate = by, all = T)

rawdata$Participant <- str_pad(rawdata$Participant, 4, side = "left", "0")
rawdata$Session <- str_pad(rawdata$Session, 2, side = "left", "0")

rawdata$Race <- "Asian"
rawdata$BirthDate <- as.Date(as.character(rawdata$BirthDate),"%Y/%m/%d")
rawdata$ScanDate <- as.Date(as.character(rawdata$ScanDate),"%Y/%m/%d")
rawdata <- rawdata[, c("Participant", "Session", "Sex", "Age", "Race", "BirthDate", "ScanDate")]
rawdata <- rawdata[-which(is.na(rawdata$Age)&is.na(rawdata$ScanDate)), ]

# calculate age in years
rawdata$Age_InYears <- floor(as.numeric(rawdata$Age))

# load handness file
setwd(paste0(filefolder, "/RawData"))
hand_file <-  "CCNPCKG_Handness_raw.xlsx"
hand <- read.xlsx(hand_file)
handness <- hand[, c(1,2,15)]

# combine all together
raw <- merge(rawdata, handness, by = intersect(names(rawdata), names(handness)),
                 by.rawdata = by, by.handness = by, all = T)

# statistics 
ckg_w1 <- subset(raw, Session == "01")
ckg_w2 <- subset(raw, Session == "02")
ckg_w3 <- subset(raw, Session == "03")
age_w1 <- cut(ckg_w1$Age, breaks = seq(5, 20, by = 1), right = F, include.lowest = T)
age_w1 <- data.frame(table(age_w1))
age_w2 <- cut(ckg_w2$Age, breaks = seq(5, 20, by = 1), right = F, include.lowest = T)
age_w2 <- data.frame(table(age_w2))
age_w3 <- cut(ckg_w3$Age, breaks = seq(5, 20, by = 1), right = F, include.lowest = T)
age_w3 <- data.frame(table(age_w3))
ckg_age <- data.frame(age_w1, age_w2$Freq, age_w3$Freq)
colnames(ckg_age) <- c("AgeGroup", "CKG_Wave1", "CKG_Wave2", "CKG_Wave3")
enroll_f <- subset(ckg_w1, Sex == "Female")
enroll_m <- subset(ckg_w1, Sex == "Male")
enroll_under12 <- subset(ckg_w1, Age < 12)

# save a copy of raw data and statistic
setwd(paste0(filefolder, "/RawData"))
write.xlsx(raw, "CCNPCKG_Demography_raw.xlsx", rowNames = F, colNames = T)
write.xlsx(ckg_age, "CCNPCKG_AgeGroup.xlsx", rowNames = F, colNames = T)

# save finial result
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1:5,9)]
write.xlsx(result, "CCNPCKG_Demography.xlsx", rowNames = F, colNames = T)

# save each participant's data into tsv file
setwd(paste0(filefolder, "/DataArrange/Demography"))
for (i in 1:nrow(result)) {
  data <- result[i,]
  dataname <- paste0("CCNPCKG", result[i, 1], "_ses-", result[i, 2], "_task-Demography_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

# save a version for open sharing
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1,2,3,9,8)]
write.table(result, "CCNPCKG_Participant.tsv",  sep = "\t",  row.names = F, col.names = T, quote = F,)

# save a version for ccnc sharing 保留小数点后一位有效数字
setwd(paste0(filefolder, "/DataArrange/"))
result <- raw[, c(1,2,3,4,9)]
result$Age10 <- floor(as.numeric(result$Age)*10)
result$ScanAge <- result$Age10/10
result <- result[, c(1,2,3,7,5)]
write.table(result, "CCNPCKG_Demo4CCNC.tsv",  sep = "\t",  row.names = F, col.names = T, quote = F,)
