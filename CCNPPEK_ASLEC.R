# this code is used to arrange CCNPPEK ASLEC result into tsv file
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "F:/CCNPdataArrange/Scales"
setwd(filefolder)

#################### arrange raw data ##############################################################

# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPPEK_Questionnaires.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1]

# pick out data from each wave
raw_ses2 <- rawdata[grep("-W2$", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("-W3$", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)

# modify participant ID and add column "Session"
# Session 1
raw_ses1$FID <- str_pad(raw_ses1$FID, 4, side = "left", "0")
ses <- c("Session", rep("01", nrow(raw_ses1)-1))
raw_ses1 <- cbind(raw_ses1$FID, ses, raw_ses1[,2:ncol(raw_ses1)])
colnames(raw_ses1)[1] <- "ID"
# Session 2
raw_ses2$FID <- Replace(raw_ses2$FID, "-W2", "")
raw_ses2$FID <- str_pad(raw_ses2$FID, 4, side = "left", "0")
ses <- rep("02", nrow(raw_ses2))
raw_ses2 <- cbind(raw_ses2$FID, ses, raw_ses2[,2:ncol(raw_ses2)])
colnames(raw_ses2)[1] <- "ID"
# Session 3
raw_ses3$FID <- Replace(raw_ses3$FID, "-W3", "")
raw_ses3$FID <- str_pad(raw_ses3$FID, 4, side = "left", "0")
ses <- rep("03", nrow(raw_ses3))
raw_ses3 <- cbind(raw_ses3$FID, ses, raw_ses3[,2:ncol(raw_ses3)])
colnames(raw_ses3)[1] <- "ID"

# combine 3 sessions together
raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
raw[1,1] <- "Participant"

######################## Arrange ASLEC result ######################################################

aslec <- raw[, c(1,2, 984:1010)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(aslec, "CCNPPEK_ASLEC_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
aslec[1,3:29] <- c(seq(1,27))
colnames(aslec) <- aslec[1,]
aslec <- aslec[-1,]

# 未发生(评分为0)，发生后的影响程度(评分为1-5)
aslec[aslec == "1"] <- "0"
aslec[aslec == "2"] <- "1"
aslec[aslec == "3"] <- "2"
aslec[aslec == "4"] <- "3"
aslec[aslec == "5"] <- "4"
aslec[aslec == "6"] <- "5"

# calculate score

Score <- data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:nrow(aslec)) {
  score <- as.numeric(aslec[i,3:29])
  Score[i, 1] <- sum(score[c(23,21,20,19,26,24,18)])
  Score[i, 2] <- sum(score[c(13,12,11,16,14,17)])
  Score[i, 3] <- sum(score[c(1,2,4,15)])
  Score[i, 4] <- sum(score[c(9,22,25,3)])
  Score[i, 5] <- sum(score[c(7,6,8,10,5)])
}

aslec <- cbind(aslec, Score)
colnames(aslec)[30:34] <- c("Punishment_factor", "Loss_factor", "Interpersonal_relationship_factor",
                            "Learning_pressure_factor", "Health_adaptation_factor")

aslec <- aslec[complete.cases(aslec),] # 去掉缺这个数据的被试信息

# save finial result of this scale
aslec <- aslec[, c(1,2,30:34)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(aslec, "CCNPPEK_ASLEC.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/ASLEC"))
for (i in 1:nrow(aslec)) {
  data <- aslec[i,]
  dataname <- paste0("CCNPPEK", aslec[i, 1], "_ses-", aslec[i, 2], "_task-ASLEC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
