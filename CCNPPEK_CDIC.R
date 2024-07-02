# this code is used to arrange CCNPPEK CDIC result into tsv file
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

######################## Arrange CDIC result ######################################################

cdic <- raw[, c(1,2, 389:415)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(cdic, "CCNPPEK_CDIC_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
cdic[1,3:29] <- c(seq(1,27))
colnames(cdic) <- cdic[1,]
cdic <- cdic[-1,]

# 按照选项顺序记0、1、2分
cdic[cdic == "1"] <- 0
cdic[cdic == "2"] <- 1
cdic[cdic == "3"] <- 2
cdic <- cdic[complete.cases(cdic),] # 去掉缺这个数据的被试信息

# 反向计分题号
index <- c(2,5,7,8,10,11,13,15,16,18,21,24,25)

for (i in 1:length(index)) {
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 0] <- 3")))
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 2] <- 0")))
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 3] <- 2")))
}

# calculate score

Score <- data.frame(matrix(ncol = 6, nrow = 0))

for (i in 1:nrow(cdic)) {
  score <- as.numeric(cdic[i,3:29])
  Score[i, 1] <- sum(score[c(4,16,17,18,19,20,21,22)])
  Score[i, 2] <- sum(score[c(1,6,8,9,10,11,13)])
  Score[i, 3] <- sum(score[c(2,7,14,25)])
  Score[i, 4] <- sum(score[c(3,15,23,24)])
  Score[i, 5] <- sum(score[c(5,12,26,27)])
  Score[i, 6] <- sum(score[c(1:27)])
}

cdic <- cbind(cdic, Score)
colnames(cdic)[30:35] <- c("Anhedonia", "Negative_Mood", "Negative_Self-Esteem",
                            "Ineffectiveness", "Interpersonal_Problem", "Total_Score")

# save finial result of this scale
cdic <- cdic[, c(1,2,30:35)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(cdic, "CCNPPEK_CDIC.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/CDIC"))
for (i in 1:nrow(cdic)) {
  data <- cdic[i,]
  dataname <- paste0("CCNPPEK", cdic[i, 1], "_ses-", cdic[i, 2], "_task-CDIC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
