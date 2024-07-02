# this code is used to arrange CCNPPEK EPQA result into tsv file
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

######################## Arrange EPQA result ######################################################

epqa <- raw[, c(1,2, 1011:1098)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(epqa, "CCNPPEK_EPQA_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
epqa[1,3:90] <- c(seq(1,88))
colnames(epqa) <- epqa[1,]
epqa <- epqa[-1,]

# 是记1分，不是记0分
epqa[epqa == "2"] <- "0"

# 反向计分题号
index <- c(2,6,9,11,18,38,42,46,56,62,72,88,21,29,45,4,8,16,20,24,28,40,44,48,52,54,60,64,70,79,83)

for (i in 1:length(index)) {
  eval(parse(text = paste0("epqa$'", index[i], "'[epqa$'", index[i], "'== 1] <- 3")))
  eval(parse(text = paste0("epqa$'", index[i], "'[epqa$'", index[i], "'== 0] <- 1")))
  eval(parse(text = paste0("epqa$'", index[i], "'[epqa$'", index[i], "'== 3] <- 0")))
}

# calculate score

Score <- data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:nrow(epqa)) {
  score <- as.numeric(epqa[i,3:90])
  Score[i, 1] <- sum(score[c(2,6,9,11,18,22,26,30,34,38,42,46,50,56,62,66,68,72,75,76,81,85,88)])
  Score[i, 2] <- sum(score[c(1,5,10,13,14,17,21,25,29,33,37,41,45,49,53,55,61,65,71,80,84)])
  Score[i, 3] <- sum(score[c(3,7,12,15,19,23,27,31,35,39,43,47,51,57,59,63,67,69,73,74,77,78,82,86)])
  Score[i, 4] <- sum(score[c(4,8,16,20,24,28,32,36,40,44,48,52,54,58,60,64,70,79,83,87)])
}

epqa <- cbind(epqa, Score)
colnames(epqa)[91:94] <- c("P_raw_score", "E_raw_score", "N_raw_score", "L_raw_score")

# 去掉没有做这个问卷的被试
epqa[is.na(epqa)]=0

for (i in 1:nrow(epqa)) {
  epqa$Total[i] <- sum(as.numeric(epqa[i, 91:94]))
}

epqa <- subset(epqa, Total != 0)
epqa <- epqa[, c(1,2,91:94)]
epqa[epqa == 0] = NA 

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(epqa, "CCNPPEK_EPQ_Adult.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/EPQ"))
for (i in 1:nrow(epqa)) {
  data <- epqa[i,]
  dataname <- paste0("CCNPPEK", epqa[i, 1], "_ses-", epqa[i, 2], "_task-EPQ_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
