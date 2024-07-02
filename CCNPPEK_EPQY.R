# this code is used to arrange CCNPPEK EPQY result into tsv file
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

######################## Arrange EPQY result ######################################################

epqy <- raw[, c(1,2, 503:590)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(epqy, "CCNPPEK_EPQY_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
epqy[1,3:90] <- c(seq(1,88))
colnames(epqy) <- epqy[1,]
epqy <- epqy[-1,]

# 是记1分，不是记0分
epqy[epqy == "2"] <- "0"

# 反向计分题号
index <- c(30,66,77,9,49,4,8,11,16,20,40,67,70,73,80,82,87)

for (i in 1:length(index)) {
  eval(parse(text = paste0("epqy$'", index[i], "'[epqy$'", index[i], "'== 1] <- 3")))
  eval(parse(text = paste0("epqy$'", index[i], "'[epqy$'", index[i], "'== 0] <- 1")))
  eval(parse(text = paste0("epqy$'", index[i], "'[epqy$'", index[i], "'== 3] <- 0")))
}

# calculate score

Score <- data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:nrow(epqy)) {
  score <- as.numeric(epqy[i,3:90])
  Score[i, 1] <- sum(score[c(3,7,12,15,23,30,32,35,39,43,47,51,53,55,59,60,66,77)])
  Score[i, 2] <- sum(score[c(1,5,9,13,17,19,21,25,28,33,37,41,45,49,57,61,64,68,71,75,79,83,85,86,88)])
  Score[i, 3] <- sum(score[c(2,6,10,14,18,22,26,29,34,38,42,46,50,54,58,62,65,69,72,74,76,81,84)])
  Score[i, 4] <- sum(score[c(4,8,11,16,20,24,27,31,36,40,44,48,52,56,63,67,70,73,78,80,82,87)])
}

epqy <- cbind(epqy, Score)
colnames(epqy)[91:94] <- c("P_raw_score", "E_raw_score", "N_raw_score", "L_raw_score")

# 去掉没有做这个问卷的被试
epqy[is.na(epqy)]=0

for (i in 1:nrow(epqy)) {
  epqy$Total[i] <- sum(as.numeric(epqy[i, 91:94]))
}

epqy <- subset(epqy, Total != 0)
epqy <- epqy[, c(1,2,91:94)]
epqy[epqy == 0] = NA 

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(epqy, "CCNPPEK_EPQ_Youth.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/EPQ"))
for (i in 1:nrow(epqy)) {
  data <- epqy[i,]
  dataname <- paste0("CCNPPEK", epqy[i, 1], "_ses-", epqy[i, 2], "_task-EPQ_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
