# this code is used to arrange CCNPPEK CLS result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)


################ 以下这部分是处理Batch1234的代码 ######################################################

# define environment variables
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表"
setwd(filefolder)

#################### arrange raw data 
# load raw data file
setwd(file.path(filefolder, "source"))
rawfile <-  "CCNPPEK_Scale_B_Batch1234.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1:-2]


######################## Arrange CSL result 

cls <- rawdata[, c(1,2, 254:277)]

setwd(file.path(filefolder, "raw"))
# save a copy of raw data
write.xlsx(cls, "CCNPPEK_CLS_Batch1234_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
cls[1,3:26] <- c(seq(1,24))
colnames(cls) <- cls[1,]
cls <- cls[-1,]

# 插入题都变为0
index <- c(2,5,7,11,13,15,19,23)+2
cls[, index] <- 0

cls <- cls[complete.cases(cls),] # 去掉缺这个数据的被试信息

# 反向计分题号
index <- c(3,6,9,12,14,17,18,20,21,24)

for (i in 1:length(index)) {
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 1] <- 6")))
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 2] <- 7")))
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 4] <- 2")))
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 5] <- 1")))
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 6] <- 5")))
  eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 7] <- 4")))
}

# calculate score

Score <- data.frame(matrix(ncol = 1, nrow = 0))

for (i in 1:nrow(cls)) {
  score <- as.numeric(cls[i,3:26])
  Score[i, 1] <- sum(score[c(1:24)])
}

cls <- cbind(cls, Score)
colnames(cls)[27] <- c("Loneliness_Score")

# save finial result of this scale
cls <- cls[, c(1,2,27)]

setwd(file.path(filefolder, "scale"))
write.xlsx(cls, "CCNPPEK_CLS_Batch1234.xlsx", rowNames = F, colNames = T)




################################### 以下是处理Batch123的代码 #######################################

# # define environment variables
# filefolder <- "F:/CCNPdataArrange/Scales"
# setwd(filefolder)
# 
# #################### arrange raw data 
# 
# # load raw data file
# setwd(paste0(filefolder, "/RawData/Source"))
# rawfile <-  "CCNPPEK_Questionnaires.xlsx"
# rawdata <- read.xlsx(rawfile, rowNames = F)
# rawdata <- rawdata[, -1]
# 
# # pick out data from each wave
# raw_ses2 <- rawdata[grep("-W2$", rawdata[, 1]),]
# raw_ses3 <- rawdata[grep("-W3$", rawdata[, 1]),]
# raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)
# 
# # modify participant ID and add column "Session"
# # Session 1
# raw_ses1$FID <- str_pad(raw_ses1$FID, 4, side = "left", "0")
# ses <- c("Session", rep("01", nrow(raw_ses1)-1))
# raw_ses1 <- cbind(raw_ses1$FID, ses, raw_ses1[,2:ncol(raw_ses1)])
# colnames(raw_ses1)[1] <- "ID"
# # Session 2
# raw_ses2$FID <- Replace(raw_ses2$FID, "-W2", "")
# raw_ses2$FID <- str_pad(raw_ses2$FID, 4, side = "left", "0")
# ses <- rep("02", nrow(raw_ses2))
# raw_ses2 <- cbind(raw_ses2$FID, ses, raw_ses2[,2:ncol(raw_ses2)])
# colnames(raw_ses2)[1] <- "ID"
# # Session 3
# raw_ses3$FID <- Replace(raw_ses3$FID, "-W3", "")
# raw_ses3$FID <- str_pad(raw_ses3$FID, 4, side = "left", "0")
# ses <- rep("03", nrow(raw_ses3))
# raw_ses3 <- cbind(raw_ses3$FID, ses, raw_ses3[,2:ncol(raw_ses3)])
# colnames(raw_ses3)[1] <- "ID"
# 
# # combine 3 sessions together
# raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
# raw[1,1] <- "Participant"

# ######################## Arrange CSL result 
# 
# cls <- raw[, c(1,2, 479:502)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(cls, "CCNPPEK_CLS_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# cls[1,3:26] <- c(seq(1,24))
# colnames(cls) <- cls[1,]
# cls <- cls[-1,]
# 
# # 插入题都变为0
# index <- c(2,5,7,11,13,15,19,23)+2
# cls[, index] <- 0
# 
# cls <- cls[complete.cases(cls),] # 去掉缺这个数据的被试信息
# 
# # 反向计分题号
# index <- c(3,6,9,12,14,17,18,20,21,24)
# 
# for (i in 1:length(index)) {
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 1] <- 6")))
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 2] <- 7")))
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 4] <- 2")))
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 5] <- 1")))
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 6] <- 5")))
#   eval(parse(text = paste0("cls$'", index[i], "'[cls$'", index[i], "'== 7] <- 4")))
# }
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 1, nrow = 0))
# 
# for (i in 1:nrow(cls)) {
#   score <- as.numeric(cls[i,3:26])
#   Score[i, 1] <- sum(score[c(1:24)])
# }
# 
# cls <- cbind(cls, Score)
# colnames(cls)[27] <- c("Loneliness_Score")
# 
# # save finial result of this scale
# cls <- cls[, c(1,2,27)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(cls, "CCNPPEK_CLS.xlsx", rowNames = F, colNames = T)
