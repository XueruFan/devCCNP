# this code is used to arrange CCNPPEK STAI result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

# clear environment
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

stai <- rawdata[, c(1,2, 124:163)]

setwd(file.path(filefolder, "raw"))
# save a copy of raw data
write.xlsx(stai, "CCNPPEK_STAI_Batch1234_raw.xlsx", rowNames = F, colNames = F)

######################## Arrange STAI result

stai[1,3:42] <- c(seq(1,40))
colnames(stai) <- stai[1,]
stai <- stai[-1,]

stai <- stai[complete.cases(stai),] # 去掉缺这个数据的被试信息

# 反向计分题号
index <- c(1,2,5,8,10,11,15,16,19,20,21,23,24,26,27,30,33,34,36,39)

for (i in 1:length(index)) {
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 1] <- 5")))
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 2] <- 6")))
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 3] <- 2")))
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 4] <- 1")))
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 5] <- 4")))
  eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 6] <- 3")))
}

# calculate score

Score <- data.frame(matrix(ncol = 3, nrow = 0))

for (i in 1:nrow(stai)) {
  score <- as.numeric(stai[i,3:42])
  Score[i, 1] <- sum(score[c(1:20)])
  Score[i, 2] <- sum(score[c(21:40)])
  Score[i, 3] <- sum(score[c(1:40)])
}

stai <- cbind(stai, Score)
colnames(stai)[43:45] <- c("State_Anxiety", "Trait_Anxiety", "Total_Score")

# save finial result of this scale
stai <- stai[, c(1,2,43:45)]
setwd(file.path(filefolder, "scale"))
write.xlsx(stai, "CCNPPEK_STAI_Batch1234.xlsx", rowNames = F, colNames = T)



################################### 以下是处理Batch123的代码 #######################################
# 
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
# 
# ######################## Arrange STAI result 
# 
# stai <- raw[, c(1,2, 349:388)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(stai, "CCNPPEK_STAI_raw.xlsx", rowNames = F, colNames = F)
# 
# stai[1,3:42] <- c(seq(1,40))
# colnames(stai) <- stai[1,]
# stai <- stai[-1,]
# 
# stai <- stai[complete.cases(stai),] # 去掉缺这个数据的被试信息
# 
# # 反向计分题号
# index <- c(1,2,5,8,10,11,15,16,19,20,21,23,24,26,27,30,33,34,36,39)
# 
# for (i in 1:length(index)) {
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 1] <- 5")))
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 2] <- 6")))
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 3] <- 2")))
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 4] <- 1")))
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 5] <- 4")))
#   eval(parse(text = paste0("stai$'", index[i], "'[stai$'", index[i], "'== 6] <- 3")))
# }
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 3, nrow = 0))
# 
# for (i in 1:nrow(stai)) {
#   score <- as.numeric(stai[i,3:42])
#   Score[i, 1] <- sum(score[c(1:20)])
#   Score[i, 2] <- sum(score[c(21:40)])
#   Score[i, 3] <- sum(score[c(1:40)])
# }
# 
# stai <- cbind(stai, Score)
# colnames(stai)[43:45] <- c("State_Anxiety", "Trait_Anxiety", "Total_Score")
# 
# # save finial result of this scale
# stai <- stai[, c(1,2,43:45)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(stai, "CCNPPEK_STAI.xlsx", rowNames = F, colNames = T)

