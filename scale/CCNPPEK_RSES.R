# this code is used to arrange CCNPPEK RSES result into tsv file
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


rses <- rawdata[, c(1,2, 654:663)]
rses <- rses[-1,]


setwd(file.path(filefolder, "raw"))
# save a copy of raw data
write.xlsx(rses, "CCNPPEK_RSES_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# apply score according rules
colnames(rses)[3:12] <- seq(1,10)

rses <- rses[complete.cases(rses),] # 去掉缺这个数据的被试信息

# 反向计分题号
index <- c(1,2,4,6,7,8)

for (i in 1:length(index)) {
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 1] <- 5")))
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 2] <- 6")))
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 3] <- 2")))
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 4] <- 1")))
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 5] <- 4")))
  eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 6] <- 3")))
}

# calculate score

Score <- data.frame(matrix(ncol = 1, nrow = 0))

for (i in 1:nrow(rses)) {
  score <- as.numeric(rses[i,3:12])
  Score[i, 1] <- sum(score[c(1:10)])
}

rses <- cbind(rses, Score)
colnames(rses)[13] <- c("Total_Score")

# save finial result of this scale
rses <- rses[, c(1,2,13)]

setwd(file.path(filefolder, "scale"))
write.xlsx(rses, "CCNPPEK_RSES_Batch1234.xlsx", rowNames = F, colNames = T)




################ 以下这部分是处理Batch123的代码 ######################################################


# # define environment variables
# filefolder <- "F:/CCNPdataArrange/Scales"
# setwd(filefolder)
# 
# #################### arrange raw data ##############################################################
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
# ######################## Arrange RSES result ######################################################
# 
# rses <- raw[, c(1,2, 879:888)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(rses, "CCNPPEK_RSES_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# rses[1,3:12] <- c(seq(1,10))
# colnames(rses) <- rses[1,]
# rses <- rses[-1,]
# 
# rses <- rses[complete.cases(rses),] # 去掉缺这个数据的被试信息
# 
# # 反向计分题号
# index <- c(1,2,4,6,7,8)
# 
# for (i in 1:length(index)) {
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 1] <- 5")))
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 2] <- 6")))
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 3] <- 2")))
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 4] <- 1")))
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 5] <- 4")))
#   eval(parse(text = paste0("rses$'", index[i], "'[rses$'", index[i], "'== 6] <- 3")))
# }
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 1, nrow = 0))
# 
# for (i in 1:nrow(rses)) {
#   score <- as.numeric(rses[i,3:12])
#   Score[i, 1] <- sum(score[c(1:10)])
# }
# 
# rses <- cbind(rses, Score)
# colnames(rses)[13] <- c("Total_Score")
# 
# # save finial result of this scale
# rses <- rses[, c(1,2,13)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(rses, "CCNPPEK_RSES.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/RSES"))
# for (i in 1:nrow(rses)) {
#   data <- rses[i,]
#   dataname <- paste0("CCNPPEK", rses[i, 1], "_ses-", rses[i, 2], "_task-RSES_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }
