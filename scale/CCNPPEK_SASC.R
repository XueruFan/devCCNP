# this code is used to arrange CCNPPEK SASC result
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

######################## Arrange SASC result

sasc <- rawdata[, c(1,2, 114:123)]

setwd(file.path(filefolder, "raw"))
# save a copy of raw data
write.xlsx(sasc, "CCNPPEK_SASC_Batch1234_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
sasc[1,3:12] <- c(seq(1,10))
colnames(sasc) <- sasc[1,]
sasc <- sasc[-1,]
# 按照选项顺序记0、1、2分
sasc[sasc == "1"] <- 0
sasc[sasc == "2"] <- 1
sasc[sasc == "3"] <- 2
sasc <- sasc[complete.cases(sasc),] # 去掉缺这个数据的被试信息

# calculate score

Score <- data.frame(matrix(ncol = 3, nrow = 0))

for (i in 1:nrow(sasc)) {
  score <- as.numeric(sasc[i,3:12])
  Score[i, 1] <- sum(score[c(1,2,5,6,8,10)])
  Score[i, 2] <- sum(score[c(3,4,7,9)])
  Score[i, 3] <- sum(score[c(1:10)])
}

sasc <- cbind(sasc, Score)
colnames(sasc)[13:15] <- c("Fear_of_Negative_Evaluation", "Social_Avoidance_and_Distress",
                           "Total_Score")

# save finial result of this scale
sasc <- sasc[, c(1,2,13:15)]
setwd(file.path(filefolder, "scale"))
write.xlsx(sasc, "CCNPPEK_SASC_Batch1234.xlsx", rowNames = F, colNames = T)




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
# 
# ######################## Arrange SASC result 
# 
# sasc <- raw[, c(1,2, 339:348)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(sasc, "CCNPPEK_SASC_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# sasc[1,3:12] <- c(seq(1,10))
# colnames(sasc) <- sasc[1,]
# sasc <- sasc[-1,]
# # 按照选项顺序记0、1、2分
# sasc[sasc == "1"] <- 0
# sasc[sasc == "2"] <- 1
# sasc[sasc == "3"] <- 2
# sasc <- sasc[complete.cases(sasc),] # 去掉缺这个数据的被试信息
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 3, nrow = 0))
# 
# for (i in 1:nrow(sasc)) {
#   score <- as.numeric(sasc[i,3:12])
#   Score[i, 1] <- sum(score[c(1,2,5,6,8,10)])
#   Score[i, 2] <- sum(score[c(3,4,7,9)])
#   Score[i, 3] <- sum(score[c(1:10)])
# }
# 
# sasc <- cbind(sasc, Score)
# colnames(sasc)[13:15] <- c("Fear_of_Negative_Evaluation", "Social_Avoidance_and_Distress",
#                            "Total_Score")
# 
# # save finial result of this scale
# sasc <- sasc[, c(1,2,13:15)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(sasc, "CCNPPEK_SASC.xlsx", rowNames = F, colNames = T)
