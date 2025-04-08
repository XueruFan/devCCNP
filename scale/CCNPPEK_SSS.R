# this code is used to arrange CCNPPEK SSS result into tsv file
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


sss <- rawdata[, c(1,2, 664:680)]
sss <- sss[-1,]

setwd(file.path(filefolder, "raw"))
write.xlsx(sss, "CCNPPEK_SSS_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# apply score according rules
colnames(sss)[3:19] <- seq(1,17)

# “符合”记5分，“有点符合”记4分，“不确定”记3分，“有点不符合”记2分，“不符合”记1发
sss[sss == "1"] <- "6"
sss[sss == "2"] <- "7"
sss[sss == "4"] <- "2"
sss[sss == "5"] <- "1"
sss[sss == "6"] <- "5"
sss[sss == "7"] <- "4"

# calculate score

Score <- data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:nrow(sss)) {
  score <- as.numeric(sss[i,3:19])
  Score[i, 1] <- sum(score[c(1,4,6,7,9)])
  Score[i, 2] <- sum(score[c(8,10,11,13,15,16)])
  Score[i, 3] <- sum(score[c(2,3,5,12,14,17)])
  Score[i, 4] <- sum(score[c(1:17)])
}

sss <- cbind(sss, Score)
colnames(sss)[20:23] <- c("Subjective_support", "Objective_support", "Utilisation_of_support",
                          "Total_Score")

# 去掉没有做这个问卷的被试
sss[is.na(sss)]=0

for (i in 1:nrow(sss)) {
  sss$Total[i] <- sum(as.numeric(sss[i, 20:23]))
}

sss <- subset(sss, Total != 0)
sss <- sss[, -24]
sss[sss == 0] = NA 

# save finial result of this scale
sss <- sss[, c(1,2,20:23)]

setwd(file.path(filefolder, "scale"))
write.xlsx(sss, "CCNPPEK_SSS_Batch1234.xlsx", rowNames = F, colNames = T)




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
# ######################## Arrange SSS result ######################################################
# 
# sss <- raw[, c(1,2, 889:905)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(sss, "CCNPPEK_SSS_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# sss[1,3:19] <- c(seq(1,17))
# colnames(sss) <- sss[1,]
# sss <- sss[-1,]
# 
# # “符合”记5分，“有点符合”记4分，“不确定”记3分，“有点不符合”记2分，“不符合”记1发
# sss[sss == "1"] <- "6"
# sss[sss == "2"] <- "7"
# sss[sss == "4"] <- "2"
# sss[sss == "5"] <- "1"
# sss[sss == "6"] <- "5"
# sss[sss == "7"] <- "4"
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 4, nrow = 0))
# 
# for (i in 1:nrow(sss)) {
#   score <- as.numeric(sss[i,3:19])
#   Score[i, 1] <- sum(score[c(1,4,6,7,9)])
#   Score[i, 2] <- sum(score[c(8,10,11,13,15,16)])
#   Score[i, 3] <- sum(score[c(2,3,5,12,14,17)])
#   Score[i, 4] <- sum(score[c(1:17)])
# }
# 
# sss <- cbind(sss, Score)
# colnames(sss)[20:23] <- c("Subjective_support", "Objective_support", "Utilisation_of_support",
#                           "Total_Score")
# 
# # 去掉没有做这个问卷的被试
# sss[is.na(sss)]=0
# 
# for (i in 1:nrow(sss)) {
#   sss$Total[i] <- sum(as.numeric(sss[i, 20:23]))
# }
# 
# sss <- subset(sss, Total != 0)
# sss <- sss[, -24]
# sss[sss == 0] = NA 
# 
# # save finial result of this scale
# sss <- sss[, c(1,2,20:23)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(sss, "CCNPPEK_SSS.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/SSS"))
# for (i in 1:nrow(sss)) {
#   data <- sss[i,]
#   dataname <- paste0("CCNPPEK", sss[i, 1], "_ses-", sss[i, 2], "_task-SSS_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }
