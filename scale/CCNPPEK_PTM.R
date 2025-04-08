# this code is used to arrange CCNPPEK PTM result into tsv file
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

ptm <- rawdata[, c(1,2, 419:444)]
ptm <- ptm[-1,]

setwd(file.path(filefolder, "raw"))

# save a copy of raw data
write.xlsx(ptm, "CCNPPEK_PTM_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# apply score according rules
colnames(ptm)[3:28] <- c(seq(1,26))

ptm <- ptm[complete.cases(ptm),] # 去掉缺这个数据的被试信息

# calculate score

Score <- data.frame(matrix(ncol = 7, nrow = 0))

for (i in 1:nrow(ptm)) {
  score <- as.numeric(ptm[i,3:28])
  Score[i, 1] <- sum(score[c(1,4,6,14)])
  Score[i, 2] <- sum(score[c(8,12,16,20,22)])
  Score[i, 3] <- sum(score[c(10,17,23,25)])
  Score[i, 4] <- sum(score[c(3,7,11,19,24)])
  Score[i, 5] <- sum(score[c(2,13,18,21,26)])
  Score[i, 6] <- sum(score[c(5,9,15)])
  Score[i, 7] <- sum(score[c(1:26)])
}

ptm <- cbind(ptm, Score)
colnames(ptm)[29:35] <- c("Public", "Anonymous", "Altruism", "Compliant", "Emotional", "Dire",
                          "Total_Score")

# save finial result of this scale
ptm <- ptm[, c(1,2,29:35)]

setwd(file.path(filefolder, "scale"))
write.xlsx(ptm, "CCNPPEK_PTM_Batch1234.xlsx", rowNames = F, colNames = T)



################################### 以下是处理Batch123的代码 #######################################


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
# ######################## Arrange PTM result ######################################################
# 
# ptm <- raw[, c(1,2, 644:669)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(ptm, "CCNPPEK_PTM_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# ptm[1,3:28] <- c(seq(1,26))
# colnames(ptm) <- ptm[1,]
# ptm <- ptm[-1,]
# 
# ptm <- ptm[complete.cases(ptm),] # 去掉缺这个数据的被试信息
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 7, nrow = 0))
# 
# for (i in 1:nrow(ptm)) {
#   score <- as.numeric(ptm[i,3:28])
#   Score[i, 1] <- sum(score[c(1,4,6,14)])
#   Score[i, 2] <- sum(score[c(8,12,16,20,22)])
#   Score[i, 3] <- sum(score[c(10,17,23,25)])
#   Score[i, 4] <- sum(score[c(3,7,11,19,24)])
#   Score[i, 5] <- sum(score[c(2,13,18,21,26)])
#   Score[i, 6] <- sum(score[c(5,9,15)])
#   Score[i, 7] <- sum(score[c(1:26)])
# }
# 
# ptm <- cbind(ptm, Score)
# colnames(ptm)[29:35] <- c("Public", "Anonymous", "Altruism", "Compliant", "Emotional", "Dire",
#                           "Total_Score")
# 
# # save finial result of this scale
# ptm <- ptm[, c(1,2,29:35)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(ptm, "CCNPPEK_PTM.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/PTM"))
# for (i in 1:nrow(ptm)) {
#   data <- ptm[i,]
#   dataname <- paste0("CCNPPEK", ptm[i, 1], "_ses-", ptm[i, 2], "_task-PTM_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }
