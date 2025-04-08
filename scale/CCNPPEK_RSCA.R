# this code is used to arrange CCNPPEK RSCA result into tsv file
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

######################## Arrange C-PDS result

rsca <- rawdata[, c(1,2, 681:707)]
rsca <- rsca[-1,]
setwd(file.path(filefolder, "raw"))

# save a copy of raw data
write.xlsx(rsca, "CCNPPEK_RSCA_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# apply score according rules
colnames(rsca)[3:29] <- seq(1,27)

# 反向计分题号
index <- c(1,2,5,6,9,12,15,16,17,21,26,27)

for (i in 1:length(index)) {
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 1] <- 6")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 2] <- 7")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 4] <- 2")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 5] <- 1")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 6] <- 5")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 7] <- 4")))
}

# calculate score

Score <- data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:nrow(rsca)) {
  score <- as.numeric(rsca[i,3:29])
  Score[i, 1] <- sum(score[c(3,4,11,20,24)])
  Score[i, 2] <- sum(score[c(1,2,5,21,23,27)])
  Score[i, 3] <- sum(score[c(10,13,14,25)])
  Score[i, 4] <- sum(score[c(8,15,16,17,19,22)])
  Score[i, 5] <- sum(score[c(6,7,9,12,18,26)])
}

rsca <- cbind(rsca, Score)
colnames(rsca)[30:34] <- c("Goal_planning", "Affect_control", "Positive_thinking",
                           "Family_support", "Help_seeking")

# 去掉没有做这个问卷的被试
rsca[is.na(rsca)]=0

for (i in 1:nrow(rsca)) {
  rsca$Total[i] <- sum(as.numeric(rsca[i, 30:34]))
}

rsca <- subset(rsca, Total != 0)
rsca <- rsca[, -35]
rsca[rsca == 0] = NA 

# save finial result of this scale
rsca <- rsca[, c(1,2,30:34)]

setwd(file.path(filefolder, "scale"))
write.xlsx(rsca, "CCNPPEK_RSCA_Batch1234.xlsx", rowNames = F, colNames = T)



################################### 以下是处理Batch123的代码 #######################################

# 
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
# ######################## Arrange RSCA result ######################################################
# 
# rsca <- raw[, c(1,2, 906:932)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(rsca, "CCNPPEK_RSCA_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# rsca[1,3:29] <- c(seq(1,27))
# colnames(rsca) <- rsca[1,]
# rsca <- rsca[-1,]
# 
# # 反向计分题号
# index <- c(1,2,5,6,9,12,15,16,17,21,26,27)
# 
# for (i in 1:length(index)) {
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 1] <- 6")))
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 2] <- 7")))
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 4] <- 2")))
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 5] <- 1")))
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 6] <- 5")))
#   eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 7] <- 4")))
# }
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 5, nrow = 0))
# 
# for (i in 1:nrow(rsca)) {
#   score <- as.numeric(rsca[i,3:29])
#   Score[i, 1] <- sum(score[c(3,4,11,20,24)])
#   Score[i, 2] <- sum(score[c(1,2,5,21,23,27)])
#   Score[i, 3] <- sum(score[c(10,13,14,25)])
#   Score[i, 4] <- sum(score[c(8,15,16,17,19,22)])
#   Score[i, 5] <- sum(score[c(6,7,9,12,18,26)])
# }
# 
# rsca <- cbind(rsca, Score)
# colnames(rsca)[30:34] <- c("Goal_planning", "Affect_control", "Positive_thinking",
#                           "Family_support", "Help-seeking")
# 
# # 去掉没有做这个问卷的被试
# rsca[is.na(rsca)]=0
# 
# for (i in 1:nrow(rsca)) {
#   rsca$Total[i] <- sum(as.numeric(rsca[i, 30:34]))
# }
# 
# rsca <- subset(rsca, Total != 0)
# rsca <- rsca[, -35]
# rsca[rsca == 0] = NA 
# 
# # save finial result of this scale
# rsca <- rsca[, c(1,2,30:34)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(rsca, "CCNPPEK_RSCA.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/RSCA"))
# for (i in 1:nrow(rsca)) {
#   data <- rsca[i,]
#   dataname <- paste0("CCNPPEK", rsca[i, 1], "_ses-", rsca[i, 2], "_task-RSCA_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }
