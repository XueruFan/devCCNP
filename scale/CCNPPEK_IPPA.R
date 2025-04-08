# this code is used to arrange CCNPPEK IPPA result into tsv file
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


ippa <- rawdata[, c(1,2, 579:653)]
ippa <- ippa[-1,]
setwd(file.path(filefolder, "raw"))

# save a copy of raw data
write.xlsx(ippa, "CCNPPEK_IPPA_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# apply score according rules
ippa_f <- ippa[, c(1,2, 3:27)]
ippa_m <- ippa[, c(1,2, 28:52)]
ippa_p <- ippa[, c(1,2, 53:77)]

name <- seq(1, 25)

colnames(ippa_f)[3:27] <- name
colnames(ippa_m)[3:27] <- name
colnames(ippa_p)[3:27] <- name

# 反向计分题号
index_f <- c(3,9,6,14,8,10,11,17,18,23)
index_m <- c(3,9,6,14,8,10,11,17,18,23)
index_p <- c(5,4,9,10,11,18,22,23)

for (i in 1:length(index_f)) {
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 7] <- 4")))
}

for (i in 1:length(index_m)) {
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 7] <- 4")))
}

for (i in 1:length(index_p)) {
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 7] <- 4")))
}

# subset subscales
# father
cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
trust_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

cols_remain<- c("5","6","7","14","15","16","19","24","25")
commu_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

cols_remain<- c("8","10","11","17","18","23")
alien_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

# mather
cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
trust_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

cols_remain<- c("5","6","7","14","15","16","19","24","25")
commu_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

cols_remain<- c("8","10","11","17","18","23")
alien_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

# peer
cols_remain<- c("5","6","8","12","13","14","15","19","20","21")
trust_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

cols_remain<- c("1","2","3","7","16","17","24","25")
commu_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

cols_remain<- c("4","9","10","11","18","22","23")
alien_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

# calculate score

Score <- data.frame(matrix(ncol = 9, nrow = 0))

for (i in 1:nrow(ippa_f)) {
  #father
  score <- as.numeric(trust_f[i,])
  Score[i, 1] <- sum(score)
  
  score <- as.numeric(commu_f[i,])
  Score[i, 2] <- sum(score)
  
  score <- as.numeric(alien_f[i,])
  Score[i, 3] <- sum(score)
  
  #mother
  score <- as.numeric(trust_m[i,])
  Score[i, 4] <- sum(score)
  
  score <- as.numeric(commu_m[i,])
  Score[i, 5] <- sum(score)
  
  score <- as.numeric(alien_m[i,])
  Score[i, 6] <- sum(score)
  
  #peer
  score <- as.numeric(trust_p[i,])
  Score[i, 7] <- sum(score)
  
  score <- as.numeric(commu_p[i,])
  Score[i, 8] <- sum(score)
  
  score <- as.numeric(alien_p[i,])
  Score[i, 9] <- sum(score)
}

IPPA <- cbind(ippa_f$Participant, ippa_f$Session, Score)
colnames(IPPA) <- c("Participant", "Session","Father_Trust", "Father_Communication",
                    "Father_Alienation", "Mother_Trust", "Mother_Communication",
                    "Mother_Alienation", "Peer_Trust", "Peer_Communication", "Peer_Alienation")


# 计算三个量表的量表分
for (i in 1:nrow(IPPA)) {
  IPPA$Father[i] <- sum(as.numeric(IPPA[i, 3:5]))
  IPPA$Mother[i] <- sum(as.numeric(IPPA[i, 6:8]))
  IPPA$Peer[i] <- sum(as.numeric(IPPA[i, 9:11]))
}


IPPA[is.na(IPPA)]=0

for (i in 1:nrow(IPPA)) {
  IPPA$Total[i] <- sum(as.numeric(IPPA[i, 12:14]))
}

IPPA <- subset(IPPA, Total != 0)
IPPA <- IPPA[, -15]
IPPA[IPPA == 0] = NA 

# save finial result of this scale
setwd(file.path(filefolder, "scale"))
write.xlsx(IPPA, "CCNPPEK_IPPA_Batch1234.xlsx", rowNames = F, colNames = T)




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
# ######################## Arrange IPPA result ######################################################
# 
# ippa <- raw[, c(1,2, 804:878)]
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(ippa, "CCNPPEK_IPPA_raw.xlsx", rowNames = F, colNames = F)
# 
# # apply score according rules
# ippa_f <- ippa[, c(1,2, 3:27)]
# ippa_m <- ippa[, c(1,2, 28:52)]
# ippa_p <- ippa[, c(1,2, 53:77)]
# 
# name <- seq(1, 25)
# 
# ippa_f[1,3:27] <- name
# ippa_m[1,3:27] <- name
# ippa_p[1,3:27] <- name
# 
# colnames(ippa_f) <- ippa_f[1,]
# ippa_f <- ippa_f[-1,]
# colnames(ippa_m) <- ippa_m[1,]
# ippa_m <- ippa_m[-1,]
# colnames(ippa_p) <- ippa_p[1,]
# ippa_p <- ippa_p[-1,]
# 
# # 反向计分题号
# index_f <- c(3,9,6,14,8,10,11,17,18,23)
# index_m <- c(3,9,6,14,8,10,11,17,18,23)
# index_p <- c(5,4,9,10,11,18,22,23)
# 
# for (i in 1:length(index_f)) {
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 1] <- 6")))
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 2] <- 7")))
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 4] <- 2")))
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 5] <- 1")))
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 6] <- 5")))
#   eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 7] <- 4")))
# }
# 
# for (i in 1:length(index_m)) {
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 1] <- 6")))
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 2] <- 7")))
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 4] <- 2")))
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 5] <- 1")))
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 6] <- 5")))
#   eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 7] <- 4")))
# }
# 
# for (i in 1:length(index_p)) {
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 1] <- 6")))
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 2] <- 7")))
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 4] <- 2")))
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 5] <- 1")))
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 6] <- 5")))
#   eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 7] <- 4")))
# }
# 
# # subset subscales
# # father
# cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
# trust_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]
# 
# cols_remain<- c("5","6","7","14","15","16","19","24","25")
# commu_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]
# 
# cols_remain<- c("8","10","11","17","18","23")
# alien_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]
# 
# # mather
# cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
# trust_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]
# 
# cols_remain<- c("5","6","7","14","15","16","19","24","25")
# commu_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]
# 
# cols_remain<- c("8","10","11","17","18","23")
# alien_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]
# 
# # peer
# cols_remain<- c("5","6","8","12","13","14","15","19","20","21")
# trust_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]
# 
# cols_remain<- c("1","2","3","7","16","17","24","25")
# commu_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]
# 
# cols_remain<- c("4","9","10","11","18","22","23")
# alien_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]
# 
# # calculate score
# 
# Score <- data.frame(matrix(ncol = 9, nrow = 0))
# 
# for (i in 1:nrow(ippa_f)) {
#   #father
#   score <- as.numeric(trust_f[i,])
#   Score[i, 1] <- sum(score)
#   
#   score <- as.numeric(commu_f[i,])
#   Score[i, 2] <- sum(score)
#   
#   score <- as.numeric(alien_f[i,])
#   Score[i, 3] <- sum(score)
# 
#   #mother
#   score <- as.numeric(trust_m[i,])
#   Score[i, 4] <- sum(score)
#   
#   score <- as.numeric(commu_m[i,])
#   Score[i, 5] <- sum(score)
#   
#   score <- as.numeric(alien_m[i,])
#   Score[i, 6] <- sum(score)
#   
#   #peer
#   score <- as.numeric(trust_p[i,])
#   Score[i, 7] <- sum(score)
#   
#   score <- as.numeric(commu_p[i,])
#   Score[i, 8] <- sum(score)
#   
#   score <- as.numeric(alien_p[i,])
#   Score[i, 9] <- sum(score)
# }
# 
# IPPA <- cbind(ippa_f$Participant, ippa_f$Session, Score)
# colnames(IPPA) <- c("Participant", "Session","Father_Trust", "Father_Communication",
#                     "Father_Alienation", "Mother_Trust", "Mother_Communication",
#                     "Mother_Alienation", "Peer_Trust", "Peer_Communication", "Peer_Alienation")
# 
# 
# # 计算三个量表的量表分
# for (i in 1:nrow(IPPA)) {
#   IPPA$Father[i] <- sum(as.numeric(IPPA[i, 3:5]))
#   IPPA$Mother[i] <- sum(as.numeric(IPPA[i, 6:8]))
#   IPPA$Peer[i] <- sum(as.numeric(IPPA[i, 9:11]))
# }
# 
# # 去掉没有做这个问卷的被试
# IPPA[is.na(IPPA)]=0
# 
# for (i in 1:nrow(IPPA)) {
#   IPPA$Total[i] <- sum(as.numeric(IPPA[i, 12:14]))
# }
# 
# IPPA <- subset(IPPA, Total != 0)
# IPPA <- IPPA[, -15]
# IPPA[IPPA == 0] = NA 
# 
# # save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(IPPA, "CCNPPEK_IPPA.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/IPPA"))
# for (i in 1:nrow(IPPA)) {
#   data <- IPPA[i,]
#   dataname <- paste0("CCNPPEK", IPPA[i, 1], "_ses-", IPPA[i, 2], "_task-IPPA_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }
