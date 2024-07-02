# this code is used to arrange CCNPPEK CPSS result into tsv file
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

######################## Arrange CPSS result ######################################################

cpss <- raw[, c(1,2, 630:643)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(pss, "CCNPPEK_PSS_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
cpss[1,3:16] <- c(seq(1,14))
colnames(cpss) <- cpss[1,]
cpss <- cpss[-1,]

cpss <- cpss[complete.cases(cpss),] # 去掉缺这个数据的被试信息

# 5个选项按照0-4计分
cpss[cpss == "1"] <- 0
cpss[cpss == "2"] <- 1
cpss[cpss == "3"] <- 2
cpss[cpss == "4"] <- 3
cpss[cpss == "5"] <- 4

# 反向计分题号
index <- c(4,5,6,7,9,10,13)

for (i in 1:length(index)) {
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 0] <- 5")))
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 1] <- 6")))
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 3] <- 1")))
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 4] <- 0")))
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 5] <- 4")))
  eval(parse(text = paste0("cpss$'", index[i], "'[cpss$'", index[i], "'== 6] <- 3")))
}

# calculate score

Score <- data.frame(matrix(ncol = 3, nrow = 0))

for (i in 1:nrow(cpss)) {
  score <- as.numeric(cpss[i,3:16])
  Score[i, 1] <- sum(score[c(1,2,3,8,11,12,14)])
  Score[i, 2] <- sum(score[c(4,5,6,7,9,10,13)])
  Score[i, 3] <- sum(score[c(1:14)])
}

cpss <- cbind(cpss, Score)
colnames(cpss)[17:19] <- c("Perceived_Distress", "Perceived_Coping", "Total_Score")

# save finial result of this scale
cpss <- cpss[, c(1,2,17:19)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(cpss, "CCNPPEK_CPSS.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/CPSS"))
for (i in 1:nrow(cpss)) {
  data <- cpss[i,]
  dataname <- paste0("CCNPPEK", cpss[i, 1], "_ses-", cpss[i, 2], "_task-CPSS_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
