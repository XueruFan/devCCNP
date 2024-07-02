# this code is used to arrange CCNPPEK MASC result into tsv file
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

######################## Arrange MASC result ######################################################

masc <- raw[, c(1,2, 591:629)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(masc, "CCNPPEK_MASC_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
masc[1,3:41] <- c(seq(1,39))
colnames(masc) <- masc[1,]
masc <- masc[-1,]

masc <- masc[complete.cases(masc),] # 去掉缺这个数据的被试信息

# calculate score

Score <- data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:nrow(masc)) {
  score <- as.numeric(masc[i,3:41])
  Score[i, 1] <- sum(score[c(1,6,8,12,15,18,20,24,27,31,35,38)])
  Score[i, 2] <- sum(score[c(2,5,11,13,21,25,28,32,36)])
  Score[i, 3] <- sum(score[c(3,10,14,16,22,29,33,37,39)])
  Score[i, 4] <- sum(score[c(4,7,9,17,19,23,26,30,34)])
  Score[i, 5] <- sum(score[c(1:39)])
}

masc <- cbind(masc, Score)
colnames(masc)[42:46] <- c("Physical_Symptoms", "Harm_Avoidance", "Social_Anxiety",
                           "Separation_Anxiety", "Total_Score")

# save finial result of this scale
masc <- masc[, c(1,2,42:46)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(masc, "CCNPPEK_MASC.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/MASC"))
for (i in 1:nrow(masc)) {
  data <- masc[i,]
  dataname <- paste0("CCNPPEK", masc[i, 1], "_ses-", masc[i, 2], "_task-MASC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
