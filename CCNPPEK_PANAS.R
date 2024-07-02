# this code is used to arrange CCNPPEK PANAS result into tsv file
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

######################## Arrange PANAS result ######################################################

panas <- raw[, c(1,2, 416:435)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(panas, "CCNPPEK_PANAS_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
panas[1,3:22] <- c(seq(1,20))
colnames(panas) <- panas[1,]
panas <- panas[-1,]

panas <- panas[complete.cases(panas),] # 去掉缺这个数据的被试信息

# calculate score

Score <- data.frame(matrix(ncol = 2, nrow = 0))

for (i in 1:nrow(panas)) {
  score <- as.numeric(panas[i,3:22])
  Score[i, 1] <- sum(score[c(1,3,5,9,10,12,14,16,17,19)])
  Score[i, 2] <- sum(score[c(2,4,6,7,8,11,13,15,18,20)])
}

panas <- cbind(panas, Score)
colnames(panas)[23:24] <- c("Positive_Affect", "Negative_Affect")

# save finial result of this scale
panas <- panas[, c(1,2,23:24)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(panas, "CCNPPEK_PANAS.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/PANAS"))
for (i in 1:nrow(panas)) {
  data <- panas[i,]
  dataname <- paste0("CCNPPEK", panas[i, 1], "_ses-", panas[i, 2], "_task-PANAS_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
