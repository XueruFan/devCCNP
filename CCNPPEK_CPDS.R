# this code is used to arrange CCNPPEK C-PDS result into tsv file
# copyright: Xue-Ru Fan @BNU, 27 Dec 2022

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
setwd(paste0(filefolder, "/RawData/"))
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

######################## Arrange C-PDS result ######################################################

cpds <- raw[, c(1,2, 228:240)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(cpds, "CCNPPEK_C-PDS_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
cpds$P0Q5B1[(cpds$P0Q5B1) == "2"] <- 4
cpds[is.na(cpds)] <- 0

# calculate developmental score and apply stage
Score <- data.frame(matrix(ncol = 3, nrow = 0))
`%!in%` <- Negate(`%in%`) # define function "without"
for (i in 2:nrow(cpds)) {
  score <- as.numeric(cpds[i,3:12])
  score[11] <- sum(score[c(2,4,5)])
  Score[i, 1] <- score[11]
  score[12] <- sum(score[c(7,9,10)])
  Score[i, 2] <- score[12]
  # if boy
  if (score[11] == 3) {
    Score[i, 3] = 'Prepubertal'
  } else if (score[11] == 4 | score[11] == 5) {
    Score[i, 3] = 'Early Puberty'
  } else if (score[11] <= 8 & score[11] >= 6) {
    Score[i, 3] = 'Midpubertal'
  } else if (score[11] <= 11 & score[11] >= 9) {
    Score[i, 3] = 'Late Puberty'
  } else if (score[11] == 12) {
    Score[i, 3] = 'Postpubertal'
  }
  # if girl
  if (score[12] == 3) {
    Score[i, 3] = 'Prepubertal'
  } else if (score[12] == 4 & score[10] == 1) {
    Score[i, 3] = 'Early Puberty'
  } else if (score[12] >= 5 & score[10] == 1) {
    Score[i, 3] = 'Midpubertal'
  } else if (score[12] <= 11 & score[10] == 4) {
    Score[i, 3] = 'Late Puberty'
  } else if (score[12] == 12) {
    Score[i, 3] = 'Postpubertal'
  }
}
cpds <- cbind(cpds, Score)
cpds[1, 16:18] <- c("Boy", "Gril", "Development Stage")
colnames(cpds) <- cpds[1,]
cpds <- cpds[-1,]

cpds$Boy[(cpds$Boy) == 0] <- NA
cpds$Gril[(cpds$Gril) == 0] <- NA

cpds <- cpds[, c(1,2,18)]
cpds <- cpds[complete.cases(cpds),] # 去掉缺这个数据的被试信息

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(cpds, "CCNPPEK_C-PDS.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/CPDS"))
for (i in 1:nrow(cpds)) {
  data <- cpds[i,]
  dataname <- paste0("CCNPPEK", cpds[i, 1], "_ses-", cpds[i, 2], "_task-CPDS_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}