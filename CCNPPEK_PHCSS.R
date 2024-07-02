# this code is used to arrange CCNPPEK PHCSS result into tsv file
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

######################## Arrange PHCSS result ######################################################

phcss <- raw[, c(1,2, 259:338)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(phcss, "CCNPPEK_PHCSS_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules

phcss[1,3:82] <- c(seq(1,80))
colnames(phcss) <- phcss[1,]
phcss <- phcss[-1,]
# 是1，否2，需要将所有的2变为0
phcss[phcss == "2"] <- 0
phcss <- phcss[complete.cases(phcss),] # 去掉缺这个数据的被试信息
# 反向计分题号
index <- c(1,3,4,6,7,8,10,11,13,14,18,20,22,25,26,28,31,32,34,37,38,40,43,45,46,47,48,50,53,56,58,59,
          61,62,64,65,66,68,71,74,75,77,78,79)

for (i in 1:length(index)) {
  eval(parse(text = paste0("phcss$'", index[i], "'[phcss$'", index[i], "'== 0] <- 2")))
  eval(parse(text = paste0("phcss$'", index[i], "'[phcss$'", index[i], "'== 1] <- 0")))
  eval(parse(text = paste0("phcss$'", index[i], "'[phcss$'", index[i], "'== 2] <- 1")))
}

# calculate score

Score <- data.frame(matrix(ncol = 7, nrow = 0))

for (i in 1:nrow(phcss)) {
  score <- as.numeric(phcss[i,3:82])
  Score[i, 1] <- sum(score[c(12,13,14,21,22,25,34,35,38,45,48,56,59,62,78,80)])
  Score[i, 2] <- sum(score[c(5,7,9,12,16,17,21,26,27,30,31,33,42,49,53,66,70)])
  Score[i, 3] <- sum(score[c(5,8,15,29,33,41,49,54,57,60,63,69,73)])
  Score[i, 4] <- sum(score[c(4,6,7,8,10,20,28,37,39,40,43,50,74,79)])
  Score[i, 5] <- sum(score[c(1,3,6,11,40,46,49,51,58,65,69,77)])
  Score[i, 6] <- sum(score[c(2,8,36,39,43,50,52,60,67,80)])
  Score[i, 7] <- sum(score[c(1:80)])
}

phcss <- cbind(phcss, Score)
colnames(phcss)[83:89] <- c("Behavior_Problems", "Academic_Competence", "Physical_Appearance",
                            "Anxiety", "Popularity", "Happiness", "Total_Score")

# save finial result of this scale
phcss <- phcss[, c(1,2,83:89)]
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(phcss, "CCNPPEK_PHCSS.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/PHCSS"))
for (i in 1:nrow(phcss)) {
  data <- phcss[i,]
  dataname <- paste0("CCNPPEK", phcss[i, 1], "_ses-", phcss[i, 2], "_task-PHCSS_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
