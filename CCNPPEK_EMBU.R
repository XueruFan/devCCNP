# this code is used to arrange CCNPPEK EMBU result into tsv file
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

######################## Arrange EMBU result ######################################################

embu <- raw[, c(1,2, 670:803)]
setwd(paste0(filefolder, "/RawData/"))

# save a copy of raw data
write.xlsx(embu, "CCNPPEK_EMBU_raw.xlsx", rowNames = F, colNames = F)

# apply score according rules
embu_f <- embu[, c(1,2, seq(5, 135, by = 2))]
embu_m <- embu[, c(1,2, seq(6, 136, by = 2))]

name <- seq(1, 66)

embu_f[1,3:68] <- name
embu_m[1,3:68] <- name

colnames(embu_f) <- embu_f[1,]
embu_f <- embu_f[-1,]
colnames(embu_m) <- embu_m[1,]
embu_m <- embu_m[-1,]

# 反向计分题号
index <- c(20, 50, 56)

for (i in 1:length(index)) {
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 1] <- 5")))
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 2] <- 6")))
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 3] <- 2")))
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 4] <- 1")))
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 5] <- 4")))
  eval(parse(text = paste0("embu_f$'", index[i], "'[embu_f$'", index[i], "'== 6] <- 3")))
  
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 1] <- 5")))
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 2] <- 6")))
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 3] <- 2")))
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 4] <- 1")))
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 5] <- 4")))
  eval(parse(text = paste0("embu_m$'", index[i], "'[embu_m$'", index[i], "'== 6] <- 3")))
}

# subset subscales
# father
cols_remain<- c("2","4","6","7","9","15","20","25","29","30","31","32","33","37","42","44","60",
                 "61","66")
ew_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("5","13","17","18","43","49","51","52","53","55","58","62")
ps_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("1","10","11","14","27","36","48","50","56","57")
oi_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("3","8","22","64","65")
fs_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("21","23","28","34","35","45")
rd_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("12","16","39","40","59")
op_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

# mather
cols_remain<- c("2","4","6","7","9","15","25","29","30","31","32","33",
                "37","42","44","54","60","61","63")
ew_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("13","17","43","51","52","53","55","58","62")
ps_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("1","11","12","14","16","19","24","27","35","36","41","48",
                "50","56","57","59")
oi_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("3","8","22","64","65")
fs_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]

cols_remain<- c("23","26","28","34","38","39","45","47")
rd_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]

# calculate score

Score <- data.frame(matrix(ncol = 11, nrow = 0))

for (i in 1:nrow(embu_f)) {
  #father
  score <- as.numeric(ew_f[i,])
  Score[i, 1] <- sum(score)
  
  score <- as.numeric(ps_f[i,])
  Score[i, 2] <- sum(score)
  
  score <- as.numeric(oi_f[i,])
  Score[i, 3] <- sum(score)
  
  score <- as.numeric(fs_f[i,])
  Score[i, 4] <- sum(score)
  
  score <- as.numeric(rd_f[i,])
  Score[i, 5] <- sum(score)
  
  score <- as.numeric(op_f[i,])
  Score[i, 6] <- sum(score)
  
  #mother
  score <- as.numeric(ew_m[i,])
  Score[i, 7] <- sum(score)
  
  score <- as.numeric(ps_m[i,])
  Score[i, 8] <- sum(score)
  
  score <- as.numeric(oi_m[i,])
  Score[i, 9] <- sum(score)
  
  score <- as.numeric(fs_m[i,])
  Score[i, 10] <- sum(score)
  
  score <- as.numeric(rd_m[i,])
  Score[i, 11] <- sum(score)
}

EMBU <- cbind(embu_f$Participant, embu_f$Session, Score)
colnames(EMBU) <- c("Participant", "Session","Paternal_Emotional_warmth_and_understanding",
                          "Paternal_Punishment_and_strictness", "Paternal_Over-interference",
                          "Paternal_Favoring_subjects", "Paternal_Refusal_and_denial",
                          "Paternal_Over-protection",
                          "Maternal_Emotional_warmth_and_understanding",
                          "Maternal_Punishment_and_strictness",
                          "Maternal_Over-interferenceand_and_over-protection",
                          "Maternal_Favoring_subjects", "Maternal_Refusal_and_denial")

# 去掉没有做这个量表的被试
EMBU[is.na(EMBU)]=0

for (i in 1:nrow(EMBU)) {
  EMBU$Total[i] <- sum(as.numeric(EMBU[i, 3:13]))
}
EMBU <- subset(EMBU, Total != 0)
EMBU <- EMBU[, -14]
EMBU[EMBU == 0] = NA 


# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(EMBU, "CCNPPEK_EMBU.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/EMBU"))
for (i in 1:nrow(EMBU)) {
  data <- EMBU[i,]
  dataname <- paste0("CCNPPEK", EMBU[i, 1], "_ses-", EMBU[i, 2], "_task-EMBU_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
