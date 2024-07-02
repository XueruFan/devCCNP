# arrange devCCNPCKG SASC results into open-share file
# Xue-Ru Fan @BNU, 20 Feb 2023

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)
library(tidyr)

# define environment variables
filefolder <- "/Volumes/FANXUERU"
setwd(filefolder)

# load raw data file
rawdata <- read.xlsx("devCCNPCKG_SASC_souce.xlsx", rowNames = F)
# load id code
idfile <- "id_code.csv"
idcode <- read.csv(idfile)
raw <- merge(rawdata, idcode, by = "ID", all = T)
# correct id
id_list <- paste0("A12", 1:6)
for (id in id_list) {
  raw[which(raw$ID == id & raw$Session == "02"), "Session"] <- "01"
  raw[which(raw$ID == id & raw$Session == "03"), "Session"] <- "02"
}
id_list <- c("A127", "A128")
for (id in id_list) {
  raw[which(raw$ID == id & raw$Session == "03"), "Session"] <- "01"
}
raw[which(raw$ID == "B079" & raw$Session == "02"), "Session"] <- "01"

sasc <- raw[, c("Participant", "Session", seq(1,10,1))]
sasc$Participant <- str_pad(sasc$Participant, 4, side = "left", "0")
sasc <- sasc %>% arrange(Participant, Session)

# save a copy of raw data
write.xlsx(sasc, "devCCNPPEK_SASC_raw.xlsx", rowNames = F)

# apply score according rules
sasc[sasc == "A"] <- 0
sasc[sasc == "B"] <- 1
sasc[sasc == "C"] <- 2
sasc <- sasc[complete.cases(sasc),] # 去掉缺这个数据的被试信息

# calculate score
Score <- data.frame(matrix(ncol = 3, nrow = 0))

for (i in 1:nrow(sasc)) {
  score <- as.numeric(sasc[i,3:12])
  Score[i, 1] <- sum(score[c(1,2,5,6,8,10)])
  Score[i, 2] <- sum(score[c(3,4,7,9)])
  Score[i, 3] <- sum(score[c(1:10)])
}

sasc <- cbind(sasc, Score)
colnames(sasc)[13:15] <- c("Fear_of_Negative_Evaluation", "Social_Avoidance_and_Distress",
                           "Total_Score")

# save finial result of this scale
sasc <- sasc[, c(1,2,13:15)]
write.xlsx(sasc, "devCCNPCKG_SASC.xlsx", rowNames = F, colNames = T)
