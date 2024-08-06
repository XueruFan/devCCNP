# this code is used to arrange CCNPCKG CDIC result into csv file
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
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/"
setwd(filefolder)

#################### arrange raw data ##############################################################

# load id and name
name <- file.path("souce", "devCCNPCKG_id_code.csv")
idname <- read.csv(name)

# wave 1
# load raw data file
rawfile <-  file.path("souce/devCCNPCKG_CDIC", "CCNPCKG_CDIC_fromQJ_wave1.xlsx")
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, c(-1, -3:-5)]
colnames(rawdata)[1] <- "Name"
wave1 <- merge(idname, rawdata, by = "Name", all.x = TRUE)
wave1 <- wave1[!apply(wave1[, 4:30], 1, function(row) all(is.na(row))), ]
wave1$Session <- "01"
wave1 <- wave1[, -1:-2]
wave1$Participant <- str_pad(wave1$Participant, 4, side = "left", "0")
colnames(wave1)[2:28] <- c(1:27)
wave1 <- wave1[, c(1, 29, 2:28)]

# wave 2
rawfile <-  file.path("souce/devCCNPCKG_CDIC", "CCNPCKG_CDIC_fromQJ_wave2.xlsx")
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, c(-2:-6)]
colnames(rawdata)[1] <- "Name"
wave2 <- merge(idname, rawdata, by = "Name", all.x = TRUE)
wave2 <- wave2[!apply(wave2[, 4:30], 1, function(row) all(is.na(row))), ]
wave2$Session <- "02"
wave2 <- wave2[, -1:-2]
wave2$Participant <- str_pad(wave2$Participant, 4, side = "left", "0")
colnames(wave2)[2:28] <- c(1:27)
wave2 <- wave2[, c(1, 29, 2:28)]

# wave 3
rawfile <-  file.path("souce/devCCNPCKG_CDIC", "CCNPCKG_CDIC_fromQJ_wave3.xlsx")
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, c(-2:-5)]
colnames(rawdata)[1] <- "Name"
wave3 <- merge(idname, rawdata, by = "Name", all.x = TRUE)
wave3 <- wave3[!apply(wave3[, 4:30], 1, function(row) all(is.na(row))), ]
wave3$Session <- "03"
wave3 <- wave3[, -1:-2]
wave3$Participant <- str_pad(wave3$Participant, 4, side = "left", "0")
colnames(wave3)[2:28] <- c(1:27)
wave3 <- wave3[, c(1, 29, 2:28)]


cdic <- rbind(wave1, wave2, wave3)
cdic[, 3:29] <- apply(cdic[, 3:29], 2, function(x) ifelse(x %in% c(1, 2, 3), x, NA))
# 按照 Participant 列和 Session 列排序
cdic <- cdic[order(cdic$Participant, cdic$Session), ]

# save a copy of raw data
write.xlsx(cdic, file.path("raw", "devCCNPCKG_CDIC_raw.xlsx"), rowNames = F)


################################## apply score according rules #####################################
# 按照选项顺序记0、1、2分
cdic[cdic == "1"] <- 0
cdic[cdic == "2"] <- 1
cdic[cdic == "3"] <- 2
cdic <- cdic[complete.cases(cdic),] # 去掉缺这个数据的被试信息

# 反向计分题号
index <- c(2,5,7,8,10,11,13,15,16,18,21,24,25)

for (i in 1:length(index)) {
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 0] <- 3")))
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 2] <- 0")))
  eval(parse(text = paste0("cdic$'", index[i], "'[cdic$'", index[i], "'== 3] <- 2")))
}

# calculate score

Score <- data.frame(matrix(ncol = 6, nrow = 0))

for (i in 1:nrow(cdic)) {
  score <- as.numeric(cdic[i, 3:29])
  Score[i, 1] <- sum(score[c(4, 16, 17, 18, 19, 20, 21, 22)])
  Score[i, 2] <- sum(score[c(1, 6, 8, 9, 10, 11, 13)])
  Score[i, 3] <- sum(score[c(2, 7, 14, 25)])
  Score[i, 4] <- sum(score[c(3, 15, 23, 24)])
  Score[i, 5] <- sum(score[c(5, 12, 26, 27)])
  Score[i, 6] <- sum(score[c(1:27)])
}

cdic <- cbind(cdic, Score)
colnames(cdic)[30:35] <- c("Anhedonia", "Negative_Mood", "Negative_Self-Esteem",
                           "Ineffectiveness", "Interpersonal_Problem", "Total_Score")

# save finial result of this scale
cdic <- cdic[, c(1,2,30:35)]
write.xlsx(cdic, file.path("scale", "devCCNPCKG_CDIC.xlsx"), rowNames = F)
