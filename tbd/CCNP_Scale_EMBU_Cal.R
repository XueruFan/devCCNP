# this script is used to arrange CCNP EMBU(家庭教养方式 page 6-4) result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'
resultDir <- '/Users/xuerufan/Desktop/流水线制作材料/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_All.xlsx", rowNames = F)

embu <- all[, c(3,4, which(names(all)=="P5Q2A"):which(names(all)=="P5Q2S66B"))]

# # define where to save raw data file
# setwd(paste0(filefolder, "/RawData/"))
# 
# # save a copy of raw data
# write.xlsx(embu, "CCNPPEK_EMBU_raw.xlsx", rowNames = F, colNames = F)

# 计分方式参考文献详见雪如笔记“家庭教养方式算分规则.docx”

# apply score according rules
embu_f <- embu[, c(1,2, seq(5, 135, by = 2))]
embu_m <- embu[, c(1,2, seq(6, 136, by = 2))]

name <- seq(1, 66)

embu_f[1, 3:68] <- name
embu_m[1, 3:68] <- name

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

## father
# 情感温暖
cols_remain<- c("2","4","6","7","9","15","20","25","29","30","31","32","33","37","42","44","60",
                 "61","66")
ew_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 惩罚严厉
cols_remain<- c("5","13","17","18","43","49","51","52","53","55","58","62")
ps_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 过分干涉
cols_remain<- c("1","10","11","14","27","36","48","50","56","57")
oi_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 偏爱
cols_remain<- c("3","8","22","64","65")
fs_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 拒绝否认
cols_remain<- c("21","23","28","34","35","45")
rd_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 过度保护
cols_remain<- c("12","16","39","40","59")
op_f <- embu_f[ ,colnames(embu_f) %in% cols_remain]

## mather
# 情感温暖
cols_remain<- c("2","4","6","7","9","15","25","29","30","31","32","33","37","42","44","54","60",
                "61","63")
ew_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 惩罚严厉
cols_remain<- c("13","17","43","51","52","53","55","58","62")
ps_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 过分干涉保护
cols_remain<- c("1","11","12","14","16","19","24","27","35","36","41","48",
                "50","56","57","59")
oi_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 偏爱
cols_remain<- c("3","8","22","64","65")
fs_m <- embu_f[ ,colnames(embu_f) %in% cols_remain]
# 拒绝否认
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

# # save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(EMBU, "CCNPPEK_EMBU.xlsx", rowNames = F, colNames = T)

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

EMBU$Paternal_Emotional_warmth_and_understanding <- scales::percent(
  EMBU$Paternal_Emotional_warmth_and_understanding/(ncol(ew_f)*4), 0.01)
EMBU$Paternal_Punishment_and_strictness <- scales::percent(
  EMBU$Paternal_Punishment_and_strictness/(ncol(ps_f)*4), 0.01)
EMBU$`Paternal_Over-interference` <- scales::percent(
  EMBU$`Paternal_Over-interference`/(ncol(oi_f)*4), 0.01)
EMBU$Paternal_Favoring_subjects <- scales::percent(
  EMBU$Paternal_Favoring_subjects/(ncol(fs_f)*4), 0.01)
EMBU$Paternal_Refusal_and_denial <- scales::percent(
  EMBU$Paternal_Refusal_and_denial/(ncol(rd_f)*4), 0.01)
EMBU$`Paternal_Over-protection` <- scales::percent(
  EMBU$`Paternal_Over-protection`/(ncol(op_f)*4), 0.01)

EMBU$Maternal_Emotional_warmth_and_understanding <- scales::percent(
  EMBU$Maternal_Emotional_warmth_and_understanding/(ncol(ew_m)*4), 0.01)
EMBU$Maternal_Punishment_and_strictness <- scales::percent(
  EMBU$Maternal_Punishment_and_strictness/(ncol(ps_m)*4), 0.01)
EMBU$`Maternal_Over-interferenceand_and_over-protection` <- scales::percent(
  EMBU$`Maternal_Over-interferenceand_and_over-protection`/(ncol(oi_m)*4), 0.01)
EMBU$Maternal_Favoring_subjects <- scales::percent(
  EMBU$Maternal_Favoring_subjects/(ncol(fs_m)*4), 0.01)
EMBU$Maternal_Refusal_and_denial <- scales::percent(
  EMBU$Maternal_Refusal_and_denial/(ncol(rd_m)*4), 0.01)

# save result for making report
write.xlsx(EMBU, file.path(resultDir, "CCNPPEK_EMBU_report.xlsx"), rowNames = F, colNames = T)