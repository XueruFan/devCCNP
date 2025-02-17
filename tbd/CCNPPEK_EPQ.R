# this script is used to arrange CCNP EPQ result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'
resultDir <- '/Users/xuerufan/Desktop/流水线制作材料/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_All.xlsx", rowNames = F)

epqy <- all[, c(2:4, which(names(all)=="P3Q1S01"):which(names(all)=="P3Q1S88"))]
epqy <- epqy[-1, -c(which(names(epqy)=="Participant"), which(names(epqy)=="Session"))]
colnames(epqy)[2:ncol(epqy)] <- seq(1:(ncol(epqy)-1))

# get basic info
basic <- read.csv(file.path("自处理数据", "CCNPPEK被试信息表.csv"))
basic$FID <- paste0(str_pad(basic$编号, 3, side = "left", "0"), "-W", basic$wave)
basic <- basic[, c("FID", "男1女0", "年龄", "wave")]
colnames(basic)[2:4] <- c("Sex", "Age", "session")
basic$Sex <- gsub("1", "M", basic$Sex)
basic$Sex <- gsub("0", "F", basic$Sex)

epqy <- merge(basic, epqy, by = "FID", all.y = TRUE)
epqy$Age <- floor(epqy$Age)

write.xlsx(epqy, file.path(resultDir, "CCNPPEK_EPQY_report.xlsx"), rowNames = F, colNames = T)