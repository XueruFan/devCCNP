# this script is used to arrange CCNP EPQ result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/source'
resultDir <- '//172.16.191.42/home/项目管理/CCNP/报告制作/Batch4'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_Batch1234.xlsx", rowNames = F)

colnames(all) <- all[1,]
epqy <- all[, c(2:4, which(names(all)=="P3Q1S01"):which(names(all)=="P3Q1S88"))]
epqy <- epqy[-1, -c(which(names(epqy)=="Participant"), which(names(epqy)=="Session"))]
colnames(epqy)[2:ncol(epqy)] <- seq(1:(ncol(epqy)-1))

# get basic info
basic <- read.csv("CCNPPEK被试信息表_Batch1234.csv")
basic$FID <- paste0(str_pad(basic$Participant, 3, side = "left", "0"), "-W", basic$Session)
basic <- basic[, c("FID", "Sex", "Age", "Session")]
colnames(basic)[4] <- "session"
basic$Sex <- gsub("1", "M", basic$Sex)
basic$Sex <- gsub("0", "F", basic$Sex)

epqy <- merge(basic, epqy, by = "FID", all.y = TRUE)
epqy$Age <- floor(epqy$Age)

write.xlsx(epqy, file.path(resultDir, "CCNPPEK_EPQY_report.xlsx"), rowNames = F, colNames = T)
