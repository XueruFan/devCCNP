# this code is used to arrange CCNPPEK CBCL score result for report
# copyright: Xue-Ru Fan @BNU, 10 Feb 2023
rm(list=ls())

dataDir <- "C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入/自处理数据/"

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

score <- read.xlsx("CCNPPEK_CBCL_Score_All.xlsx", rowNames = F)

eid <- score$EvaluationId

score_select <- score[, -c(1:which(names(score)=="EnterProblemItemsOnly"))]
score_select <- score_select[, -c(2:which(names(score_select)=="ScoreThisForm"))]

# 读入IDcode
idcode <- read.csv("保存备用_CBCL_IDcode.csv")
idcode$EvaluationId <- paste0("T", idcode$subjectno)
score_select <- merge(idcode[, c("FID", "EvaluationId")], score_select, by = "EvaluationId", all.y = T)

score_select$Participant <- str_pad(substr(score_select$FID, 1, 3), 3, side = "left", "0")
score_select$Session <- substr(score_select$FID, 6, 6)


score_select <- score_select[, c(which(names(score_select)=="Participant"),
                                 which(names(score_select)=="Session"),
                                 which(names(score_select)=="FID"),
                                 (which(names(score_select)=="FID")+1):(ncol(score_select)-2))]
#### 手动检查一边编号啥的有没有问题
score_select <- score_select[, -c(which(names(score_select)=="FID"))]

write.csv(score_select, "CCNPPEK_CBCL_Score_report.csv", row.names = F)
