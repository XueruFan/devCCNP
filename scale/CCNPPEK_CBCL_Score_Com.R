# this code is used to combine all CCNPPEK CBCL score result for report
# copyright: Xue-Ru Fan @BNU, 10 Feb 2023
rm(list=ls())

filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据电子化/自处理数据/CBCL"
setwd(filefolder)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

################ 以下这部分是处理Batch1234的代码 ######################################################
score123 <- read.csv("CCNPPEK_CBCL_Score_Report_Batch123.csv")

score4 <- read.csv("CCNPPEK_CBCL_Score_Report_Batch4.csv")
score4 <- score4[, names(score123)]

# 按照上一次的列名进行合并
score <- rbind(score123, score4)

score$Participant <- str_pad(score$Participant, 4, side = "left", "0")
score$Session <- str_pad(score$Session, 2, side = "left", "0")

write.xlsx(score, "CCNPPEK_CBCL_Score_Report_Batch1234.xlsx", rowNames = F)


################################### 以下是处理Batch123的代码 #######################################

# score3 <- read.xlsx("CCNPPEK_CBCL_Raw_and_Score_batch3_1.xlsx", rowNames = F)
# score4 <- read.xlsx("CCNPPEK_CBCL_Raw_and_Score_batch3_2.xlsx", rowNames = F)
# score <- rbind.fill(score3, score4)
# 
# eid <- score$EvaluationId
# 
# score_select <- score[, -c(1:which(names(score)=="EnterProblemItemsOnly"))]
# score_select <- score_select[, -c(2:which(names(score_select)=="ScoreThisForm"))]
# 
# # 读入IDcode
# idcode <- read.csv("保存备用_CBCL_IDcode.csv")
# idcode$EvaluationId <- paste0("T", idcode$subjectno)
# score_select <- merge(idcode[, c("FID", "EvaluationId")], score_select, by = "EvaluationId", all.y = T)
# 
# score_select$Participant <- str_pad(substr(score_select$FID, 1, 3), 4, side = "left", "0")
# score_select$Session <- str_pad(substr(score_select$FID, 6, 6), 2, side = "left", "0")
# 
# 
# score_select <- score_select[, c(which(names(score_select)=="Participant"),
#                                  which(names(score_select)=="Session"),
#                                  which(names(score_select)=="FID"),
#                                  (which(names(score_select)=="FID")+1):(ncol(score_select)-2))]
# #### 手动检查一边编号啥的有没有问题
# score_select <- score_select[, -c(which(names(score_select)=="FID"))]
# 
# 
# # 读入之前的数据
# score_before <- read.xlsx("CCNPPEK_CBCL_Score_batch1nd2.xlsx", rowNames = F)
# 
# col_name <- names(score_before)
# score_now <- score_select[, col_name]
# 
# score_all <- rbind(score_before, score_now)
# 
# write.csv(score_all, "CCNPPEK_CBCL_Score_Report_Batch123.csv", row.names = F)
