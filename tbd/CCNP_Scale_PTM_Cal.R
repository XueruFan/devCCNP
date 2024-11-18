# this script is used to arrange CCNP PTM(亲社会行为倾向 page 6-2) result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'
resultDir <- '/Users/xuerufan/Desktop/流水线制作材料/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_All.xlsx", rowNames = F)

## pick up PTM part
ptm <- all[, c(3, 4, which(names(all)=="P5Q1S01"):which(names(all)=="P5Q1S26"))]

# # define where to save raw data file
# setwd(filefolder)
# 
# # save a copy of raw data
# write.xlsx(ptm, "CCNPPEK_PTM_raw.xlsx", rowNames = F, colNames = F)

# 计分方式参考文献详见雪如笔记“亲社会行为倾向算分规则.docx”

# apply score according rules
ptm[1, 3:28] <- c(seq(1, 26))
colnames(ptm) <- ptm[1,]
ptm <- ptm[-1,]

# 去掉缺这个数据的被试信息
ptm <- ptm[complete.cases(ptm),] 

# calculate score
Score <- data.frame(matrix(ncol = 7, nrow = 0))

for (i in 1:nrow(ptm)) {
  score <- as.numeric(ptm[i, 3:28])
  Score[i, 1] <- sum(score[c(1,4,6,14)])
  Score[i, 2] <- sum(score[c(8,12,16,20,22)])
  Score[i, 3] <- sum(score[c(10,17,23,25)])
  Score[i, 4] <- sum(score[c(3,7,11,19,24)])
  Score[i, 5] <- sum(score[c(2,13,18,21,26)])
  Score[i, 6] <- sum(score[c(5,9,15)])
  Score[i, 7] <- sum(score[c(1:26)])
}

ptm <- cbind(ptm, Score)
colnames(ptm)[29:35] <- c("Public", "Anonymous", "Altruism", "Compliant", "Emotional", "Dire",
                          "Total_Score")

# # save finial result of this scale
# ptm <- ptm[, c(1,2,29:35)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(ptm, "CCNPPEK_PTM.xlsx", rowNames = F, colNames = T)

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

ptm$Public <- scales::percent(ptm$Public/(4*5), 0.01)
ptm$Anonymous <- scales::percent(ptm$Anonymous/(5*5), 0.01)
ptm$Altruism <- scales::percent(ptm$Altruism/(4*5), 0.01)
ptm$Compliant <- scales::percent(ptm$Compliant/(5*5), 0.01)
ptm$Emotional <- scales::percent(ptm$Emotional/(5*5), 0.01)
ptm$Dire <- scales::percent(ptm$Dire/(3*5), 0.01)

# save result for making report
write.xlsx(ptm[, c(1,2,29:34)], file.path(resultDir, "CCNPPEK_PTM_report.xlsx"), rowNames = F,
           colNames = T)