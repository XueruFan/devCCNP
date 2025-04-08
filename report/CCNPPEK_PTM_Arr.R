# this script is used to arrange CCNP EPQ result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/scale'
resultDir <- '//172.16.191.42/home/项目管理/CCNP/报告制作/Batch4'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

ptm <- read.xlsx(file.path(dataDir, "CCNPPEK_PTM_Batch1234.xlsx"), rowNames = F, colNames = T)

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

ptm$Public <- scales::percent(ptm$Public/(4*5), 0.01)
ptm$Anonymous <- scales::percent(ptm$Anonymous/(5*5), 0.01)
ptm$Altruism <- scales::percent(ptm$Altruism/(4*5), 0.01)
ptm$Compliant <- scales::percent(ptm$Compliant/(5*5), 0.01)
ptm$Emotional <- scales::percent(ptm$Emotional/(5*5), 0.01)
ptm$Dire <- scales::percent(ptm$Dire/(3*5), 0.01)

# save result for making report
write.xlsx(ptm[,1:8], file.path(resultDir, "CCNPPEK_PTM_report.xlsx"), rowNames = F, colNames = T)
