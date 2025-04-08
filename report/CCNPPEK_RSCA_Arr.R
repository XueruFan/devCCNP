# this script is used to arrange CCNP RSCA(青少年心理韧性 page 6-3) result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/scale'
resultDir <- '//172.16.191.42/home/项目管理/CCNP/报告制作/Batch4'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

rsca <- read.xlsx(file.path(dataDir, "CCNPPEK_RSCA_Batch1234.xlsx"), rowNames = F, colNames = T)

rsca$Self <- rsca$Goal_planning + rsca$Affect_control + rsca$Positive_thinking
rsca$Support <- rsca$Family_support + rsca$Help_seeking

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

rsca$Goal_planning <- scales::percent(rsca$Goal_planning/(5*5), 0.01)
rsca$Affect_control <- scales::percent(rsca$Affect_control/(6*5), 0.01)
rsca$Positive_thinking <- scales::percent(rsca$Positive_thinking/(4*5), 0.01)
rsca$Family_support <- scales::percent(rsca$Family_support/(6*5), 0.01)
rsca$Help_seeking <- scales::percent(rsca$Help_seeking/(6*5), 0.01)
rsca$Self <- scales::percent(rsca$Self/(15*5), 0.01)
rsca$Support <- scales::percent(rsca$Support/(12*5), 0.01)

# save result for making report
write.xlsx(rsca, file.path(resultDir, "CCNPPEK_RSCA_report.xlsx"), rowNames = F, colNames = T)
