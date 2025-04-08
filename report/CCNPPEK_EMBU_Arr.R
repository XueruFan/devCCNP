# this script is used to arrange CCNP EMBU(家庭教养方式 page 6-4) result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/scale'
resultDir <- '//172.16.191.42/home/项目管理/CCNP/报告制作/Batch4'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

EMBU <- read.xlsx(file.path(dataDir, "CCNPPEK_EMBU_Batch1234.xlsx"), rowNames = F, colNames = T)


# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

EMBU$Paternal_Emotional_warmth_and_understanding <- scales::percent(
  EMBU$Paternal_Emotional_warmth_and_understanding/(19*4), 0.01)
EMBU$Paternal_Punishment_and_strictness <- scales::percent(
  EMBU$Paternal_Punishment_and_strictness/(12*4), 0.01)
EMBU$`Paternal_Over-interference` <- scales::percent(
  EMBU$`Paternal_Over-interference`/(10*4), 0.01)
EMBU$Paternal_Favoring_subjects <- scales::percent(
  EMBU$Paternal_Favoring_subjects/(5*4), 0.01)
EMBU$Paternal_Refusal_and_denial <- scales::percent(
  EMBU$Paternal_Refusal_and_denial/(6*4), 0.01)
EMBU$`Paternal_Over-protection` <- scales::percent(
  EMBU$`Paternal_Over-protection`/(5*4), 0.01)

EMBU$Maternal_Emotional_warmth_and_understanding <- scales::percent(
  EMBU$Maternal_Emotional_warmth_and_understanding/(19*4), 0.01)
EMBU$Maternal_Punishment_and_strictness <- scales::percent(
  EMBU$Maternal_Punishment_and_strictness/(9*4), 0.01)
EMBU$`Maternal_Over-interferenceand_and_over-protection` <- scales::percent(
  EMBU$`Maternal_Over-interferenceand_and_over-protection`/(16*4), 0.01)
EMBU$Maternal_Favoring_subjects <- scales::percent(
  EMBU$Maternal_Favoring_subjects/(5*4), 0.01)
EMBU$Maternal_Refusal_and_denial <- scales::percent(
  EMBU$Maternal_Refusal_and_denial/(8*4), 0.01)

# save result for making report
write.xlsx(EMBU, file.path(resultDir, "CCNPPEK_EMBU_report.xlsx"), rowNames = F, colNames = T)
