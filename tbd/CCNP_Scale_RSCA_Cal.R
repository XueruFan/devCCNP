# this script is used to arrange CCNP RSCA(青少年心理韧性 page 6-3) result
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'
resultDir <- '/Users/xuerufan/Desktop/流水线制作材料/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_All.xlsx", rowNames = F)

## pick up RSCA part
rsca <- all[, c(3,4, which(names(all)=="P5Q6S01"):which(names(all)=="P5Q6S27"))]

# # define where to save raw data file
# setwd(filefolder)
# 
# # save a copy of raw data
# write.xlsx(rsca, "CCNPPEK_RSCA_raw.xlsx", rowNames = F, colNames = F)

# 计分方式参考文献详见雪如笔记“青少年心理韧性算分规则.docx”

# apply score according rules
rsca[1, 3:29] <- c(seq(1,27))
colnames(rsca) <- rsca[1,]
rsca <- rsca[-1,]

# 反向计分题号
index <- c(1,2,5,6,9,12,15,16,17,21,26,27)

for (i in 1:length(index)) {
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 1] <- 6")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 2] <- 7")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 4] <- 2")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 5] <- 1")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 6] <- 5")))
  eval(parse(text = paste0("rsca$'", index[i], "'[rsca$'", index[i], "'== 7] <- 4")))
}

# calculate score

Score <- data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:nrow(rsca)) {
  score <- as.numeric(rsca[i, 3:29])
  Score[i, 1] <- sum(score[c(3,4,11,20,24)])
  Score[i, 2] <- sum(score[c(1,2,5,21,23,27)])
  Score[i, 3] <- sum(score[c(10,13,14,25)])
  Score[i, 4] <- sum(score[c(8,15,16,17,19,22)])
  Score[i, 5] <- sum(score[c(6,7,9,12,18,26)])
}

rsca <- cbind(rsca, Score)
colnames(rsca)[30:34] <- c("Goal_planning", "Affect_control", "Positive_thinking",
                          "Family_support", "Help_seeking")

# 去掉没有做这个问卷的被试
rsca[is.na(rsca)]=0

for (i in 1:nrow(rsca)) {
  rsca$Total[i] <- sum(as.numeric(rsca[i, 30:34]))
}

rsca <- subset(rsca, Total != 0)
rsca <- rsca[, -35]
rsca[rsca == 0] = NA

rsca$Self <- rsca$Goal_planning + rsca$Affect_control + rsca$Positive_thinking
rsca$Support <- rsca$Family_support + rsca$Help_seeking

# # save finial result of this scale
# rsca <- rsca[, c(1,2,30:34)]
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(rsca, "CCNPPEK_RSCA.xlsx", rowNames = F, colNames = T)


## save each participant's data into tsv file
# setwd(paste0(filefolder, "/DataArrange/RSCA"))
# for (i in 1:nrow(rsca)) {
#   data <- rsca[i,]
#   dataname <- paste0("CCNPPEK", rsca[i, 1], "_ses-", rsca[i, 2], "_task-RSCA_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

rsca$Goal_planning <- scales::percent(rsca$Goal_planning/(5*5), 0.01)
rsca$Affect_control <- scales::percent(rsca$Affect_control/(6*5), 0.01)
rsca$Positive_thinking <- scales::percent(rsca$Positive_thinking/(4*5), 0.01)
rsca$Family_support <- scales::percent(rsca$Family_support/(6*5), 0.01)
rsca$Help_seeking <- scales::percent(rsca$Help_seeking/(6*5), 0.01)
rsca$Self <- scales::percent(rsca$Self/(15*5), 0.01)
rsca$Support <- scales::percent(rsca$Support/(12*5), 0.01)

# save result for making report
write.xlsx(rsca[, c(1,2,30:ncol(rsca))], file.path(resultDir, "CCNPPEK_RSCA_report.xlsx"),
           rowNames = F, colNames = T)