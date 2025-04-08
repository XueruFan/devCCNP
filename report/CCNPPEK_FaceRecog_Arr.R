# 本代码用来实现整合处理面部表情识别的数据，用于制作报告
# 范雪如 2024年大年初四于北师大办公室

rm(list=ls())

dataDir <- "//172.16.191.42/home/项目管理/CCNP/数据电子化/自处理数据/情绪识别原始文件"
# resultDir <- '/Users/xuerufan/Desktop/流水线制作材料/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)


############################################## wave 1 ##############################################

face_w1 <- read.xlsx("wave1.xlsx", rowNames = F)


####################### 删除练习的4个试次数据（每名被试的前4个试次，前4行）#########################
filtered <- face_w1 %>%
  group_by(Subject) %>%
  slice(-(1:4)) %>%
  ungroup()

######################## 筛选特定的列
selected <- dplyr::select(filtered, Subject, word1, word2, Trial, Attribute2, correctanswer,
                             Judgement1.ACC, Judgement1.RESP, Judgement1.RT)
selected$word1 <- substr(selected$word1, 3, 4)
selected$word2 <- substr(selected$word2, 3, 4)
selected$Attribute2 <- substr(selected$Attribute2, 3, 4)

######################## 确定表情分组
selected$Group <- paste0(selected$word1, selected$word2)
selected$Group <- gsub("恐惧悲伤", "悲伤恐惧", selected$Group)
selected$Group <- gsub("高兴悲伤", "悲伤高兴", selected$Group)
selected$Group <- gsub("惊讶悲伤", "悲伤惊讶", selected$Group)
selected$Group <- gsub("愤怒悲伤", "悲伤愤怒", selected$Group)
selected$Group <- gsub("厌恶悲伤", "悲伤厌恶", selected$Group)
selected$Group <- gsub("高兴愤怒", "愤怒高兴", selected$Group)
selected$Group <- gsub("惊讶愤怒", "愤怒惊讶", selected$Group)
selected$Group <- gsub("恐惧愤怒", "愤怒恐惧", selected$Group)
selected$Group <- gsub("厌恶愤怒", "愤怒厌恶", selected$Group)
selected$Group <- gsub("惊讶高兴", "高兴惊讶", selected$Group)
selected$Group <- gsub("恐惧高兴", "高兴恐惧", selected$Group)
selected$Group <- gsub("厌恶高兴", "高兴厌恶", selected$Group)
selected$Group <- gsub("恐惧惊讶", "惊讶恐惧", selected$Group)
selected$Group <- gsub("厌恶惊讶", "惊讶厌恶", selected$Group)
selected$Group <- gsub("厌恶恐惧", "恐惧厌恶", selected$Group)

######################## 记录正确试次的反应时
selected$RTadjust <- NA
for (i in 1:nrow(selected)) {
  if (selected$Judgement1.ACC[i] == "1") {
    selected$RTadjust[i] <- selected$Judgement1.RT[i]
  } else {
    selected$RTadjust[i] <- NA
  }
}
clean <- selected %>% drop_na(RTadjust)

######################## 统计数据
clean$Group <- as.factor(clean$Group)
clean$Attribute2 <- as.factor(clean$Attribute2)
rt_mean <- aggregate(clean$RTadjust, by = list(clean$Subject, clean$Group,
                                                  clean$Attribute2), mean)
acc_sum <- aggregate(clean$Judgement1.ACC, by = list(clean$Subject, clean$Group,
                                                  clean$Attribute2), sum)
all <- merge(rt_mean, acc_sum, by = c("Group.1", "Group.2", "Group.3"))
all$x.y <- all$x.y / 10 # 正确率
colnames(all) <- c("P", "G", "E", "RTmean", "ACC")

rt_each_mean <- aggregate(all$RTmean, by = list(all$P, all$E), mean)
acc_each_mean <- aggregate(all$ACC, by = list(all$P, all$E), mean)
all2 <- merge(rt_each_mean, acc_each_mean, by = c("Group.1", "Group.2"))
colnames(all2) <- c("FID", "E", "RTmean", "ACCmean")

######################## 规范命名
all2$FID <- paste0(str_pad(all2$FID, 3, side = "left", "0"), "-W1")
wave1 <- all2

objects_to_keep <- c("dataDir", "wave1", "face_w1") # 列出要保留的对象名称
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象



############################################## wave 2 ##############################################

face_w2 <- read.xlsx("wave2.xlsx", rowNames = F)


####################### 删除练习的4个试次数据（每名被试的前4个试次，前4行）#########################
filtered <- face_w2 %>%
  group_by(Subject) %>%
  slice(-(1:4)) %>%
  ungroup()

######################## 筛选特定的列
selected <- dplyr::select(filtered, Subject, word1, word2, Trial, Attribute2, correctanswer,
                          Judgement1.ACC, Judgement1.RESP, Judgement1.RT)
selected$word1 <- substr(selected$word1, 3, 4)
selected$word2 <- substr(selected$word2, 3, 4)
selected$Attribute2 <- substr(selected$Attribute2, 3, 4)

######################## 确定表情分组
selected$Group <- paste0(selected$word1, selected$word2)
selected$Group <- gsub("恐惧悲伤", "悲伤恐惧", selected$Group)
selected$Group <- gsub("高兴悲伤", "悲伤高兴", selected$Group)
selected$Group <- gsub("惊讶悲伤", "悲伤惊讶", selected$Group)
selected$Group <- gsub("愤怒悲伤", "悲伤愤怒", selected$Group)
selected$Group <- gsub("厌恶悲伤", "悲伤厌恶", selected$Group)
selected$Group <- gsub("高兴愤怒", "愤怒高兴", selected$Group)
selected$Group <- gsub("惊讶愤怒", "愤怒惊讶", selected$Group)
selected$Group <- gsub("恐惧愤怒", "愤怒恐惧", selected$Group)
selected$Group <- gsub("厌恶愤怒", "愤怒厌恶", selected$Group)
selected$Group <- gsub("惊讶高兴", "高兴惊讶", selected$Group)
selected$Group <- gsub("恐惧高兴", "高兴恐惧", selected$Group)
selected$Group <- gsub("厌恶高兴", "高兴厌恶", selected$Group)
selected$Group <- gsub("恐惧惊讶", "惊讶恐惧", selected$Group)
selected$Group <- gsub("厌恶惊讶", "惊讶厌恶", selected$Group)
selected$Group <- gsub("厌恶恐惧", "恐惧厌恶", selected$Group)

######################## 记录正确试次的反应时
selected$RTadjust <- NA
for (i in 1:nrow(selected)) {
  if (selected$Judgement1.ACC[i] == "1") {
    selected$RTadjust[i] <- selected$Judgement1.RT[i]
  } else {
    selected$RTadjust[i] <- NA
  }
}
clean <- selected %>% drop_na(RTadjust)

######################## 统计数据
clean$Group <- as.factor(clean$Group)
clean$Attribute2 <- as.factor(clean$Attribute2)
rt_mean <- aggregate(clean$RTadjust, by = list(clean$Subject, clean$Group,
                                               clean$Attribute2), mean)
acc_sum <- aggregate(clean$Judgement1.ACC, by = list(clean$Subject, clean$Group,
                                                     clean$Attribute2), sum)
all <- merge(rt_mean, acc_sum, by = c("Group.1", "Group.2", "Group.3"))
all$x.y <- all$x.y / 10 # 正确率
colnames(all) <- c("P", "G", "E", "RTmean", "ACC")

rt_each_mean <- aggregate(all$RTmean, by = list(all$P, all$E), mean)
acc_each_mean <- aggregate(all$ACC, by = list(all$P, all$E), mean)
all2 <- merge(rt_each_mean, acc_each_mean, by = c("Group.1", "Group.2"))
colnames(all2) <- c("FID", "E", "RTmean", "ACCmean")

######################## 规范命名
all2$FID <- paste0(str_pad(all2$FID, 3, side = "left", "0"), "-W2")
wave2 <- all2

objects_to_keep <- c("dataDir", "wave1", "wave2", "face_w1", "face_w2") # 列出要保留的对象名称
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象


############################################## wave 3 ##############################################

face_w3 <- read.xlsx("wave3.xlsx", rowNames = F)


####################### 删除练习的4个试次数据（每名被试的前4个试次，前4行）#########################
filtered <- face_w3 %>%
  group_by(Subject) %>%
  slice(-(1:4)) %>%
  ungroup()

######################## 筛选特定的列
selected <- dplyr::select(filtered, Subject, word1, word2, Trial, Attribute2, correctanswer,
                          Judgement1.ACC, Judgement1.RESP, Judgement1.RT)
selected$word1 <- substr(selected$word1, 3, 4)
selected$word2 <- substr(selected$word2, 3, 4)
selected$Attribute2 <- substr(selected$Attribute2, 3, 4)

######################## 确定表情分组
selected$Group <- paste0(selected$word1, selected$word2)
selected$Group <- gsub("恐惧悲伤", "悲伤恐惧", selected$Group)
selected$Group <- gsub("高兴悲伤", "悲伤高兴", selected$Group)
selected$Group <- gsub("惊讶悲伤", "悲伤惊讶", selected$Group)
selected$Group <- gsub("愤怒悲伤", "悲伤愤怒", selected$Group)
selected$Group <- gsub("厌恶悲伤", "悲伤厌恶", selected$Group)
selected$Group <- gsub("高兴愤怒", "愤怒高兴", selected$Group)
selected$Group <- gsub("惊讶愤怒", "愤怒惊讶", selected$Group)
selected$Group <- gsub("恐惧愤怒", "愤怒恐惧", selected$Group)
selected$Group <- gsub("厌恶愤怒", "愤怒厌恶", selected$Group)
selected$Group <- gsub("惊讶高兴", "高兴惊讶", selected$Group)
selected$Group <- gsub("恐惧高兴", "高兴恐惧", selected$Group)
selected$Group <- gsub("厌恶高兴", "高兴厌恶", selected$Group)
selected$Group <- gsub("恐惧惊讶", "惊讶恐惧", selected$Group)
selected$Group <- gsub("厌恶惊讶", "惊讶厌恶", selected$Group)
selected$Group <- gsub("厌恶恐惧", "恐惧厌恶", selected$Group)

######################## 记录正确试次的反应时
selected$RTadjust <- NA
for (i in 1:nrow(selected)) {
  if (selected$Judgement1.ACC[i] == "1") {
    selected$RTadjust[i] <- selected$Judgement1.RT[i]
  } else {
    selected$RTadjust[i] <- NA
  }
}
clean <- selected %>% drop_na(RTadjust)

######################## 统计数据
clean$Group <- as.factor(clean$Group)
clean$Attribute2 <- as.factor(clean$Attribute2)
rt_mean <- aggregate(clean$RTadjust, by = list(clean$Subject, clean$Group,
                                               clean$Attribute2), mean)
acc_sum <- aggregate(clean$Judgement1.ACC, by = list(clean$Subject, clean$Group,
                                                     clean$Attribute2), sum)
all <- merge(rt_mean, acc_sum, by = c("Group.1", "Group.2", "Group.3"))
all$x.y <- all$x.y / 10 # 正确率
colnames(all) <- c("P", "G", "E", "RTmean", "ACC")

rt_each_mean <- aggregate(all$RTmean, by = list(all$P, all$E), mean)
acc_each_mean <- aggregate(all$ACC, by = list(all$P, all$E), mean)
all2 <- merge(rt_each_mean, acc_each_mean, by = c("Group.1", "Group.2"))
colnames(all2) <- c("FID", "E", "RTmean", "ACCmean")

######################## 规范命名
all2$FID <- paste0(str_pad(all2$FID, 3, side = "left", "0"), "-W3")
wave3 <- all2

objects_to_keep <- c("dataDir", "wave1", "wave2", "wave3", "face_w1", "face_w2", "face_w3") 
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象


############################################## wave 4 ##############################################

face_w4 <- read.xlsx("wave4.xlsx", rowNames = F)


####################### 删除练习的4个试次数据（每名被试的前4个试次，前4行）#########################
filtered <- face_w4 %>%
  group_by(Subject) %>%
  slice(-(1:4)) %>%
  ungroup()

######################## 筛选特定的列
selected <- dplyr::select(filtered, Subject, word1, word2, Trial, Attribute2, correctanswer,
                          Judgement1.ACC, Judgement1.RESP, Judgement1.RT)
selected$word1 <- substr(selected$word1, 3, 4)
selected$word2 <- substr(selected$word2, 3, 4)
selected$Attribute2 <- substr(selected$Attribute2, 3, 4)

######################## 确定表情分组
selected$Group <- paste0(selected$word1, selected$word2)
selected$Group <- gsub("恐惧悲伤", "悲伤恐惧", selected$Group)
selected$Group <- gsub("高兴悲伤", "悲伤高兴", selected$Group)
selected$Group <- gsub("惊讶悲伤", "悲伤惊讶", selected$Group)
selected$Group <- gsub("愤怒悲伤", "悲伤愤怒", selected$Group)
selected$Group <- gsub("厌恶悲伤", "悲伤厌恶", selected$Group)
selected$Group <- gsub("高兴愤怒", "愤怒高兴", selected$Group)
selected$Group <- gsub("惊讶愤怒", "愤怒惊讶", selected$Group)
selected$Group <- gsub("恐惧愤怒", "愤怒恐惧", selected$Group)
selected$Group <- gsub("厌恶愤怒", "愤怒厌恶", selected$Group)
selected$Group <- gsub("惊讶高兴", "高兴惊讶", selected$Group)
selected$Group <- gsub("恐惧高兴", "高兴恐惧", selected$Group)
selected$Group <- gsub("厌恶高兴", "高兴厌恶", selected$Group)
selected$Group <- gsub("恐惧惊讶", "惊讶恐惧", selected$Group)
selected$Group <- gsub("厌恶惊讶", "惊讶厌恶", selected$Group)
selected$Group <- gsub("厌恶恐惧", "恐惧厌恶", selected$Group)

######################## 记录正确试次的反应时
selected$RTadjust <- NA
for (i in 1:nrow(selected)) {
  if (selected$Judgement1.ACC[i] == "1") {
    selected$RTadjust[i] <- selected$Judgement1.RT[i]
  } else {
    selected$RTadjust[i] <- NA
  }
}
clean <- selected %>% drop_na(RTadjust)

######################## 统计数据
clean$Group <- as.factor(clean$Group)
clean$Attribute2 <- as.factor(clean$Attribute2)
rt_mean <- aggregate(clean$RTadjust, by = list(clean$Subject, clean$Group,
                                               clean$Attribute2), mean)
acc_sum <- aggregate(clean$Judgement1.ACC, by = list(clean$Subject, clean$Group,
                                                     clean$Attribute2), sum)
all <- merge(rt_mean, acc_sum, by = c("Group.1", "Group.2", "Group.3"))
all$x.y <- all$x.y / 10 # 正确率
colnames(all) <- c("P", "G", "E", "RTmean", "ACC")

rt_each_mean <- aggregate(all$RTmean, by = list(all$P, all$E), mean)
acc_each_mean <- aggregate(all$ACC, by = list(all$P, all$E), mean)
all2 <- merge(rt_each_mean, acc_each_mean, by = c("Group.1", "Group.2"))
colnames(all2) <- c("FID", "E", "RTmean", "ACCmean")

######################## 规范命名
all2$FID <- paste0(str_pad(all2$FID, 3, side = "left", "0"), "-W4")
wave4 <- all2

objects_to_keep <- c("dataDir", "wave1", "wave2", "wave3", "face_w1", "face_w2", "face_w3", "wave4",
                     "face_w4") 
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象


############################################## 整合到一起 ##############################################

all <- rbind(wave1, wave2)
all <- rbind(all, wave3)
all <- rbind(all, wave4)
all_ACC <- spread(all[, c("FID", "E", "ACCmean")], key = "E", value = "ACCmean")
all_RT <- spread(all[, c("FID", "E", "RTmean")], key = "E", value = "RTmean")
colnames(all_ACC)[2:ncol(all_ACC)] <- paste0("ACCmean_", names(all_ACC)[2:ncol(all_ACC)])
colnames(all_RT)[2:ncol(all_RT)] <- paste0("RTmean_", names(all_RT)[2:ncol(all_RT)])                

all <- merge(all_ACC, all_RT, by = "FID")

all <- all[, c("FID", "ACCmean_恐惧", "ACCmean_惊讶", "ACCmean_悲伤", "ACCmean_厌恶", "ACCmean_愤怒",
               "ACCmean_高兴", "RTmean_恐惧", "RTmean_惊讶", "RTmean_悲伤", "RTmean_厌恶",
               "RTmean_愤怒", "RTmean_高兴")]

write.xlsx(all, "CCNPPEK_FaceRecog_report.xlsx", rowNames = F, colNames = T)
