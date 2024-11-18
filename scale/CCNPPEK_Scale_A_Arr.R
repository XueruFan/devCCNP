# 本代码用来实现合并不同批次的彩巢人口信息
# 范雪如 2024年1月26日

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)



############### 从Batch4及以后开始使用下面这部分代码 ##############

############## 合并新旧文件
batch_old <- read.xlsx(file.path(dataDir, "CCNPPEK_Scale_A_Batch123.xlsx"), rowNames = F)
batch_old <- batch_old[, -3:-4]

batch_new <- read.xlsx(file.path(dataDir, "基本信息表+问卷首页+补录数据OK.xlsx"), rowNames = F)

batch_all <- rbind(batch_old, batch_new) # 先2个合并在一起

rm(list = c("batch_old", "batch_new"))


############# 挑批次
batch_all$FID <- Replace(batch_all$FID, "BJCCNP_", "") # delete BJCCNP_

ses2 <- batch_all[grep("-W2$", batch_all$FID),]
ses3 <- batch_all[grep("-W3$", batch_all$FID),]
ses4 <- batch_all[grep("-W4$", batch_all$FID),]

ses1 <- setdiff(setdiff(setdiff(batch_all, ses2), ses3), ses4)


################ 规范化处理
ses2$Participant <- str_pad(Replace(ses2$FID, "-W2", ""), 4, side = "left", "0")
ses2$Session <- "02"

ses3$Participant <- str_pad(Replace(ses3$FID, "-W3", ""), 4, side = "left", "0")
ses3$Session <- "03"

ses4$Participant <- str_pad(Replace(ses4$FID, "-W4", ""), 4, side = "left", "0")
ses4$Session <- "04"


ses1 <- ses1 %>% # 添加上W1
  mutate(FID = if_else(str_detect(FID, "-W"), FID, paste0(FID, "-W1")))

ses1$Participant <- str_pad(Replace(ses1$FID, "-W1", ""), 4, side = "left", "0")
ses1$Session <- c("Session", rep("01", nrow(ses1)-1))

rm(list = c("batch_all"))


############# 合并不同的轮次
all <- rbind(ses1, ses2, ses3, ses4)
all$FID <- str_pad(all$FID, 6, side = "left", "0")
all$FID[1] <- "编号"
all$Participant[1] <- "Participant"

all <- all[, c(1, 2, ncol(all)-1, ncol(all), 3:(ncol(all)-2))]

########## 删掉多余的行名
all <- all[order(all$FID, decreasing = F),]
all <- all[-nrow(all),]
all <- all[c(nrow(all), 1:((nrow(all)-1))), ]

write.xlsx(all, file.path(dataDir, "CCNPPEK_Scale_A_All_Batch4.xlsx"), rowNames = F, colNames = T)



# 以下是处理Batch123的代码，从Batch4开始不同###############
# batch1a <- read.xlsx(file.path(dataDir, 
#                               "第一批公司录入/基本信息表page1+总问卷首页.xlsx"), rowNames = F)
# batch1b <- read.xlsx(file.path(dataDir, 
#                               "第一批公司录入/基本信息表page2+主观社会地位+识字测验.xlsx"), 
#                     rowNames = F)
# batch2 <- read.xlsx(file.path(dataDir, 
#                              "第二批公司录入/基本信息表+主观社会地位+总问卷首页+识字测验.xlsx"), 
#                    rowNames = F)
# batch3 <- read.xlsx(file.path(dataDir, 
#                              "第三批公司录入/基本信息表+主观社会地位+总问卷首页+识字测验.xlsx"), 
#                    rowNames = F)
# 
# batch23 <- rbind(batch2, batch3) # 先把batch2和3合并在一起
# rm(list = c("batch2", "batch3"))
# 
# ################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# 
# batch23 <- distinct(batch23) # 把重复的行名删掉一行
# 
# batch1a <- batch1a[, -2] # 这俩表都有FID列，但是1a里的格式和其他的不一致，因此删掉
# batch1 <- merge(batch1a, batch1b, by = "ID", all = TRUE) # 把batch1的两个合并了
# rm(list = c("batch1a", "batch1b"))
# 
# batch13 <- rbind.fill(batch23, batch1) # 把bath23和1合并在一起
# rm(list = c("batch23", "batch1"))
# 
# ################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# 
# batch13 <- batch13[, -ncol(batch13)] # 删掉最后一列，使它和第二批的录入列名完全一致
# batch13 <- distinct(batch13) # 把重复的行名删掉一行
# 
# batch13$FID <- gsub("BJCCNP-", "", batch13$FID) # 把编号这一列整理成规范格式
# batch13 <- batch13 %>% # 添加上W1
#   mutate(FID = if_else(str_detect(FID, "-W"), FID, paste0(FID, "-W1")))
# batch13$FID[1] <- "编号"
# 
# 
# # pick out data from each wave，modify participant ID and add column "Session"
# ses2 <- batch13[grep("-W2$", batch13$FID),]
# ses3 <- batch13[grep("-W3$", batch13$FID),]
# 
# ses1 <- setdiff(setdiff(batch13, ses2), ses3)
# 
# ses2$Participant <- str_pad(Replace(ses2$FID, "-W2", ""), 4, side = "left", "0")
# ses2$Session <- "02"
# ses3$Participant <- str_pad(Replace(ses3$FID, "-W3", ""), 4, side = "left", "0")
# ses3$Session <- "03"
# ses1 <- ses1 %>% # 添加上W1
#   mutate(FID = if_else(str_detect(FID, "-W"), FID, paste0(FID, "-W1")))
# ses1$Participant <- str_pad(Replace(ses1$FID, "-W1", ""), 4, side = "left", "0")
# ses1$Session <- c("Session", rep("01", nrow(ses1)-1))
# 
# rm(list = c("batch13"))
# 
# all <- rbind(ses1, ses2, ses3)
# all$FID <- str_pad(all$FID, 6, side = "left", "0")
# all$FID[1] <- "编号"
# all$Participant[1] <- "Participant"
# all <- all[, c(1, 2, ncol(all)-1, ncol(all), 3:(ncol(all)-2))]
# 
# write.xlsx(all, file.path(dataDir, "CCNPPEK_Scale_A_Batch123.xlsx"), rowNames = F, colNames = T)