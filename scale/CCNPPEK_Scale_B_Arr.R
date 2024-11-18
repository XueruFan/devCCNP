# this script is used to combine different batches of CCNP scales raw data
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

rm(list=ls()) 

dataDir <- '//172.16.191.42/home/CCNP/20240517/scale'

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)



############### 从Batch4及以后开始使用下面这部分代码 ##############

batchold <- read.xlsx(file.path(dataDir, "CCNPPEK_Scale_B_Batch123.xlsx"), rowNames = F)
batchnew <- read.xlsx(file.path(dataDir, "总问卷OK.xlsx"), rowNames = F)
batchold <- batchold[, -3:-4]  ####删掉原合并数据的3.4列



######## 下面这部分很重要，检查这两批次的问卷录入表头是不是完全一致的
Index1 <- colnames(batchold)
Index2 <- colnames(batchnew)
CheckName <- data.frame(Index1, Index2)
CheckName$SameIndex <- CheckName$Index1 == CheckName$Index2
CheckName$Name1 <- as.character(t(batchold[1, ]))
CheckName$Name2 <- as.character(t(batchnew[1, ]))
CheckName$SameName <- CheckName$Name1 == CheckName$Name2
##### 之后肉眼检查两批次不一样的地方，确认没有再继续，有的话要分析原因及时修正

batchall <- rbind(batchold, batchnew) # 先把old和new合并在一起
rm(list = c("batchold", "batchnew", "CheckName", "Index1", "Index2", "packages"))
################# 做完这一步后，视觉检查一下合并的有没有问题，再继续

batchall <- distinct(batchall) # 把重复的行名删掉一行


# 把编号整理成规范格式
batchall$FID <- gsub("BJCCNP_", "", batchall$FID)

# pick out data from each wave，modify participant ID and add column "Session"
ses2 <- batchall[grep("-W2$", batchall$FID),]
ses3 <- batchall[grep("-W3$", batchall$FID),]
ses4 <- batchall[grep("-W4$", batchall$FID),]

ses1 <- setdiff(setdiff(setdiff(batchall, ses2), ses3), ses4)

#####################规范化命名###########
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

rm(list = c("batchall"))

batchall <- rbind(ses1, ses2, ses3, ses4)
batchall$FID <- str_pad(batchall$FID, 6, side = "left", "0")
batchall$FID[1] <- "编码"
batchall$Participant[1] <- "Participant"
batchall <- batchall[, c(1, 2, ncol(batchall)-1, ncol(batchall), 3:(ncol(batchall)-2))]

# 保存一份总问卷数据
write.xlsx(all, file.path(dataDir, "CCNPPEK_Scale_B_Batch4.xlsx"), rowNames = F, colNames = T)



# 以下是处理Batch123的代码，从Batch4开始不同###############
# 
# batch1 <- read.xlsx(file.path(dataDir, "第一批公司录入/CBCL+总问卷.xlsx"), rowNames = F)
# batch2 <- read.xlsx(file.path(dataDir, "第二批公司录入/总问卷.xlsx"), rowNames = F)
# batch3 <- read.xlsx(file.path(dataDir, "第三批公司录入/总问卷.xlsx"), rowNames = F)
# 
# ######## 下面这部分很重要，检查这两批次的问卷录入表头是不是完全一致的
# Index2 <- colnames(batch2)
# Index3 <- colnames(batch3)
# CheckName <- data.frame(Index2, Index3)
# CheckName$SameIndex <- CheckName$Index2 == CheckName$Index3
# CheckName$Name2 <- as.character(t(batch2[1, ]))
# CheckName$Name3 <- as.character(t(batch3[1, ]))
# CheckName$SameName <- CheckName$Name2 == CheckName$Name3
# ##### 之后肉眼检查第三批和第二批不一致的地方，确认没有再继续，有的话要分析原因及时修正
# 
# batch23 <- rbind(batch2, batch3) # 先把bath2和3合并在一起
# rm(list = c("batch2", "batch3", "CheckName", "Index2", "Index3", "packages"))
# ################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# 
# batch23 <- distinct(batch23) # 把重复的行名删掉一行
# 
# batch1 <- batch1[, -c(3:227)] # 把batch1里的cbcl部分删掉
# ######## 下面这部分很重要，检查第一批次和后面两批次的问卷录入表头是不是完全一致的
# Index1 <- colnames(batch1)
# Index23 <- colnames(batch23)
# CheckName <- data.frame(Index1, Index23)
# CheckName$SameIndex <- CheckName$Index1 == CheckName$Index23
# CheckName$Name1 <- as.character(t(batch1[1, ]))
# CheckName$Name23 <- as.character(t(batch23[1, ]))
# CheckName$SameName <- CheckName$Name1 == CheckName$Name23
# ##### 之后肉眼检查是否有不一致的地方，确认没有再继续，有的话要分析原因及时修正
# 
# batch13 <- rbind(batch23, batch1) # 把bath23和1合并在一起
# rm(list = c("batch23", "batch1", "CheckName", "Index1", "Index23"))
# batch13 <- distinct(batch13) # 把重复的行名删掉一行, 这个文件就是合并后的原始文件
# 
# # 把编号整理成规范格式
# batch13$FID <- gsub("BJCCNP-", "", batch13$FID)
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
# all$FID[1] <- "编码"
# all$Participant[1] <- "Participant"
# all <- all[, c(1, 2, ncol(all)-1, ncol(all), 3:(ncol(all)-2))]
# 
# # 保存一份总问卷数据
# write.xlsx(all, file.path(dataDir, "CCNPPEK_Scale_B_Batch123.xlsx"), rowNames = F, colNames = T)
