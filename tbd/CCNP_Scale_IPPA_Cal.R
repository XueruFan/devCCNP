# 本代码用来实现计算彩巢数据中IPPA得分
# 范雪如 2024年1月26日

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入'
resultDir <- '/Users/xuerufan/Desktop/材料准备'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

all <- read.xlsx("CCNPPEK_Scale_B_All.xlsx", rowNames = F)

ippa <- all[, c(3:4, which(names(all)=="P5Q3S01A"):which(names(all)=="P5Q3S25C"))]

# # define where to save raw data file
# setwd(filefolder)
# 
# # save a copy of raw data
# write.xlsx(ippa, "CCNPPEK_IPPA_raw.xlsx", rowNames = F, colNames = F)

## pick up three sub-scales of father, mother and peer
ippa_f <- ippa[, c(1, 2, 3:27)]
ippa_m <- ippa[, c(1, 2, 28:52)]
ippa_p <- ippa[, c(1, 2, 53:77)]

## rename colnames
ippa_f[1, 3:27] <- seq(1, 25)
ippa_m[1, 3:27] <- seq(1, 25)
ippa_p[1, 3:27] <- seq(1, 25)

colnames(ippa_f) <- ippa_f[1, ]
ippa_f <- ippa_f[-1, ]
colnames(ippa_m) <- ippa_m[1, ]
ippa_m <- ippa_m[-1, ]
colnames(ippa_p) <- ippa_p[1, ]
ippa_p <- ippa_p[-1, ]

# 计分方式参考文献详见雪如笔记“父母、同伴关系问卷计分规则.docx”

## 反向计分题号 
index_f <- c(3,9,6,14,8,10,11,17,18,23)
index_m <- c(3,9,6,14,8,10,11,17,18,23)
index_p <- c(5,4,9,10,11,18,22,23)

## 进行反向赋值
for (i in 1:length(index_f)) {
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_f$'", index_f[i], "'[ippa_f$'", index_f[i], "'== 7] <- 4")))
}
for (i in 1:length(index_m)) {
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_m$'", index_f[i], "'[ippa_m$'", index_f[i], "'== 7] <- 4")))
}
for (i in 1:length(index_p)) {
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 1] <- 6")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 2] <- 7")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 4] <- 2")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 5] <- 1")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 6] <- 5")))
  eval(parse(text = paste0("ippa_p$'", index_f[i], "'[ippa_p$'", index_f[i], "'== 7] <- 4")))
}

## 提取每个子量表下的三个维度：信任、交流、疏远
# father
cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
trust_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

cols_remain<- c("5","6","7","14","15","16","19","24","25")
commu_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

cols_remain<- c("8","10","11","17","18","23")
alien_f <- ippa_f[ ,colnames(ippa_f) %in% cols_remain]

# mather
cols_remain<- c("1","2","3","4","9","12","13","20","21","22")
trust_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

cols_remain<- c("5","6","7","14","15","16","19","24","25")
commu_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

cols_remain<- c("8","10","11","17","18","23")
alien_m <- ippa_m[ ,colnames(ippa_m) %in% cols_remain]

# peer
cols_remain<- c("5","6","8","12","13","14","15","19","20","21")
trust_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

cols_remain<- c("1","2","3","7","16","17","24","25")
commu_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

cols_remain<- c("4","9","10","11","18","22","23")
alien_p <- ippa_p[ ,colnames(ippa_p) %in% cols_remain]

## calculate score
Score <- data.frame(matrix(ncol = 9, nrow = 0))

for (i in 1:nrow(ippa_f)) {
  #father
  score <- as.numeric(trust_f[i,])
  Score[i, 1] <- sum(score)
  
  score <- as.numeric(commu_f[i,])
  Score[i, 2] <- sum(score)
  
  score <- as.numeric(alien_f[i,])
  Score[i, 3] <- sum(score)
  
  #mother
  score <- as.numeric(trust_m[i,])
  Score[i, 4] <- sum(score)
  
  score <- as.numeric(commu_m[i,])
  Score[i, 5] <- sum(score)
  
  score <- as.numeric(alien_m[i,])
  Score[i, 6] <- sum(score)
  
  #peer
  score <- as.numeric(trust_p[i,])
  Score[i, 7] <- sum(score)
  
  score <- as.numeric(commu_p[i,])
  Score[i, 8] <- sum(score)
  
  score <- as.numeric(alien_p[i,])
  Score[i, 9] <- sum(score)
}

IPPA <- cbind(ippa_f$Participant, ippa_f$Session, Score)
colnames(IPPA) <- c("Participant", "Session","Father_Trust", "Father_Communication",
                    "Father_Alienation", "Mother_Trust", "Mother_Communication",
                    "Mother_Alienation", "Peer_Trust", "Peer_Communication", "Peer_Alienation")


# 计算三个子量表的量表分
for (i in 1:nrow(IPPA)) {
  IPPA$Father[i] <- sum(as.numeric(IPPA[i, 3:5]))
  IPPA$Mother[i] <- sum(as.numeric(IPPA[i, 6:8]))
  IPPA$Peer[i] <- sum(as.numeric(IPPA[i, 9:11]))
}

## 去掉没有做这个问卷的被试
IPPA[is.na(IPPA)] = 0

# 计算总分
for (i in 1:nrow(IPPA)) {
  IPPA$Total[i] <- sum(as.numeric(IPPA[i, 12:14]))
}

IPPA <- subset(IPPA, Total != 0)
IPPA <- IPPA[, -15]
IPPA[IPPA == 0] = NA 

## save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(IPPA, "CCNPPEK_IPPA.xlsx", rowNames = F, colNames = T)

# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

# 父亲、母亲和同伴三个子量表的子维度
IPPA$Father_Trust_Per <- scales::percent(IPPA$Father_Trust/(ncol(trust_f)*5), 0.01)
IPPA$Father_Communication_Per <- scales::percent(IPPA$Father_Communication/(ncol(commu_f)*5), 0.01)
IPPA$Father_Alienation_Per <- scales::percent(IPPA$Father_Alienation/(ncol(alien_f)*5), 0.01)
IPPA$Mother_Trust_Per <- scales::percent(IPPA$Mother_Trust/(ncol(trust_m)*5), 0.01)
IPPA$Mother_Communication_Per <- scales::percent(IPPA$Mother_Communication/(ncol(commu_m)*5), 0.01)
IPPA$Mother_Alienation_Per <- scales::percent(IPPA$Mother_Alienation/(ncol(alien_m)*5), 0.01)
IPPA$Peer_Trust_Per <- scales::percent(IPPA$Peer_Trust/(ncol(trust_p)*5), 0.01)
IPPA$Peer_Communication_Per <- scales::percent(IPPA$Peer_Communication/(ncol(commu_p)*5), 0.01)
IPPA$Peer_Alienation_Per <- scales::percent(IPPA$Peer_Alienation/(ncol(alien_p)*5), 0.01)

# 父亲、母亲和同伴三个子量表的总分
IPPA$Father_Per <- scales::percent(IPPA$Father/(25*5), 0.01)
IPPA$Mother_Per <- scales::percent(IPPA$Mother/(25*5), 0.01)
IPPA$Peer_Per <- scales::percent(IPPA$Peer/(25*5), 0.01)

# save result for making report
write.xlsx(IPPA[, c(1,2,15:ncol(IPPA))], file.path(resultDir, "CCNPPEK_IPPA_Report.xlsx"),
           rowNames = F, colNames = T)

