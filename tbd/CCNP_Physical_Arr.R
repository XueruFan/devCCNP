# 本代码用来实现合并不同批次的彩巢视力和体质数据
# 范雪如 2024年1月26日

rm(list=ls())

dataDir <- '/Users/xuerufan/Desktop/问卷录入/自处理数据'
resultDir <- "/Users/xuerufan/Desktop/材料准备"
setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)


########################################################## Part1：视力 #############################

basic <- read.csv("CCNPPEK被试信息表.csv")
eye <- read.csv("视力.csv")

eye <- eye[, c("姓名", "裸眼视力.L.R.", "X.1", "矫正视力.戴眼镜..L.R.", "X.2", "wave")]
basic <- merge(basic, eye, by = c("姓名", "wave"), all.y = TRUE)

basic$编号 <- str_pad(basic$编号, 3, side = "left", "0")
basic$FID <- paste0(basic$编号, "-W", basic$wave)

eye <- basic[, c("FID", "姓名", "裸眼视力.L.R.", "X.1", "矫正视力.戴眼镜..L.R.", "X.2")]
eye$"裸眼视力.L.R." <- as.numeric(eye$"裸眼视力.L.R.")
eye$"矫正视力.戴眼镜..L.R." <- as.numeric(eye$"矫正视力.戴眼镜..L.R.")
eye$"X.1" <- as.numeric(eye$"X.1")
eye$"X.2" <- as.numeric(eye$"X.2")
eye[, c("裸眼视力.L.R.", "X.1", "矫正视力.戴眼镜..L.R.", "X.2")] <-
  format(eye[, c("裸眼视力.L.R.","X.1", "矫正视力.戴眼镜..L.R.", "X.2")], digits = 2)
################### 到这一步后检查一下这个数据框，有时候主试把被试名字输错了，会没有编号

colnames(eye)[3:6] <- c("裸眼视力左", "裸眼视力右", "矫正视力左", "矫正视力右")
eye <- eye[, -2]

write.xlsx(eye, file.path(resultDir, "CCNPPEK_Eyesight_All.xlsx"), rowNames = F, colNames = T)
rm(list = c("eye", "basic"))


########################################################## Part2：体能 #############################

basic <- read.csv("CCNPPEK被试信息表.csv")
basic$编号 <- str_pad(basic$编号, 3, side = "left", "0")
basic$FID <- paste0(basic$编号, "-W", basic$wave)

physic <- read.xlsx("体质数据13.xlsx", rowNames = F)
physic$编号 <- str_pad(physic$编号, 3, side = "left", "0")
physic <- physic %>% # 添加上W1
  mutate(编号 = if_else(str_detect(编号, "w"), 编号, paste0(编号, "-W1")))
colnames(physic)[1] <- "FID"
physic$FID <- gsub("w", "W", physic$FID)

weight <- read.xlsx("/Users/xuerufan/Desktop/问卷录入/CCNPPEK_Scale_A_All.xlsx", rowNames = F)[-1,]
weight <- data.frame(weight$FID, weight$WEIGHT)
colnames(weight) <- c("FID", "WEIGHT")

physic <- merge(physic, weight, by = "FID", all.x = TRUE)
physic$体重 <- round(as.numeric(physic$WEIGHT), 2)
physic$握力 <- (physic$左握力 + physic$右握力) / 2
physic$握力指数 <- round(physic$握力/physic$体重 * 100, 2)
physic <- physic[, c(-2, -3, -6)]

physic <- merge(physic, basic[, c(3,7,8)], by = "FID", all.x = TRUE)
physic <- physic[, c(1,8,7,4:6,2,3)]


physic2 <- read.xlsx("体质数据2.xlsx", rowNames = F)
physic2$ID <- str_pad(physic2$ID, 3, side = "left", "0")
physic2$FID <- paste0(physic2$ID, physic2$wave)
physic2 <- physic2[, c(8,3:7)]
physic2 <- merge(physic2, basic[, c(3,7,8)], by = "FID", all.x = TRUE)
physic2 <- physic2[, c(1,8,7,2:6)]
physic2$握力指数 <- round(physic2$握力指数, 2)
physic2$立定跳远 <- round(physic2$立定跳远)

physic <- rbind(physic, physic2)
physic <- distinct(physic) # 把重复的行名删掉一行

# physic$FID <- gsub("-W", "_", physic$FID)

colnames(physic)[c(3,7)] <- c("性别", "跳远")

write.xlsx(physic, "CCNPPEK_Physical_All.xlsx", rowNames = F, colNames = T)