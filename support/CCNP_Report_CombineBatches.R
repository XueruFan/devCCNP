# 本代码用来实现彩巢报告流水线制作之前的材料准备
# 范雪如 2024年1月26日

####################################################################################################
# 公司录入的数据文件的格式不一样, 由于流水线报告制作是按照第二批的格式写的代码，因此这里把相关按照第
# 二批的格式进行合并处理
####################################################################################################


rm(list=ls())

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)


resultDir <- "C:/系统文件/工作管理/项目管理/彩巢计划/报告制作/报告流水线制作/材料准备"



################### Part1：基本信息表 #############################

dataDir <- 'C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入'

bath1a <- read.xlsx(file.path(dataDir, 
                              "第一批公司录入/基本信息表page1+总问卷首页.xlsx"), rowNames = F)
bath1b <- read.xlsx(file.path(dataDir, 
                              "第一批公司录入/基本信息表page2+主观社会地位+识字测验.xlsx"), 
                    rowNames = F)
bath2 <- read.xlsx(file.path(dataDir, 
                             "第二批公司录入/基本信息表+主观社会地位+总问卷首页+识字测验.xlsx"), 
                   rowNames = F)
bath3 <- read.xlsx(file.path(dataDir, 
                             "第三批公司录入/基本信息表+主观社会地位+总问卷首页+识字测验.xlsx"), 
                   rowNames = F)

bath23 <- rbind(bath2, bath3) # 先把bath2和3合并在一起
rm(list = c("bath2", "bath3"))

################# 做完这一步后，视觉检查一下合并的有没有问题，再继续

bath23 <- distinct(bath23) # 把重复的行名删掉一行

bath1a <- bath1a[, -2] # 这俩表都有FID列，但是1a里的格式和其他的不一致，因此删掉
bath1 <- merge(bath1a, bath1b, by = "ID", all = TRUE) # 把bath1的两个合并了
rm(list = c("bath1a", "bath1b"))

bath13 <- rbind.fill(bath23, bath1) # 把bath23和1合并在一起
rm(list = c("bath23", "bath1"))

################# 做完这一步后，视觉检查一下合并的有没有问题，再继续

bath13 <- bath13[, -ncol(bath13)] # 删掉最后一列，使它和第二批的录入列名完全一致
bath13 <- distinct(bath13) # 把重复的行名删掉一行

# 把编号这一列整理成第二批的规范格式
bath13$FID <- gsub("BJCCNP-", "", bath13$FID)

bath13 <- bath13 %>% # 添加上W1
  mutate(FID = if_else(str_detect(FID, "-W"), FID, paste0(FID, "-W1")))

bath13$FID[1] <- "编号"

# write.xlsx(bath13, file.path(resultDir, "基本信息表.xlsx"), rowNames = F, colNames = T)


################### Part2：视力 #############################

dataDir <- 'C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入'

basic <- read.csv(file.path(dataDir, "自录数据/CCNPPEK被试信息表240127.csv"))
eye <- read.csv(file.path(dataDir, "自录数据/视力.csv"))

eye <- eye[, c(-1, -4, -5, -10)]
basic <- merge(basic, eye, by = c("姓名", "wave"), all.y = TRUE)

basic$编号 <- str_pad(basic$编号, 3, side = "left", "0")
basic$FID <- paste0(basic$编号, "-W", basic$wave)

eye <- basic[, c(13, 1, 8, 4, 9:12)]
eye$"裸眼视力.L.R." <- as.numeric(eye$"裸眼视力.L.R.")
eye$"矫正视力.戴眼镜..L.R." <- as.numeric(eye$"矫正视力.戴眼镜..L.R.")
eye$"X.1" <- as.numeric(eye$"X.1")
eye$"X.2" <- as.numeric(eye$"X.2")
eye[, c(5:8)] <- format(eye[, c(5:8)], digits = 2)
################### 到这一步后检查一下这个数据框，有时候主试把被试名字输错了，会没有编号

colnames(eye)[4] <- "性别"

write.xlsx(eye, file.path(resultDir, "视力.xlsx"), rowNames = T, colNames = T)
rm(list = c("eye", "basic"))


################### Part3：体能 #############################

dataDir <- 'C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入/自录数据'

basic <- read.csv(file.path(dataDir, "CCNPPEK被试信息表240127.csv"))
basic$编号 <- str_pad(basic$编号, 3, side = "left", "0")
basic$FID <- paste0(basic$编号, "-W", basic$wave)

physic <- read.xlsx(file.path(dataDir, "体质数据13.xlsx"), rowNames = F)
physic$编号 <- str_pad(physic$编号, 3, side = "left", "0")
physic <- physic %>% # 添加上W1
  mutate(编号 = if_else(str_detect(编号, "w"), 编号, paste0(编号, "-w1")))
colnames(physic)[1] <- "FID"
physic$FID <- gsub("w", "W", physic$FID)

weight <- bath13[-1, c(2,23)]

physic <- merge(physic, weight, by = "FID", all.x = TRUE)
physic$体重 <- round(as.numeric(physic$WEIGHT), 2)
physic$握力 <- (physic$左握力 + physic$右握力) / 2
physic$握力指数 <- round(physic$握力/physic$体重 * 100, 2)
physic <- physic[, c(-2, -3, -6)]

physic <- merge(physic, basic[, c(3,7,8)], by = "FID", all.x = TRUE)
physic <- physic[, c(1,8,7,4:6,2,3)]


physic2 <- read.xlsx(file.path(dataDir, "体质数据2.xlsx"), rowNames = F)
physic2$ID <- str_pad(physic2$ID, 3, side = "left", "0")
physic2$FID <- paste0(physic2$ID, physic2$wave)
physic2 <- physic2[, c(8,3:7)]
physic2 <- merge(physic2, basic[, c(3,7,8)], by = "FID", all.x = TRUE)
physic2 <- physic2[, c(1,8,7,2:6)]
physic2$握力指数 <- round(physic2$握力指数, 2)
physic2$立定跳远 <- round(physic2$立定跳远)

physic <- rbind(physic, physic2)
physic <- distinct(physic) # 把重复的行名删掉一行

physic$FID <- gsub("-W", "_", physic$FID)

colnames(physic)[c(1,3,7)] <- c("整理后编号", "性别", "跳远")

write.xlsx(physic, file.path(resultDir, "体能测试.xlsx"), rowNames = F, colNames = T)
rm(list = c("physic", "physic2", "weight"))


################### Part4：生理信息 #############################

