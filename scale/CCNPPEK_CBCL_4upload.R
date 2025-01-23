# this script is used to arrange CCNP CBCL raw data into standard format to upload onto website
# copyright: Xue-Ru Fan @BNU, 8 Feb 2024

rm(list=ls())

dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据电子化/自处理数据/CBCL'
setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", 
              "stringr", "do", "tidyselect")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)


####################### Part 1：规范处理一下本批次的被试编号 #########################################
batch4 <- read.xlsx(file.path(dataDir, "CBCL_Batch4.xlsx"), rowNames = F)[, -1]

colnames(batch4)[1] <- "FID"
batch4$FID <- Replace(batch4$FID, "BJCCNP_", "") # delete BJCCNP_

ses2 <- batch4[grep("-W2$", batch4$FID),]
ses3 <- batch4[grep("-W3$", batch4$FID),]
ses4 <- batch4[grep("-W4$", batch4$FID),]

ses1 <- setdiff(setdiff(setdiff(batch4, ses2), ses3), ses4)

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

############# 合并不同的轮次
batch4 <- rbind(ses1, ses2, ses3, ses4)
batch4$FID <- str_pad(batch4$FID, 6, side = "left", "0")
batch4$FID[1] <- "FID"
batch4$Participant[1] <- "Participant"

batch4 <- batch4[, c(1, ncol(batch4)-1, ncol(batch4), 2:(ncol(batch4)-2))]
#### 这些是本次要上传计算得分的人

rm(list = c("ses1", "ses2", "ses3", "ses4"))



####################### Part 2: 合并本批次和之前的CBCL原始数据 #########################################

batch123 <- read.xlsx(file.path(dataDir, "CCNPPEK_CBCL_Batch123.xlsx"), rowNames = F)

####### 下面这部分很重要，检查本批次和之前的问卷录入表头是不是完全一致的
Index4 <- colnames(batch4)
Index1 <- colnames(batch123)
CheckName14 <- data.frame(Index4, Index1)
CheckName14$SameIndex <- CheckName14$Index4 == CheckName14$Index1
CheckName14$Name4 <- as.character(t(batch4[1, ]))
CheckName14$Name1 <- as.character(t(batch123[1, ]))
CheckName14$SameName <- CheckName14$Name4 == CheckName14$Name1

#### 之后肉眼检查是否有不一致的地方，确认没有再继续，有的话要分析原因及时修正
#### 公司每次录入的表头都有一点不一样，找到以后需要手动改成和之前的batch一致
#### 如果是顺序错了没关系，但如果是表头不一致需要改成一致的
#### P3Q7E、F、G这部分顺序经常搞错，表头也补全，需要认真核对手动补全

batch1234 <- rbind.fill(batch123, batch4) # 把本批次和之前的合并在一起

rm(list = c("batch123", "CheckName14", "Index1", "Index4"))
################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# Index <- data.frame(colnames(batch1234))
# Index$Name <- as.character(t(batch1234[1, ]))

batch1234 <- distinct(batch1234) # 把重复的行名删掉一行

######################### 保存一个合并后的原始文件 #####################################
write.xlsx(batch1234, "CCNPPEK_CBCL_Batch1234.xlsx", rowNames = F, colNames = T)



####################### Part 3: 把本批次要上传计算分数的人挑出来 ###################################

# 根据batch4的FID列筛选batch1234
batch <- batch1234 %>%
  filter(FID %in% batch4$FID)


####################### Part 4: 列顺序置换并提取需要的列 ###########################################

##### 注意不同批次录入的数据列顺序可能不一样，一定要根据正确的列名来索引
# 能力量表共40题
comp_index <- c("P1Q1S1A", "P1Q1S1B", "P1Q1S1C", "P1Q1S2A", "P1Q1S3A", "P1Q1S2B", "P1Q1S3B",
                "P1Q1S2C", "P1Q1S3C", "P1Q2S1A", "P1Q2S1B", "P1Q2S1C", "P1Q2S2A", "P1Q2S3A",
                "P1Q2S2B", "P1Q2S3B", "P1Q2S2C", "P1Q2S3C", "P1Q3S1A", "P1Q3S1B", "P1Q3S1C",
                "P1Q3S2A", "P1Q3S2B", "P1Q3S2C", "P1Q4S1A", "P1Q4S1B", "P1Q4S1C", "P1Q4S2A",
                "P1Q4S2B", "P1Q4S2C", "P2Q5A", "P2Q5B", "P2Q6A", "P2Q6B", "P2Q6C", "P2Q6D", "P3Q7A",
                "P3Q7B", "P3Q7C", "P3Q7D", "P3Q7E", "P3Q7F", "P3Q7G", "P3Q7S2A", "P3Q7S3A",
                "P3Q7S4A", "P3Q7S4D", "P3Q7S5A")
comp <- batch[, comp_index]

# 在列名为P1Q1S1A，P1Q2S1A，P1Q3S1A，P1Q4S1A左侧插入列，计数这些列右3列非空单元格的数量
target_cols <- c("P1Q1S1A", "P1Q2S1A", "P1Q3S1A", "P1Q4S1A") # 定位目标列的名称
for(col in target_cols) { # 为每个目标列计算右侧三列中有内容的列数，并创建新列
  new_col_name <- paste0("Count_", col)  # 计算右侧三列中有内容的列数
  right_cols <- which(names(comp) == col) + 1:min(3, ncol(comp) - which(names(comp) == col))
  comp[[new_col_name]] <- rowSums(!is.na(comp[, right_cols]), na.rm = TRUE)
}
new_order <- names(comp)
for(col in target_cols) {
  new_col_name <- paste0("Count_", col)
  pos <- match(col, new_order)  # 找到原始列名的位置
  new_order <- c(new_order[1:(pos-1)], new_col_name, new_order[pos:length(new_order)])
}
new_order <- new_order[2:(length(new_order)-4)]
comp <- comp[, new_order]
comp_to_remove <- c("P1Q1S1A", "P1Q1S1B", "P1Q1S1C", "P1Q2S1A", "P1Q2S1B", "P1Q2S1C", "P1Q3S1A",
                    "P1Q3S1B", "P1Q3S1C", "P1Q4S1A", "P1Q4S1B", "P1Q4S1C")
comp <- comp[, !(names(comp) %in% comp_to_remove)] # 删除上面那些列

# 问题量表共120题
bp <- batch[, c(which(names(batch)=="Q8S1"):which(names(batch)=="Q8S113"))]
bp_to_remove <- c("Q8S2A", "Q8S9A", "Q8S29A", "Q8S40A", "Q8S46A",
                  "Q8S56", "Q8S56D1", "Q8S56H1", "Q8S58A", "Q8S66A", "Q8S70A", "Q8S73A",
                  "Q8S77A", "Q8S79A", "Q8S83A", "Q8S84A", "Q8S85A", "Q8S92A", "Q8S100A",
                  "Q8S105A", "Q8S113A", "Q8S113A1", "Q8S113B", "Q8S113B1", "Q8S113C", "Q8S113C1")
bp <- bp[, !(names(bp) %in% bp_to_remove)] # 删除上面那些列

# 也就是说从原始量表中删除了以下列
# "P3Q7": 7 (1)以下是有关贵子女学业成绩之问题。如子女没有受教育，请写出理由【原来这列叫AA】
# "P3Q7E1","P3Q7F1","P3Q7G1"：其他学术科目，例如电脑、地理或商科。(不包括体操、劳作或其他非学术科目)
# "P3Q7S2B"：请注明班级或学校类别
# "P3Q7S3B"：请注明留级的班级及理由
# "P3Q7S4B"：请详述困难
# "P3Q7S4C"：该困难在甚么时候开始
# "P3Q7S4E"：请注明何时终止
# "P3Q7S5B"：请详述状况
# "P3Q7S6"：(6)你最关注贵子女的是甚么
# "P3Q7S7"：(7)请形容贵子女之各项优点
# "Q8S2A","Q8S9A","Q8S29A","Q8S40A","Q8S46A","Q8S56D1","Q8S56H1","Q8S58A","Q8S66A","Q8S70A",
# "Q8S73A","Q8S77A","Q8S79A","Q8S83A","Q8S84A","Q8S85A","Q8S92A","Q8S100A","Q8S105A": 请描述
# "Q8S113A": a.
# "Q8S113B": b.
# "Q8S113C": c.
# "Q8S113A1","Q8S113B1","Q8S113C1": 请描述


####################### Part 5: 数值替换并提取字符 #################################################

# 将量表数值中超过3的值改为3，空白值填为9
# 能力量表
comp[2:nrow(comp),] <- lapply(comp[2:nrow(comp),], function(x) {
  x <- as.numeric(as.character(x))  # 将所有列的字符数据转换为数值
  x[x > 3] <- 3
  x[is.na(x)] <- 9
  return(x)
})
# 提取数字变成字符串
comp_text <- apply(comp[2:nrow(comp),], 2, as.character) # 将数字转换为字符
partAB <- data.frame(apply(comp_text, 1, paste, collapse = ""))
colnames(partAB) <- "Comp"
partAB$Comp <- paste0("'", partAB$Comp) # 增加英文符号'

# 问题量表
bp[2:nrow(bp),] <- lapply(bp[2:nrow(bp),], function(x) {
  x <- as.numeric(as.character(x))  # 将所有列的字符数据转换为数值
  x[x > 3] <- 3
  x[is.na(x)] <- 9
  return(x)
})
bp_text <- apply(bp[2:nrow(bp),], 2, as.character) # 将数字转换为字符
partAB$Bp <- paste0("'", apply(bp_text, 1, paste, collapse = "")) # 增加英文符号'

objects_to_keep <- c("bp", "comp", "batch", "partAB", "dataDir") # 列出要保留的对象名称
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象


####################### Part 3: 整理成待上传模板格式 ###############################################

batch$subjectno <- seq(1000, 999+nrow(batch)) # 给所有被试新的编号从1001开始，1000是标题行
###### 每批次上传的编号要和之前的不重复，batch5从901开始

temp <- read.csv(file.path(dataDir, "保存备用_CBCL_上传模板.csv"))

# 创建一个全是NA的数据框
na <- as.data.frame(matrix(rep(NA, (nrow(batch)-1) * ncol(temp)), nrow = nrow(batch)-1,
                           ncol = ncol(temp)))
names(na) <- names(temp) # 设置新数据框的列名与原始数据框相同
temp <- rbind(temp, na) # 将NA数据框添加到原始数据框下方

temp$subjectno <- batch$subjectno # 给所有被试新的编号从1001开始，1000是标题行
temp$formno <- batch$subjectno
temp$id <- paste0(batch$subjectno, "t")
temp$formid <- paste0("T", batch$subjectno)

temp$compitems[2:nrow(temp)] <- partAB$Comp
temp$bpitems[2:nrow(temp)] <- partAB$Bp

# 把cbcl上传使用的id和原始FID的对应关系保存备份
IDcode <- data.frame(batch$FID, batch$subjectno)[-1,]
colnames(IDcode) <- c("FID", "subjectno")
write.csv(IDcode, file.path(dataDir, "保存备用_CBCL_IDcode_Batch4.csv"), row.names = F)

objects_to_keep <- c("batch", "temp", "dataDir") # 列出要保留的对象名称
rm(list = (setdiff(ls(), objects_to_keep))) # 删除不在保留列表中的所有对象


####################### Part 4: 补全待上传模板中的信息 #############################################
# 补全性别和年龄
basic <- read.xlsx("//172.16.191.42/home/项目管理/CCNP/数据电子化/自处理数据/CCNPPEK被试信息表_Batch1234.xlsx")

basic$FID <- paste0(str_pad(basic$编号, 3, side = "left", "0"), "-W", basic$wave)
basic <- basic[, c("FID", "性别", "年龄")]
colnames(basic)[2:3] <- c("Sex", "Age")
basic$Sex <- gsub("1", "M", basic$Sex)
basic$Sex <- gsub("0", "F", basic$Sex)

temp$X <- temp$X[1]
temp$datatype <- temp$datatype[1]
temp$ethniccode <- temp$ethniccode[1]
temp$dataver <- temp$dataver[1]
temp$type <- temp$type[1]
temp$origin <- temp$origin[1]
temp$fstatus <- temp$fstatus[1]
temp$admcatlg <- temp$admcatlg[1]
temp$society <- temp$society[1]
temp <- temp[-1,]
colnames(temp)[1] <- "admver"

batch <- merge(basic, batch[-1, ], by = "FID", all.y = T)
basic <- batch[, c("FID", "Sex", "Age", "subjectno")]

temp2 <- merge(basic, temp, by = "subjectno", all.y = T)

temp2$gender <- temp2$Sex
temp2$age <- floor(temp2$Age) # 年龄取整
# temp2$age <- temp2$Age # 精确年龄


cbcl_batch4 <- temp2[, c(5,6,1,7:ncol(temp2))]

write.csv(cbcl_batch4, file.path(dataDir, "CCNPPEK_CBCL_forUpload_Batch4.csv"),
          row.names = F, na = "")



# ####################### Part 0: 合并不同批次的CBCL原始数据 #########################################
# 
# batch1 <- read.xlsx(file.path(dataDir, "第一批公司录入/CBCL+总问卷_Batch1.xlsx"), rowNames = F)[, 2:227]
# batch2 <- read.xlsx(file.path(dataDir, "第二批公司录入/CBCL_Batch2.xlsx"), rowNames = F)[, -1]
# batch3 <- read.xlsx(file.path(dataDir, "第三批公司录入/CBCL_Batch3.xlsx"), rowNames = F)[, -1]
# 
# ####### 下面这部分很重要，检查这两批次的问卷录入表头是不是完全一致的
# Index2 <- colnames(batch2)
# Index3 <- colnames(batch3)
# CheckName23 <- data.frame(Index2, Index3)
# CheckName23$SameIndex <- CheckName23$Index2 == CheckName23$Index3
# CheckName23$Name2 <- as.character(t(batch2[1, ]))
# CheckName23$Name3 <- as.character(t(batch3[1, ]))
# CheckName23$SameName <- CheckName23$Name2 == CheckName23$Name3
# 
# Index1 <- colnames(batch1)
# Index2 <- colnames(batch2)
# CheckName12 <- data.frame(Index1, Index2)
# CheckName12$SameIndex <- CheckName12$Index1 == CheckName12$Index2
# CheckName12$Name1 <- as.character(t(batch1[1, ]))
# CheckName12$Name2 <- as.character(t(batch2[1, ]))
# CheckName12$SameName <- CheckName12$Name1 == CheckName12$Name2
# #### 之后肉眼检查是否有不一致的地方，确认没有再继续，有的话要分析原因及时修正
# 
# batch23 <- rbind.fill(batch2, batch3) # 把bath2和3合并在一起
# rm(list = c("batch2", "batch3", "CheckName23", "Index1", "Index2", "Index3"))
# ################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# 
# batch13 <- rbind.fill(batch1, batch23) # 把bath2和3合并在一起
# rm(list = c("batch1", "batch23", "CheckName12"))
# ################# 做完这一步后，视觉检查一下合并的有没有问题，再继续
# # Index <- data.frame(colnames(batch13))
# # Index$Name <- as.character(t(batch13[1, ]))
# 
# # 先写成一个文件视觉检查一下有没有问题
# # write.xlsx(batch13, file.path(dataDir, "CCNPPEK_CBCL_All.xlsx"), rowNames = F, colNames = T)
# 
# batch13 <- distinct(batch13) # 把重复的行名删掉一行
# 
# batch13$FID <- gsub("BJCCNP-", "", batch13$FID) # 把编号这一列整理成规范格式
# batch13 <- batch13 %>% # 添加上W1
#   mutate(FID = if_else(str_detect(FID, "-W"), FID, paste0(FID, "-W1")))
# batch13$FID <- str_pad(batch13$FID, 6, side = "left", "0")
# batch13$FID[1] <- "FID"
# 
# ######################### 保存一个合并后的原始文件 #####################################
# write.xlsx(batch13, "CCNPPEK_CBCL_Batch123.xlsx", rowNames = F, colNames = T)
