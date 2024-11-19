# 本代码用于把彩巢IQ报告的数据汇总成excel文件，以便进行后续的数据分析和报告制作
# 注意有些IQ分数异常的，这个代码会报错，手动挑出来给他们补充手录一下就行
# 另外由于幼儿版、成人版和这个不一样，人数有比较少，就手动录入一下个别数据就行
# 范雪如 2024年1月12日 @北师大205办公室
# 注意这个代码适用升级后的系统哈

rm(list=ls())

# 安装/加载必要的包
packages <- c("pdftools", "writexl", "tidyr", "reshape2", "openxlsx")
# sapply(packages,install.packages,character.only=TRUE)
sapply(packages, require, character.only = TRUE)

ReportsFil <- "//172.16.191.42/home/项目管理/CCNP/原始数据/IQ原始报告/Batch4/儿童青少年/new_system"
ResultFil <- "//172.16.191.42/home/项目管理/CCNP/原始数据/IQ原始报告/Batch4/儿童青少年/new_system"

setwd(ReportsFil)
File_ID <- list.files()

CCNP_IQ_Result <- data.frame()

for (pdf_file in File_ID) {
  # pdf_file <- "BJCCNP_338_w1.pdf"
  # pdf_file <- "BJCCNP_322_w1.pdf"
  # pdf_file <- "BJCCNP_316_w1.pdf"
  
  # 读取PDF文件的第一页
  text <- pdf_text(pdf_file)[1]
  
  
  ################# 获取志愿者姓名 ###############################################################
  
  pattern <- "儿童姓名[\\s\\S]+?儿童代码"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  Name <- data.frame(data_list[[1]][2])
  colnames(Name) <- c("儿童姓名")
  
  ################# 获取志愿者测试信息 ###############################################################
  
  pattern <- "测试日期[\\s\\S]+?●"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  Name$"测试日期" <- paste0(data_list[[1]][2], "年", data_list[[1]][3], "月", data_list[[1]][4], "日")
  
  pattern <- "出生日期[\\s\\S]+?●"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  Name$"出生日期" <- paste0(data_list[[1]][2], "年", data_list[[1]][3], "月", data_list[[1]][4], "日")
  
  pattern <- "实足年龄[\\s\\S]+?●"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  Name$"实足年龄" <- paste0(data_list[[2]][1], "岁", data_list[[2]][2], "月", data_list[[2]][3], "日")
  
  
  ################# 获取 原始分数与量表分数转换表 数据 ###############################################
  
  Subset <- c("积木", "类同", "背数", "图画概念", "译码", "词汇", "字母-数字", "矩阵推理", "理解",
              "符号检索", "填图", "划消", "常识", "算术", "量表分数总和")
  
  data_frame <- data.frame()
  
  # 十个主要分测验
  for (set in 1:10) {
    # set <- 1
    pattern <- paste0(Subset[set], "[\\s\\S]+?", Subset[set+1])
    table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    lines <- unlist(strsplit(table_text, "\n"))
    data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
    data_frame <- rbind(data_frame, data_list[[1]][1:3])
  }
  
  # 单独处理有问题的3个分测验
  pattern <- "图画概念[\\s\\S]+?译码"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  data_frame[4,2:3] <- c(data_list[[2]][1:2])

  pattern <- "矩阵推理[\\s\\S]+?理解"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  data_frame[8,2:3] <- c(data_list[[2]][1:2])

  pattern <- "字母-数字[\\s\\S]+?符号检索"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  data_frame[9,2:3] <- c(data_list[[6]][2:3])
  
  # 处理主测验没做的那些行的得分
  data_frame[is.na(data_frame[, 3]), 2:3] <- ""
  data_frame[data_frame[, 3] == "●", 2:3] <- ""
  
  # 四个补充分测验
  for (set in 11:14) {
    # set <- 14
    pattern <- paste0(Subset[set], "[\\s\\S]+?", Subset[set+1])
    table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    lines <- unlist(strsplit(table_text, "\n"))
    data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
    data_frame <- rbind(data_frame, data_list[[1]][c(1,3,5)])
  }
  
  data_frame[, 2:3] <- Replace(data_frame[, 2:3], "\\(", "")
  data_frame[, 2:3] <- Replace(data_frame[, 2:3], "\\)", "")
  
  colnames(data_frame) <- c("分测验", "原始分数", "量表分数")
  Set_long <- melt(data_frame, id.vars = "分测验", variable.name = "测试类型", value.name = "分数")
  Set_long$"类型" <- paste0(Set_long[, 1], "_", Set_long[, 2])
  Set <- as.data.frame(t(Set_long[, c(4,3)]))
  colnames(Set) <- Set[1, ]
  Set <- Set[-1,]
  
  
  ################# 获取 量表分数总和与合成分数转换表 数据 ###########################################
  
  pattern <- "量表分数总和:[\\s\\S]+?Copyright"
  table_text <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  lines <- unlist(strsplit(table_text, "\n"))
  data_list <- lapply(lines, function(line) unlist(strsplit(trimws(line), " +")))
  data_frame <- do.call(rbind.data.frame, data_list)
  
  Scale <- data.frame(data_frame[c(8,9,10,11,12), c(1:4)])
  colnames(Scale) <- c("量表", "量表分数总和", "合成分数", "")
  Scale_long <- data.frame(t(Scale[, 2]), t(Scale[, 4]))
  colnames(Scale_long) <- c(Scale[, 1], Scale[, 3])
  colnames(Scale_long) <- gsub(":", "", colnames(Scale_long))
  
  
  ################ 以上数据整合到汇总表里 ############################################################
  All <- cbind(Name, Set, Scale_long)
  All$ID <- sub(".*P_(.*)\\..*", "\\1", pdf_file)
  
  CCNP_IQ_Result <- rbind(CCNP_IQ_Result, All)
  
}

CCNP_IQ_Result <- CCNP_IQ_Result[, c(ncol(CCNP_IQ_Result), 1:(ncol(CCNP_IQ_Result)-1))]

setwd(ResultFil)
write.xlsx(CCNP_IQ_Result, "CCNPPEK_WISC_Batch4.xlsx", rowNames = F, colNames = T)
