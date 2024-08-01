# this code is used to arrange CCNPCKG CDIC result into csv file
# copyright: Xue-Ru Fan @BNU, 20 Feb 2023

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据备份/问卷量表"
setwd(filefolder)

#################### arrange raw data ##############################################################

# load raw data file
rawfile <-  file.path("未规范化预处理的原始材料", "CCNPCKG_CDIC_fromQJ.xlsx")
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, c(-1, -3:-5)]
colnames(rawdata)[1] <- "Name"
# load id and name
name <- file.path("相关材料", "CKG_id_code.csv")
idname <- read.csv(name)
# 合并数据框，保留 idname 中的所有行
merged <- merge(idname, rawdata, by = "Name", all.x = TRUE)

# 找到 rawdata 中的 Name 列有但 idname 中没有的行
diff <- rawdata[!rawdata$Name %in% idname$Name, ]

write.xlsx(merged, file = "彩巢西南地区被试_CDI.xlsx")
write.xlsx(diff, file = "非彩巢西南地区被试_CDI.xlsx")
