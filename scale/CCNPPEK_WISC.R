# this code is used to arrange CCNPPEK IQ result, old system
# copyright: Xue-Ru Fan @BNU, 4 Dec 2022

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表"
setwd(filefolder)

#################### arrange raw data ##############################################################
# load raw data file
setwd(file.path(filefolder, "source"))
rawfile <-  "CCNPPEK_WISC_Batch4.xlsx"
rawdata <- read.xlsx(rawfile)

########### 以下这部分是双重比对之前手工录入的和公司录入的是否一致，今后不再需要这一步了
# # double-check ！！！重要
# checkfile <-  "CCNPPEK_IQ_2nd.csv"
# checkdata <- read.csv(checkfile) # 第二遍录入的数据
# rawcheck <- left_join(checkdata, rawdata, by = "ID")
# rawcheck <- rawcheck[, c(1,42, 43, 45:82)] # 第一遍录入的数据
# colnames(rawcheck) <- colnames(checkdata)
# which(checkdata!=rawcheck, arr.ind = TRUE) # 比对
# # 返回出不一致的位置，再核对修改后再进行下面的步骤

# pick out data from each wave
raw_ses2 <- rawdata[grep("_w2$", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("_w3$", rawdata[, 1]),]
raw_ses4 <- rawdata[grep("_w4$", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(setdiff(rawdata, raw_ses2), raw_ses3), raw_ses4)

# modify participant ID and add column "Session"
# Session 1
raw_ses1$ID <- Replace(raw_ses1$ID, "_w1", "")
raw_ses1$ID <- str_pad(raw_ses1$ID, 4, side = "left", "0")
ses <- rep("01", nrow(raw_ses1))
raw_ses1 <- cbind(raw_ses1$ID, ses, raw_ses1[,2:ncol(raw_ses1)])
colnames(raw_ses1)[1] <- "ID"
# Session 2
raw_ses2$ID <- Replace(raw_ses2$ID, "_w2", "")
raw_ses2$ID <- str_pad(raw_ses2$ID, 4, side = "left", "0")
ses <- rep("02", nrow(raw_ses2))
raw_ses2 <- cbind(raw_ses2$ID, ses, raw_ses2[,2:ncol(raw_ses2)])
colnames(raw_ses2)[1] <- "ID"
# Session 3
raw_ses3$ID <- Replace(raw_ses3$ID, "_w3", "")
raw_ses3$ID <- str_pad(raw_ses3$ID, 4, side = "left", "0")
ses <- rep("03", nrow(raw_ses3))
raw_ses3 <- cbind(raw_ses3$ID, ses, raw_ses3[,2:ncol(raw_ses3)])
colnames(raw_ses3)[1] <- "ID"
# Session 4
raw_ses4$ID <- Replace(raw_ses4$ID, "_w4", "")
raw_ses4$ID <- str_pad(raw_ses4$ID, 4, side = "left", "0")
ses <- rep("04", nrow(raw_ses4))
raw_ses4 <- cbind(raw_ses4$ID, ses, raw_ses4[,2:ncol(raw_ses4)])
colnames(raw_ses4)[1] <- "ID"

# combine 3 sessions together
raw <- rbind(raw_ses1, raw_ses2, raw_ses3, raw_ses4)
colnames(raw)[c(1,2,6)] <- c("Participant","Session","Age_in_Years")

######################## Arrange IQ result #########################################################

# save a copy of raw data
setwd(file.path(filefolder, "raw"))
write.xlsx(raw, "CCNPPEK_WISC_Batch4_raw.xlsx", rowNames = F, colNames = T)

# arrange data
colnames(raw)[c(21:34, 40:44)] <- c("CoreSubtest_BlockDesign_ScaleScore",
                                    "CoreSubtest_Similarities_ScaleScore",
                                    "CoreSubtest_DigitSpan_ScaleScore",
                                    "CoreSubtest_PictureConcept_ScaleScore",
                                    "CoreSubtest_Coding_ScaleScore",
                                    "CoreSubtest_Vocabulary_ScaleScore",
                                    "CoreSubtest_LetterNumberSequencing_ScaleScore",
                                    "CoreSubtest_MatrixReasoning_ScaleScore",
                                    "CoreSubtest_Comprehension_ScaleScore",
                                    "CoreSubtest_SymbolSearch_ScaleScore",
                                    "SupplementalSubtest_PictureCompletion_ScaleScore",
                                    "SupplementalSubtest_Cancellation_ScaleScore",
                                    "SupplementalSubtest_Information_ScaleScore",
                                    "SupplementalSubtest_Arithmetic_ScaleScore",
                                    "VerbalComprehensionIndex", "PerceptualReasoningIndex",
                                    "WorkingMemoryIndex", "ProcessingSpeedIndex",
                                    "FSIQ")
iq <- raw[c(1, 2, 6, 21:34, 40:44)]
iq$Age_in_Years <- as.numeric(sub("岁.*", "", iq$Age_in_Years))

# save finial result of this scale
setwd(file.path(filefolder, "scale"))
write.xlsx(iq, "CCNPPEK_WISC_Batch4.xlsx", rowNames = F, colNames = T)


############ 以下是处理成年人和幼儿已经手动录好的文件，从Batch3之后就没有维护了

# #################### arrange adult data ############################################################
# 
# # clear environment
# rm(list=ls()) 
# 
# # load packages
# library(ggplot2)
# library(dplyr)
# library(openxlsx)
# library(stringr)
# library(do)
# 
# # define environment variables
# filefolder <- "F:/CCNPdataArrange/Scales"
# 
# # load raw data file
# setwd(paste0(filefolder, "/RawData/Source"))
# rawfile <-  "CCNPPEK_IQ_Adult.csv"
# rawdata <- read.csv(rawfile)
# 
# # modify participant ID and add column "Session"
# # Session 1
# rawdata$ID <- str_pad(rawdata$ID, 4, side = "left", "0")
# ses <- rep("01", nrow(rawdata))
# Note <- rep("AdultVersion", nrow(rawdata))
# rawdata <- cbind(rawdata$ID, ses, rawdata[,2:ncol(rawdata)], Note)
# colnames(rawdata)[c(1,2,5)] <- c("Participant","Session","Age_in_Years")
# 
# # save a copy of raw data
# setwd(paste0(filefolder, "/RawData/"))
# write.xlsx(rawdata, "CCNPPEK_IQ_Adult_raw.xlsx", rowNames = F, colNames = T)
# 
# # arrange data
# colnames(rawdata)[c(16:32)] <- c("VC_ScaleScore", "SI_ScaleScore", "IN_ScaleScore", "BD_ScaleScore",
#                              "MR_ScaleScore", "VP_ScaleScore", "DS_ScaleScore", "AR_ScaleScore",
#                              "CD_ScaleScore", "SS_ScaleScore", "GAI", "CPI", "VCI", "PRI", "WMI",
#                              "PSI", "FSIQ")
# iq <- rawdata[c(1,2,5,16:33)]
# 
# # save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(iq, "CCNPPEK_IQ_Adult.xlsx", rowNames = F, colNames = T)
# 
# 
# #################### arrange preschool data ########################################################
# 
# # clear environment
# rm(list=ls()) 
# 
# # load packages
# library(ggplot2)
# library(dplyr)
# library(openxlsx)
# library(stringr)
# library(do)
# 
# # define environment variables
# filefolder <- "F:/CCNPdataArrange/Scales"
# 
# # load raw data file
# setwd(paste0(filefolder, "/RawData/Source"))
# rawfile <-  "CCNPPEK_IQ_Preschool.csv"
# rawdata <- read.csv(rawfile)
# 
# # modify participant ID and add column "Session"
# # Session 1
# rawdata$ID <- str_pad(rawdata$ID, 4, side = "left", "0")
# ses <- rep("01", nrow(rawdata))
# Note <- rep("PreschoolVersion", nrow(rawdata))
# rawdata <- cbind(rawdata$ID, ses, rawdata[,2:ncol(rawdata)], Note)
# colnames(rawdata)[c(1,2,5)] <- c("Participant","Session","Age_in_Years")
# 
# # save a copy of raw data
# setwd(paste0(filefolder, "/RawData/"))
# write.xlsx(rawdata, "CCNPPEK_IQ_Preschool_raw.xlsx", rowNames = F, colNames = T)
# 
# # arrange data
# colnames(rawdata)[c(17:27,34:39)] <- c("BD_ScaleScore", "IN_ScaleScore", "MR_ScaleScore", 
#                                        "BS_ScaleScore", "PM_ScaleScore", "SI_ScaleScore",
#                                        "PC_ScaleScore", "CA_ScaleScore", "ZL_ScaleScore",
#                                        "OA_ScaleScore", "AC_ScaleScore", "VCI", "VSI", "FRI", "WMI",
#                                        "PSI", "FSIQ")
# iq <- rawdata[c(1,2,5,17:27,34:40)]
# 
# # save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(iq, "CCNPPEK_IQ_Preschool.xlsx", rowNames = F, colNames = T)
# 
