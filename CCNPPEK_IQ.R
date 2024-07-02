# this code is used to arrange CCNPPEK IQ result into tsv file
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
filefolder <- "F:/CCNPdataArrange/Scales"
setwd(filefolder)

#################### arrange raw data ##############################################################
# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPPEK_IQ.csv"
rawdata <- read.csv(rawfile)

# double-check ！！！重要
checkfile <-  "CCNPPEK_IQ_2nd.csv"
checkdata <- read.csv(checkfile) # 第二遍录入的数据
rawcheck <- left_join(checkdata, rawdata, by = "ID")
rawcheck <- rawcheck[, c(1,42, 43, 45:82)] # 第一遍录入的数据
colnames(rawcheck) <- colnames(checkdata)
which(checkdata!=rawcheck, arr.ind = TRUE) # 比对
# 返回出不一致的位置，再核对修改后再进行下面的步骤

# pick out data from each wave
raw_ses2 <- rawdata[grep("_w2$", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("_w3$", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)

# modify participant ID and add column "Session"
# Session 1
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

# combine 3 sessions together
raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
colnames(raw)[c(1,2,5)] <- c("Participant","Session","Age_in_Years")

######################## Arrange IQ result #########################################################

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(raw, "CCNPPEK_IQ_raw.xlsx", rowNames = F, colNames = T)

# arrange data
colnames(raw)[c(20:33, 39:43)] <- c("CoreSubtest_BlockDesign_ScaleScore",
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
iq <- raw[c(1,2,5,20:33,39:43)]

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(iq, "CCNPPEK_IQ.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/WISC"))
for (i in 1:nrow(iq)) {
  data <- iq[i,]
  dataname <- paste0("CCNPPEK", iq[i, 1], "_ses-", iq[i, 2], "_task-WISC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

#####
#################### arrange adult data ############################################################

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "F:/CCNPdataArrange/Scales"

# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPPEK_IQ_Adult.csv"
rawdata <- read.csv(rawfile)

# modify participant ID and add column "Session"
# Session 1
rawdata$ID <- str_pad(rawdata$ID, 4, side = "left", "0")
ses <- rep("01", nrow(rawdata))
Note <- rep("AdultVersion", nrow(rawdata))
rawdata <- cbind(rawdata$ID, ses, rawdata[,2:ncol(rawdata)], Note)
colnames(rawdata)[c(1,2,5)] <- c("Participant","Session","Age_in_Years")

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(rawdata, "CCNPPEK_IQ_Adult_raw.xlsx", rowNames = F, colNames = T)

# arrange data
colnames(rawdata)[c(16:32)] <- c("VC_ScaleScore", "SI_ScaleScore", "IN_ScaleScore", "BD_ScaleScore",
                             "MR_ScaleScore", "VP_ScaleScore", "DS_ScaleScore", "AR_ScaleScore",
                             "CD_ScaleScore", "SS_ScaleScore", "GAI", "CPI", "VCI", "PRI", "WMI",
                             "PSI", "FSIQ")
iq <- rawdata[c(1,2,5,16:33)]

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(iq, "CCNPPEK_IQ_Adult.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/WISC"))
for (i in 1:nrow(iq)) {
  data <- iq[i,]
  dataname <- paste0("CCNPPEK", iq[i, 1], "_ses-", iq[i, 2], "_task-WISC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

#####
#################### arrange preschool data ########################################################

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)

# define environment variables
filefolder <- "F:/CCNPdataArrange/Scales"

# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPPEK_IQ_Preschool.csv"
rawdata <- read.csv(rawfile)

# modify participant ID and add column "Session"
# Session 1
rawdata$ID <- str_pad(rawdata$ID, 4, side = "left", "0")
ses <- rep("01", nrow(rawdata))
Note <- rep("PreschoolVersion", nrow(rawdata))
rawdata <- cbind(rawdata$ID, ses, rawdata[,2:ncol(rawdata)], Note)
colnames(rawdata)[c(1,2,5)] <- c("Participant","Session","Age_in_Years")

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(rawdata, "CCNPPEK_IQ_Preschool_raw.xlsx", rowNames = F, colNames = T)

# arrange data
colnames(rawdata)[c(17:27,34:39)] <- c("BD_ScaleScore", "IN_ScaleScore", "MR_ScaleScore", 
                                       "BS_ScaleScore", "PM_ScaleScore", "SI_ScaleScore",
                                       "PC_ScaleScore", "CA_ScaleScore", "ZL_ScaleScore",
                                       "OA_ScaleScore", "AC_ScaleScore", "VCI", "VSI", "FRI", "WMI",
                                       "PSI", "FSIQ")
iq <- rawdata[c(1,2,5,17:27,34:40)]

# save finial result of this scale
setwd(paste0(filefolder, "/DataArrange/"))
write.xlsx(iq, "CCNPPEK_IQ_Preschool.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/WISC"))
for (i in 1:nrow(iq)) {
  data <- iq[i,]
  dataname <- paste0("CCNPPEK", iq[i, 1], "_ses-", iq[i, 2], "_task-WISC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
