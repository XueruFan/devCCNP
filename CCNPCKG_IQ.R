# this code is used to arrange CCNPCKG IQ result into tsv file
# copyright: Xue-Ru Fan @BNU, 4 Dec 2023

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
rawfile <-  "CCNPSWU_IQ.csv"
rawdata <- read.csv(rawfile)

# pick out data from each wave
raw_ses2 <- rawdata[grep("wave2", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("wave3", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)

# modify participant ID and add column "Session"
# Session 1
raw_ses1$ID <- Replace(raw_ses1$ID, "wave1_", "")
raw_ses1$ID <- str_pad(raw_ses1$ID, 4, side = "left", "0")
ses <- rep("01", nrow(raw_ses1))
raw_ses1 <- cbind(raw_ses1$ID, ses, raw_ses1[,2:ncol(raw_ses1)])
colnames(raw_ses1)[1] <- "ID"
# Session 2
raw_ses2$ID <- Replace(raw_ses2$ID, "wave2_", "")
raw_ses2$ID <- str_pad(raw_ses2$ID, 4, side = "left", "0")
ses <- rep("02", nrow(raw_ses2))
raw_ses2 <- cbind(raw_ses2$ID, ses, raw_ses2[,2:ncol(raw_ses2)])
colnames(raw_ses2)[1] <- "ID"
# Session 3
raw_ses3$ID <- Replace(raw_ses3$ID, "wave3_", "")
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
write.xlsx(raw, "CCNPCKG_IQ_raw.xlsx", rowNames = F, colNames = T)

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
write.xlsx(iq, "CCNPCKG_IQ.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/WISC"))
for (i in 1:nrow(iq)) {
  data <- iq[i,]
  dataname <- paste0("CCNPCKG", iq[i, 1], "_ses-", iq[i, 2], "_task-WISC_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
