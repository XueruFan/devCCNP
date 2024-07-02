# this code is used to arrange CCNPPEK CBCL raw result into tsv file
# copyright: Xue-Ru Fan @BNU, 10 Feb 2023

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
# rawfile <-  "CCNPPEK_Questionnaires.xlsx"
rawfile <-  "CCNPPEK_CBCL2.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1]

# pick out data from each wave
raw_ses2 <- rawdata[grep("-W2$", rawdata[, 1]),]
raw_ses3 <- rawdata[grep("-W3$", rawdata[, 1]),]
raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)

# modify participant ID and add column "Session"
# Session 1
raw_ses1$FID1 <- str_pad(raw_ses1$FID1, 4, side = "left", "0")
ses <- c("Session", rep("01", nrow(raw_ses1)-1))
raw_ses1 <- cbind(raw_ses1$FID1, ses, raw_ses1[,2:ncol(raw_ses1)])
colnames(raw_ses1)[1] <- "ID"
# Session 2
raw_ses2$FID1 <- Replace(raw_ses2$FID1, "-W2", "")
raw_ses2$FID1 <- str_pad(raw_ses2$FID1, 4, side = "left", "0")
ses <- rep("02", nrow(raw_ses2))
raw_ses2 <- cbind(raw_ses2$FID1, ses, raw_ses2[,2:ncol(raw_ses2)])
colnames(raw_ses2)[1] <- "ID"
# Session 3
raw_ses3$FID1 <- Replace(raw_ses3$FID1, "-W3", "")
raw_ses3$FID1 <- str_pad(raw_ses3$FID1, 4, side = "left", "0")
ses <- rep("03", nrow(raw_ses3))
raw_ses3 <- cbind(raw_ses3$FID1, ses, raw_ses3[,2:ncol(raw_ses3)])
colnames(raw_ses3)[1] <- "ID"

# combine 3 sessions together
raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
raw[1,1] <- "Participant"

######################## CBCLraw ###################################################################

# select CBCL data from raw data and save it
cbcl <- raw[, 1:227]
setwd(paste0(filefolder, "/RawData/"))
# write.xlsx(cbcl, "CCNPPEK_CBCLraw.xlsx", rowNames = F, colNames = F)
write.xlsx(cbcl, "CCNPPEK_CBCL2raw.xlsx", rowNames = F, colNames = F)

# save each participant's data into tsv file
setwd(paste0(filefolder, "/DataArrange/CBCLraw"))
for (i in 2:nrow(cbcl)) {
  data <- cbcl[c(1, i),]
  dataname <- paste0("CCNPPEK", cbcl[i, 1], "_ses-", cbcl[i, 2], "_task-CBCLraw_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = F, quote = F,)
}