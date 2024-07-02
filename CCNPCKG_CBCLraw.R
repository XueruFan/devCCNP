# this code is used to arrange CCNPCKG CBCL raw result into tsv file
# copyright: Xue-Ru Fan @BNU, 21 Feb 2023

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
# wave1
rawfile <-  "CCNPCKG_wave1.xlsx"
rawdata_w1 <- read.xlsx(rawfile, sheet = 1, rowNames = F, colNames = F)
rawdata_w1[2,1] <- "ID"
colnames(rawdata_w1) <- rawdata_w1[2,]
rawdata_w1 <- rawdata_w1[-2:-1,]
#wave2
rawfile <-  "CCNPCKG_wave2.xlsx"
rawdata_w2 <- read.xlsx(rawfile, sheet = 1, rowNames = F, colNames = F)
rawdata_w2[2,1] <- "ID"
colnames(rawdata_w2) <- rawdata_w2[2,]
rawdata_w2 <- rawdata_w2[-2:-1,]
######## 两轮的数据一样？？？？？？？？？？

idfile <- "CCNPSWU_IDcode.csv"
idcode <- read.csv(idfile)

# # modify
# rawdata <- merge(rawdata, idcode, by = "ID", all = T)
# colnames(rawdata)[3:5] <- c("1", "2", "3")
# rawdata <- gather(rawdata, Session, Age, 3:5) # wide to long
# rawdata <- rawdata[, -1]
# scandate <- scandate[, -1]
# rawdata <- merge(rawdata, scandate, by = intersect(names(rawdata), names(scandate)),
#                  by.rawdata = by, by.scandate = by, all = T)
# 
# rawdata$Participant <- str_pad(rawdata$Participant, 4, side = "left", "0")
# rawdata$Session <- str_pad(rawdata$Session, 2, side = "left", "0")
# 
# rawdata$Race <- "Asian"
# rawdata$BirthDate <- as.Date(as.character(rawdata$BirthDate),"%Y/%m/%d")
# rawdata$ScanDate <- as.Date(as.character(rawdata$ScanDate),"%Y/%m/%d")
# rawdata <- rawdata[, c("Participant", "Session", "Sex", "Age", "Race", "BirthDate", "ScanDate")]
# rawdata <- rawdata[-which(is.na(rawdata$Age)&is.na(rawdata$ScanDate)), ]
# 
# 
# 
# # modify participant ID and add column "Session"
# # Session 1
# raw_ses1$FID <- str_pad(raw_ses1$FID, 4, side = "left", "0")
# ses <- c("Session", rep("01", nrow(raw_ses1)-1))
# raw_ses1 <- cbind(raw_ses1$FID, ses, raw_ses1[,2:ncol(raw_ses1)])
# colnames(raw_ses1)[1] <- "ID"
# # Session 2
# raw_ses2$FID <- Replace(raw_ses2$FID, "-W2", "")
# raw_ses2$FID <- str_pad(raw_ses2$FID, 4, side = "left", "0")
# ses <- rep("02", nrow(raw_ses2))
# raw_ses2 <- cbind(raw_ses2$FID, ses, raw_ses2[,2:ncol(raw_ses2)])
# colnames(raw_ses2)[1] <- "ID"
# # Session 3
# raw_ses3$FID <- Replace(raw_ses3$FID, "-W3", "")
# raw_ses3$FID <- str_pad(raw_ses3$FID, 4, side = "left", "0")
# ses <- rep("03", nrow(raw_ses3))
# raw_ses3 <- cbind(raw_ses3$FID, ses, raw_ses3[,2:ncol(raw_ses3)])
# colnames(raw_ses3)[1] <- "ID"
# 
# # combine 3 sessions together
# raw <- rbind(raw_ses1, raw_ses2, raw_ses3)
# raw[1,1] <- "Participant"
# 
# ######################## CBCLraw ###################################################################
# 
# # select CBCL data from raw data and save it
# cbcl <- raw[, 1:227]
# setwd(paste0(filefolder, "/RawData/"))
# write.xlsx(cbcl, "CCNPPEK_CBCLraw.xlsx", rowNames = F, colNames = F)
# 
# # save each participant's data into tsv file
# setwd(paste0(filefolder, "/DataArrange/CBCLraw"))
# for (i in 2:nrow(cbcl)) {
#   data <- cbcl[c(1, i),]
#   dataname <- paste0("CCNPPEK", cbcl[i, 1], "_ses-", cbcl[i, 2], "_task-CBCLraw_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = F, quote = F,)
# }