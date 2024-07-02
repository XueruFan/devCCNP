# this code is used to arrange CCNPPEK CBCL score result into tsv file
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
# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPPEK_CBCL.csv"
rawdata <- read.csv(rawfile)
rawdata <- rawdata[-1,]

# modify participant ID and add column "Session"
rawdata$Participant <- str_pad(rawdata$Participant, 4, side = "left", "0")
rawdata$Session <- str_pad(rawdata$Session, 2, side = "left", "0")
rawdata <- rawdata[,-3]
rawdata <- rawdata[, c(2,1,3:ncol(rawdata))]

######################## Arrange CBCL result #######################################################

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
write.xlsx(rawdata, "CCNPPEK_CBCL.xlsx", rowNames = F, colNames = T)

############# save each participant's data into tsv file ###########################################
setwd(paste0(filefolder, "/DataArrange/CBCL"))
for (i in 1:nrow(rawdata)) {
  data <- rawdata[i,]
  dataname <- paste0("CCNPPEK", rawdata[i, 1], "_ses-", rawdata[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
