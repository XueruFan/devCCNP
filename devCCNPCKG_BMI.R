# arrange devCCNPCKG Physical results into open-share csv file
# Xue-Ru Fan @BNU, 20 Feb 2023

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)
library(tidyr)

# define environment variables
filefolder <- "/Volumes/FANXUERU/重庆数据整理"
setwd(filefolder)

# load raw data file
rawdata <- read.xlsx("demography_SWU-20220628.xlsx", rowNames = F)
bmi_columns <- grep("BMI", colnames(rawdata), value = TRUE)
bmi <- rawdata[, c("ID", bmi_columns)]
# load id code
idfile <- "id_code.csv"
idcode <- read.csv(idfile)

# modify
rawdata <- merge(bmi, idcode, by = "ID", all = T)
colnames(rawdata)[2:4] <- c("1", "2", "3")
# correct id
id_list <- paste0("A12", 1:6)
for (id in id_list) {
  rawdata[which(rawdata$ID == id), "1"] <- rawdata[which(rawdata$ID == id), "2"]
  rawdata[which(rawdata$ID == id), "2"] <- rawdata[which(rawdata$ID == id), "3"]
  rawdata[which(rawdata$ID == id), "3"] <- NA
}
id_list <- c("A127", "A128")
for (id in id_list) {
  rawdata[which(rawdata$ID == id), "1"] <- rawdata[which(rawdata$ID == id), "3"]
  rawdata[which(rawdata$ID == id), "3"] <- NA
}
rawdata[which(rawdata$ID == "B079"), "1"] <- rawdata[which(rawdata$ID == "B079"), "2"]
rawdata[which(rawdata$ID == "B079"), "2"] <- NA

bmi <- gather(rawdata, Session, BMI, 2:4) # wide to long
bmi <- bmi[, -1]

bmi$Participant <- str_pad(bmi$Participant, 4, side = "left", "0")
bmi$Session <- str_pad(bmi$Session, 2, side = "left", "0")

bmi <- bmi %>% filter(!is.na(BMI) & BMI != 0)
bmi$BMI <- sprintf("%.2f", bmi$BMI)

write.xlsx(bmi, "devCCNPCKG_BMI.xlsx", rowNames = F, colNames = T)

