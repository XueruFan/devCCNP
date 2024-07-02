# this code is used to arrange CCNPCKG CBCL score result into tsv file
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

# load raw data file
setwd(paste0(filefolder, "/RawData/Source"))
idfile <- "CCNPSWU_IDcode.csv"
idcode <- read.csv(idfile)

###############################wave1
rawfile <-  "CCNPCKG_CBCL_wave1.xlsx"

#sheet1 男6-11
rawdata1_1 <- read.xlsx(rawfile, sheet = 1, rowNames = F)
rawdata1_1 <- rawdata1_1[, -124:-2]
colnames(rawdata1_1)[1] <- "ID"
rawdata1_1 <- merge(rawdata1_1, idcode, by = "ID")
rawdata1_1$Participant <- str_pad(rawdata1_1$Participant, 4, side = "left", "0")
rawdata1_1$Session <- "01"

rawdata1_1 <- rawdata1_1[, c(12,13,3,6,8,9,10,7,2,4,5,11)]
colnames(rawdata1_1)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                          "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                          "Schizoid_Total", "Uncommunicative_Total", "Obsessive_Total",
                          "Total_Problems_Total")

#sheet2 男12-16
rawdata1_2 <- read.xlsx(rawfile, sheet = 2, rowNames = F)
rawdata1_2 <- rawdata1_2[, -124:-2]
colnames(rawdata1_2)[1] <- "ID"
rawdata1_2 <- merge(rawdata1_2, idcode, by = "ID")
rawdata1_2$Participant <- str_pad(rawdata1_2$Participant, 4, side = "left", "0")
rawdata1_2$Session <- "01"
rawdata1_2 <- rawdata1_2[, c(12,13,2,3,5,8,9,4,6,7,10,11)]
colnames(rawdata1_2)[3:12] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Uncommunicative_Total",
                                "Obsessive_Total", "Hostile_Total", "Hyperactive_Total",
                                "Total_Problems_Total")
  
#sheet3 女6-11
rawdata1_3 <- read.xlsx(rawfile, sheet = 3, rowNames = F)
rawdata1_3 <- rawdata1_3[, -124:-2]
colnames(rawdata1_3)[1] <- "ID"
rawdata1_3 <- merge(rawdata1_3, idcode, by = "ID")
rawdata1_3$Participant <- str_pad(rawdata1_3$Participant, 4, side = "left", "0")
rawdata1_3$Session <- "01"
rawdata1_3 <- rawdata1_3[, c(12,13,2,4,6,9,8,3,5,7,10,11)]
colnames(rawdata1_3)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                                "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                                "Schizoid_Obsessive_Total", "Sex_Problems_Total", "Cruel_Total",
                                "Total_Problems_Total")

#sheet4 女12-16
rawdata1_4 <- read.xlsx(rawfile, sheet = 4, rowNames = F)
rawdata1_4 <- rawdata1_4[, -124:-2]
colnames(rawdata1_4)[1] <- "ID"
rawdata1_4 <- merge(rawdata1_4, idcode, by = "ID")
rawdata1_4$Participant <- str_pad(rawdata1_4$Participant, 4, side = "left", "0")
rawdata1_4$Session <- "01"
rawdata1_4 <- rawdata1_4[, c(11,12,4,5,7,8,9,3,6,10,2)]
colnames(rawdata1_4)[3:11] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Anxious_Obsessive_Total",
                                "Depressed_Withdrawal_Total", "Cruel_Total", "Total_Problems_Total")

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
sheet <- list("Boys_Age6_11" = rawdata1_1, "Boys_Age12_16" = rawdata1_2,
              "Girls_Age6_11" = rawdata1_3, "Girls_Age12_16" = rawdata1_4)
write.xlsx(sheet, file = "CCNPCKG_CBCL_Wave1.xlsx")

# save each participant's data into tsv file 
setwd(paste0(filefolder, "/DataArrange/CBCL"))
for (i in 1:nrow(rawdata1_1)) {
  data <- rawdata1_1[i,]
  dataname <- paste0("CCNPCKG", rawdata1_1[i, 1], "_ses-", rawdata1_1[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata1_2)) {
  data <- rawdata1_2[i,]
  dataname <- paste0("CCNPCKG", rawdata1_2[i, 1], "_ses-", rawdata1_2[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata1_3)) {
  data <- rawdata1_3[i,]
  dataname <- paste0("CCNPCKG", rawdata1_3[i, 1], "_ses-", rawdata1_3[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata1_4)) {
  data <- rawdata1_4[i,]
  dataname <- paste0("CCNPCKG", rawdata1_4[i, 1], "_ses-", rawdata1_4[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

###############################wave2
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPCKG_CBCL_wave2.xlsx"

#sheet1 男8-11
rawdata2_1 <- read.xlsx(rawfile, sheet = 1, rowNames = F)
rawdata2_1 <- rawdata2_1[, -124:-2]
colnames(rawdata2_1)[1] <- "ID"
rawdata2_1 <- merge(rawdata2_1, idcode, by = "ID")
rawdata2_1$Participant <- str_pad(rawdata2_1$Participant, 4, side = "left", "0")
rawdata2_1$Session <- "02"
rawdata2_1 <- rawdata2_1[, c(12,13,3,6,8,9,10,7,2,4,5,11)]
colnames(rawdata2_1)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                                "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                                "Schizoid_Total", "Uncommunicative_Total", "Obsessive_Total",
                                "Total_Problems_Total")

#sheet2 男12-16
rawdata2_2 <- read.xlsx(rawfile, sheet = 2, rowNames = F)
rawdata2_2 <- rawdata2_2[, -124:-2]
colnames(rawdata2_2)[1] <- "ID"
rawdata2_2 <- merge(rawdata2_2, idcode, by = "ID")
rawdata2_2$Participant <- str_pad(rawdata2_2$Participant, 4, side = "left", "0")
rawdata2_2$Session <- "02"
rawdata2_2 <- rawdata2_2[, c(12,13,2,3,5,8,9,4,6,7,10,11)]
colnames(rawdata2_2)[3:12] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Uncommunicative_Total",
                                "Obsessive_Total", "Hostile_Total", "Hyperactive_Total",
                                "Total_Problems_Total")

#sheet3 女8-11
rawdata2_3 <- read.xlsx(rawfile, sheet = 3, rowNames = F)
rawdata2_3 <- rawdata2_3[, -124:-2]
colnames(rawdata2_3)[1] <- "ID"
rawdata2_3 <- merge(rawdata2_3, idcode, by = "ID")
rawdata2_3$Participant <- str_pad(rawdata2_3$Participant, 4, side = "left", "0")
rawdata2_3$Session <- "02"
rawdata2_3 <- rawdata2_3[, c(12,13,2,4,6,9,8,3,5,7,10,11)]
colnames(rawdata2_3)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                                "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                                "Schizoid_Obsessive_Total", "Sex_Problems_Total", "Cruel_Total",
                                "Total_Problems_Total")

#sheet4 女12-16
rawdata2_4 <- read.xlsx(rawfile, sheet = 4, rowNames = F)
rawdata2_4 <- rawdata2_4[, -124:-2]
colnames(rawdata2_4)[1] <- "ID"
rawdata2_4 <- merge(rawdata2_4, idcode, by = "ID")
rawdata2_4$Participant <- str_pad(rawdata2_4$Participant, 4, side = "left", "0")
rawdata2_4$Session <- "02"
rawdata2_4 <- rawdata2_4[, c(11,12,3,4,6,7,8,2,5,9,10)]
colnames(rawdata2_4)[3:11] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Anxious_Obsessive_Total",
                                "Depressed_Withdrawal_Total", "Cruel_Total", "Total_Problems_Total")

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
sheet <- list("Boys_Age8_11" = rawdata2_1, "Boys_Age12_16" = rawdata2_2,
              "Girls_Age8_11" = rawdata2_3, "Girls_Age12_16" = rawdata2_4)
write.xlsx(sheet, file = "CCNPCKG_CBCL_Wave2.xlsx")

# save each participant's data into tsv file 
setwd(paste0(filefolder, "/DataArrange/CBCL"))
for (i in 1:nrow(rawdata2_1)) {
  data <- rawdata2_1[i,]
  dataname <- paste0("CCNPCKG", rawdata2_1[i, 1], "_ses-", rawdata2_1[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata2_2)) {
  data <- rawdata2_2[i,]
  dataname <- paste0("CCNPCKG", rawdata2_2[i, 1], "_ses-", rawdata2_2[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata2_3)) {
  data <- rawdata2_3[i,]
  dataname <- paste0("CCNPCKG", rawdata2_3[i, 1], "_ses-", rawdata2_3[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata2_4)) {
  data <- rawdata2_4[i,]
  dataname <- paste0("CCNPCKG", rawdata2_4[i, 1], "_ses-", rawdata2_4[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}

###############################wave3
setwd(paste0(filefolder, "/RawData/Source"))
rawfile <-  "CCNPCKG_CBCL_wave3.xlsx"

#sheet1 男8-11
rawdata3_1 <- read.xlsx(rawfile, sheet = 1, rowNames = F)
rawdata3_1 <- rawdata3_1[, -124:-2]
colnames(rawdata3_1)[1] <- "ID"
rawdata3_1 <- merge(rawdata3_1, idcode, by = "ID")
rawdata3_1$Participant <- str_pad(rawdata3_1$Participant, 4, side = "left", "0")
rawdata3_1$Session <- "03"
rawdata3_1 <- rawdata3_1[, c(12,13,3,6,8,9,10,7,2,4,5,11)]
colnames(rawdata3_1)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                                "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                                "Schizoid_Total", "Uncommunicative_Total", "Obsessive_Total",
                                "Total_Problems_Total")

#sheet2 男12-16
rawdata3_2 <- read.xlsx(rawfile, sheet = 2, rowNames = F)
rawdata3_2 <- rawdata3_2[, -124:-2]
colnames(rawdata3_2)[1] <- "ID"
rawdata3_2 <- merge(rawdata3_2, idcode, by = "ID")
rawdata3_2$Participant <- str_pad(rawdata3_2$Participant, 4, side = "left", "0")
rawdata3_2$Session <- "03"
rawdata3_2 <- rawdata3_2[, c(12,13,2,3,5,8,9,4,6,7,10,11)]
colnames(rawdata3_2)[3:12] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Uncommunicative_Total",
                                "Obsessive_Total", "Hostile_Total", "Hyperactive_Total",
                                "Total_Problems_Total")

#sheet3 女8-11
rawdata3_3 <- read.xlsx(rawfile, sheet = 3, rowNames = F)
rawdata3_3 <- rawdata3_3[, -124:-2]
colnames(rawdata3_3)[1] <- "ID"
rawdata3_3 <- merge(rawdata3_3, idcode, by = "ID")
rawdata3_3$Participant <- str_pad(rawdata3_3$Participant, 4, side = "left", "0")
rawdata3_3$Session <- "03"
rawdata3_3 <- rawdata3_3[, c(12,13,2,4,6,9,8,3,5,7,10,11)]
colnames(rawdata3_3)[3:12] <- c("Depressed_Total", "Somatic_Complaints_Total", "Hyperactive_Total",
                                "Aggressive_Totale", "Delinquent_Total", "Socia_Withdrawal_Total",
                                "Schizoid_Obsessive_Total", "Sex_Problems_Total", "Cruel_Total",
                                "Total_Problems_Total")

#sheet4 女12-16
rawdata3_4 <- read.xlsx(rawfile, sheet = 4, rowNames = F)
rawdata3_4 <- rawdata3_4[, -124:-2]
colnames(rawdata3_4)[1] <- "ID"
rawdata3_4 <- merge(rawdata3_4, idcode, by = "ID")
rawdata3_4$Participant <- str_pad(rawdata3_4$Participant, 4, side = "left", "0")
rawdata3_4$Session <- "03"
rawdata3_4 <- rawdata3_4[, c(11,12,4,5,7,8,9,3,6,10,2)]
colnames(rawdata3_4)[3:11] <- c("Somatic_Complaints_Total", "Schizoid_Total", "Immature_Total",
                                "Delinquent_Total", "Aggressive_Totale", "Anxious_Obsessive_Total",
                                "Depressed_Withdrawal_Total", "Cruel_Total", "Total_Problems_Total")

# save a copy of raw data
setwd(paste0(filefolder, "/RawData/"))
sheet <- list("Boys_Age8_11" = rawdata3_1, "Boys_Age12_16" = rawdata3_2,
              "Girls_Age8_11" = rawdata3_3, "Girls_Age12_16" = rawdata3_4)
write.xlsx(sheet, file = "CCNPCKG_CBCL_Wave3.xlsx")

# save each participant's data into tsv file 
setwd(paste0(filefolder, "/DataArrange/CBCL"))
for (i in 1:nrow(rawdata3_1)) {
  data <- rawdata3_1[i,]
  dataname <- paste0("CCNPCKG", rawdata3_1[i, 1], "_ses-", rawdata3_1[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata3_2)) {
  data <- rawdata3_2[i,]
  dataname <- paste0("CCNPCKG", rawdata3_2[i, 1], "_ses-", rawdata3_2[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata3_3)) {
  data <- rawdata3_3[i,]
  dataname <- paste0("CCNPCKG", rawdata3_3[i, 1], "_ses-", rawdata3_3[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
for (i in 1:nrow(rawdata3_4)) {
  data <- rawdata3_4[i,]
  dataname <- paste0("CCNPCKG", rawdata3_4[i, 1], "_ses-", rawdata3_4[i, 2], "_task-CBCL_beh.tsv")
  write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
}
