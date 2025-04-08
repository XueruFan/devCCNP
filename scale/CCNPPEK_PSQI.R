# this code is used to arrange CCNPPEK PSQI result into tsv file
# copyright: Xue-Ru Fan @BNU, 29 Dec 2022

# clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(do)


################ 以下这部分是处理Batch1234的代码 ######################################################

# define environment variables
filefolder <- "//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表"
setwd(filefolder)

#################### arrange raw data 
# load raw data file
setwd(file.path(filefolder, "source"))
rawfile <-  "CCNPPEK_Scale_B_Batch1234.xlsx"
rawdata <- read.xlsx(rawfile, rowNames = F)
rawdata <- rawdata[, -1:-2]


######################## Arrange PSQI result #######################################################


psqi <- rawdata[, c(1,2, 16:33)]
# psqi <- psqi[-1,]

# save a copy of raw data
setwd(file.path(filefolder, "raw"))
write.xlsx(psqi[-1,], "CCNPPEK_PSQI_Batch1234_raw.xlsx", rowNames = F, colNames = T)

# calculate  score for 7 factors
####### 1 主观睡眠质量
SubjectiveSleepQuality <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SubjectiveSleepQuality[i, 1] <- psqi[i, 1]
  SubjectiveSleepQuality[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i,17])
  SubjectiveSleepQuality[i, 3] <- score - 1
}
SubjectiveSleepQuality <- SubjectiveSleepQuality[-1,]
colnames(SubjectiveSleepQuality) <- c("Participant", "Session", "Subjective Sleep Quality")
####### 2 入睡时间
SleepLatency <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SleepLatency[i, 1] <- psqi[i, 1]
  SleepLatency[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i, c(4, 7)])
  scores <- score[1] + score[2] - 2
  if (is.na(scores) == 1) {
    next
  } else if (scores == 0) {
    SleepLatency[i, 3] = 0
  } else if (scores == 1 | scores == 2) {
    SleepLatency[i, 3] = 1
  } else if (scores == 3 | scores == 4) {
    SleepLatency[i, 3] = 2
  } else if (scores == 5 | scores == 6) {
    SleepLatency[i, 3] = 3
  }
}
SleepLatency <- SleepLatency[-1,]
colnames(SleepLatency) <- c("Participant", "Session", "Sleep Latency")
####### 3 睡眠时间
SleepDuration <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SleepDuration[i, 1] <- psqi[i, 1]
  SleepDuration[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i,6])
  SleepDuration[i, 3] <- score - 1
}
SleepDuration <- SleepDuration[-1,]
colnames(SleepDuration) <- c("Participant", "Session", "Sleep Duration")
####### 4 睡眠效率(填写不规范的数据无法计算出效率)
SleepEfficiency <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SleepEfficiency[i, 1] <- psqi[i, 1]
  SleepEfficiency[i, 2] <- psqi[i, 2]
  score <- psqi[i, c(5, 3, 6)]
  # calculate duration in the bed
  getup <- as.numeric(score[1])
  if (is.numeric(getup) == 1) {
    getup_hrs <- (getup-floor(getup))/0.6+floor(getup)
  }
  gotobed <- as.numeric(score[2])
  if (is.na(gotobed) ==1) {
    next
  }else if (is.numeric(gotobed) == 1) {
    gotobed_hrs <- (gotobed-floor(gotobed))/0.6+floor(gotobed)
    if (gotobed_hrs <= 12) {
      hrs <- 12 - gotobed_hrs
    } else if (gotobed_hrs > 12){
      hrs <- 24 - gotobed_hrs
    }
  }
  H <- getup_hrs + hrs #床上时间
  # apply sleeping hours??(彩巢给了选项，每个选项是一个范围，无法计算，因此这里统一将范围变成数字)
  if (score[3] == 1) {
    S = 7.5 #睡眠时间
  } else if (score[3] == 2) {
    S = 6.5
  } else if (score[3] == 3) {
    S = 5.5
  } else if (score[3] == 4) {
    S = 4.5
  }
  E <- S/H #睡眠效率
  if (is.na(E) == 1) {
    next
  } else if (E>=0.85) {
    SleepEfficiency[i, 3] <- 0
  } else if (E>=0.75 & E<0.85) {
    SleepEfficiency[i, 3] <- 1
  } else if (E>=0.65 & E<0.75) {
    SleepEfficiency[i, 3] <- 2
  } else if (E<0.65) {
    SleepEfficiency[i, 3] <- 3
  }
}
SleepEfficiency <- SleepEfficiency[-1,]
colnames(SleepEfficiency) <- c("Participant", "Session", "Sleep Efficiency")
####### 5 睡眠障碍
SleepDisturbance <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SleepDisturbance[i, 1] <- psqi[i, 1]
  SleepDisturbance[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i, 8:16])
  scores <- sum(score) - 9
  if (is.na(scores) == 1) {
    next
  } else if (scores == 0) {
    SleepDisturbance[i, 3] <- 0
  } else if (scores>=1 & scores<=9) {
    SleepDisturbance[i, 3] <- 1
  } else if (scores>=10 & scores<=18) {
    SleepDisturbance[i, 3] <- 2
  } else if (scores>=19 & scores<=27) {
    SleepDisturbance[i, 3] <- 3
  }
}
SleepDisturbance <- SleepDisturbance[-1,]
colnames(SleepDisturbance) <- c("Participant", "Session", "Sleep Disturbance")
####### 6 催眠药物的使用
SleepMedication <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  SleepMedication[i, 1] <- psqi[i, 1]
  SleepMedication[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i,18])
  SleepMedication[i, 3] <- score - 1
}
SleepMedication <- SleepMedication[-1,]
colnames(SleepMedication) <- c("Participant", "Session", "Use of Sleep Medication")
###### 7 日间功能障碍
DayTimeDis <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 2:nrow(psqi)) {
  DayTimeDis[i, 1] <- psqi[i, 1]
  DayTimeDis[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i, 19:20])
  scores <- sum(score) - 2
  if (is.na(scores) == 1) {
    next
  } else if (scores == 0) {
    DayTimeDis[i, 3] <- 0
  } else if (scores == 1 | scores == 2) {
    DayTimeDis[i, 3] <- 1
  } else if (scores == 3 | scores == 4) {
    DayTimeDis[i, 3] <- 2
  } else if (scores == 5 | scores == 6) {
    DayTimeDis[i, 3] <- 3
  }
}
DayTimeDis <- DayTimeDis[-1,]
colnames(DayTimeDis) <- c("Participant", "Session", "Day-time Disfunction")

# put all the seven scores together
# colnames(psqi) <- psqi[1,]
psqi <- psqi[-1,]
psqi <- merge(psqi, SubjectiveSleepQuality, by = c("Participant", "Session"))
psqi <- merge(psqi, SleepLatency, by = c("Participant", "Session"))
psqi <- merge(psqi, SleepDuration, by = c("Participant", "Session"))
psqi <- merge(psqi, SleepEfficiency, by = c("Participant", "Session"))
psqi <- merge(psqi, SleepDisturbance, by = c("Participant", "Session"))
psqi <- merge(psqi, SleepMedication, by = c("Participant", "Session"))
psqi <- merge(psqi, DayTimeDis, by = c("Participant", "Session"))

# calculate total score AKA.PSQI
Total <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:nrow(psqi)) {
  Total[i, 1] <- psqi[i, 1]
  Total[i, 2] <- psqi[i, 2]
  score <- as.numeric(psqi[i, 21:27])
  Total[i, 3] <- sum(score)
}
colnames(Total) <- c("Participant", "Session", "Total")
psqi <- merge(psqi, Total, by = c("Participant", "Session"))
#去掉全部数据都缺失的被试
nan <- vector()
for (i in 1:nrow(psqi)) {
  if (is.na(psqi[i, 21]) == 1 & is.na(psqi[i, 22]) == 1 & is.na(psqi[i, 23]) == 1 &
      is.na(psqi[i, 24]) == 1 & is.na(psqi[i, 25]) == 1 & is.na(psqi[i, 26]) == 1 &
      is.na(psqi[i, 27]) == 1) {
    nan <- c(nan,i)
  } else {
    nan <- nan
  }
}
psqi <- psqi[-c(nan),]
psqi <- psqi[, c(1,2,21:28)]

# save finial result of this scale
setwd(file.path(filefolder, "scale"))
write.xlsx(psqi, "CCNPPEK_PSQI_Batch1234.xlsx", rowNames = F, colNames = T)


################ 以下这部分是处理Batch123的代码 ######################################################


# # define environment variables
# filefolder <- "F:/CCNPdataArrange/Scales"
# setwd(filefolder)
# 
# #################### arrange raw data ##############################################################
# 
# # load raw data file
# setwd(paste0(filefolder, "/RawData/Source"))
# rawfile <-  "CCNPPEK_Questionnaires.xlsx"
# rawdata <- read.xlsx(rawfile, rowNames = F)
# rawdata <- rawdata[, -1]
# 
# # pick out data from each wave
# raw_ses2 <- rawdata[grep("-W2$", rawdata[, 1]),]
# raw_ses3 <- rawdata[grep("-W3$", rawdata[, 1]),]
# raw_ses1 <- setdiff(setdiff(rawdata, raw_ses2), raw_ses3)
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
# ######################## Arrange PSQI result #######################################################
# 
# psqi <- raw[, c(1,2, 241:258)]
# 
# # save a copy of raw data
# setwd(paste0(filefolder, "/RawData/"))
# write.xlsx(psqi, "CCNPPEK_PSQI_raw.xlsx", rowNames = F, colNames = F)
# 
# # calculate  score for 7 factors
# ####### 1 主观睡眠质量
# SubjectiveSleepQuality <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SubjectiveSleepQuality[i, 1] <- psqi[i, 1]
#   SubjectiveSleepQuality[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i,17])
#   SubjectiveSleepQuality[i, 3] <- score - 1
# }
# SubjectiveSleepQuality <- SubjectiveSleepQuality[-1,]
# colnames(SubjectiveSleepQuality) <- c("Participant", "Session", "Subjective Sleep Quality")
# ####### 2 入睡时间
# SleepLatency <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SleepLatency[i, 1] <- psqi[i, 1]
#   SleepLatency[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i, c(4, 7)])
#   scores <- score[1] + score[2] - 2
#   if (is.na(scores) == 1) {
#     next
#   } else if (scores == 0) {
#     SleepLatency[i, 3] = 0
#   } else if (scores == 1 | scores == 2) {
#     SleepLatency[i, 3] = 1
#   } else if (scores == 3 | scores == 4) {
#     SleepLatency[i, 3] = 2
#   } else if (scores == 5 | scores == 6) {
#     SleepLatency[i, 3] = 3
#   }
# }
# SleepLatency <- SleepLatency[-1,]
# colnames(SleepLatency) <- c("Participant", "Session", "Sleep Latency")
# ####### 3 睡眠时间
# SleepDuration <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SleepDuration[i, 1] <- psqi[i, 1]
#   SleepDuration[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i,6])
#   SleepDuration[i, 3] <- score - 1
# }
# SleepDuration <- SleepDuration[-1,]
# colnames(SleepDuration) <- c("Participant", "Session", "Sleep Duration")
# ####### 4 睡眠效率(填写不规范的数据无法计算出效率)
# SleepEfficiency <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SleepEfficiency[i, 1] <- psqi[i, 1]
#   SleepEfficiency[i, 2] <- psqi[i, 2]
#   score <- psqi[i, c(5, 3, 6)]
#   # calculate duration in the bed
#   getup <- as.numeric(score[1])
#   if (is.numeric(getup) == 1) {
#     getup_hrs <- (getup-floor(getup))/0.6+floor(getup)
#   }
#   gotobed <- as.numeric(score[2])
#   if (is.na(gotobed) ==1) {
#     next
#   }else if (is.numeric(gotobed) == 1) {
#     gotobed_hrs <- (gotobed-floor(gotobed))/0.6+floor(gotobed)
#     if (gotobed_hrs <= 12) {
#       hrs <- 12 - gotobed_hrs
#     } else if (gotobed_hrs > 12){
#       hrs <- 24 - gotobed_hrs
#     }
#   }
#   H <- getup_hrs + hrs #床上时间
#   # apply sleeping hours??(彩巢给了选项，每个选项是一个范围，无法计算，因此这里统一将范围变成数字)
#   if (score[3] == 1) {
#     S = 7.5 #睡眠时间
#   } else if (score[3] == 2) {
#     S = 6.5
#   } else if (score[3] == 3) {
#     S = 5.5
#   } else if (score[3] == 4) {
#     S = 4.5
#   }
#   E <- S/H #睡眠效率
#   if (is.na(E) == 1) {
#     next
#   } else if (E>=0.85) {
#     SleepEfficiency[i, 3] <- 0
#   } else if (E>=0.75 & E<0.85) {
#     SleepEfficiency[i, 3] <- 1
#   } else if (E>=0.65 & E<0.75) {
#     SleepEfficiency[i, 3] <- 2
#   } else if (E<0.65) {
#     SleepEfficiency[i, 3] <- 3
#   }
# }
# SleepEfficiency <- SleepEfficiency[-1,]
# colnames(SleepEfficiency) <- c("Participant", "Session", "Sleep Efficiency")
# ####### 5 睡眠障碍
# SleepDisturbance <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SleepDisturbance[i, 1] <- psqi[i, 1]
#   SleepDisturbance[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i, 8:16])
#   scores <- sum(score) - 9
#   if (is.na(scores) == 1) {
#     next
#   } else if (scores == 0) {
#     SleepDisturbance[i, 3] <- 0
#   } else if (scores>=1 & scores<=9) {
#     SleepDisturbance[i, 3] <- 1
#   } else if (scores>=10 & scores<=18) {
#     SleepDisturbance[i, 3] <- 2
#   } else if (scores>=19 & scores<=27) {
#     SleepDisturbance[i, 3] <- 3
#   }
# }
# SleepDisturbance <- SleepDisturbance[-1,]
# colnames(SleepDisturbance) <- c("Participant", "Session", "Sleep Disturbance")
# ####### 6 催眠药物的使用
# SleepMedication <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   SleepMedication[i, 1] <- psqi[i, 1]
#   SleepMedication[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i,18])
#   SleepMedication[i, 3] <- score - 1
# }
# SleepMedication <- SleepMedication[-1,]
# colnames(SleepMedication) <- c("Participant", "Session", "Use of Sleep Medication")
# ###### 7 日间功能障碍
# DayTimeDis <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 2:nrow(psqi)) {
#   DayTimeDis[i, 1] <- psqi[i, 1]
#   DayTimeDis[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i, 19:20])
#   scores <- sum(score) - 2
#   if (is.na(scores) == 1) {
#     next
#   } else if (scores == 0) {
#     DayTimeDis[i, 3] <- 0
#   } else if (scores == 1 | scores == 2) {
#     DayTimeDis[i, 3] <- 1
#   } else if (scores == 3 | scores == 4) {
#     DayTimeDis[i, 3] <- 2
#   } else if (scores == 5 | scores == 6) {
#     DayTimeDis[i, 3] <- 3
#   }
# }
# DayTimeDis <- DayTimeDis[-1,]
# colnames(DayTimeDis) <- c("Participant", "Session", "Day-time Disfunction")
# 
# # put all the seven scores together
# colnames(psqi) <- psqi[1,]
# psqi <- psqi[-1,]
# psqi <- merge(psqi, SubjectiveSleepQuality, by = c("Participant", "Session"))
# psqi <- merge(psqi, SleepLatency, by = c("Participant", "Session"))
# psqi <- merge(psqi, SleepDuration, by = c("Participant", "Session"))
# psqi <- merge(psqi, SleepEfficiency, by = c("Participant", "Session"))
# psqi <- merge(psqi, SleepDisturbance, by = c("Participant", "Session"))
# psqi <- merge(psqi, SleepMedication, by = c("Participant", "Session"))
# psqi <- merge(psqi, DayTimeDis, by = c("Participant", "Session"))
# 
# # calculate total score AKA.PSQI
# Total <- data.frame(matrix(ncol = 3, nrow = 0))
# for (i in 1:nrow(psqi)) {
#   Total[i, 1] <- psqi[i, 1]
#   Total[i, 2] <- psqi[i, 2]
#   score <- as.numeric(psqi[i, 21:27])
#   Total[i, 3] <- sum(score)
# }
# colnames(Total) <- c("Participant", "Session", "Total")
# psqi <- merge(psqi, Total, by = c("Participant", "Session"))
# #去掉全部数据都缺失的被试
# nan <- vector()
# for (i in 1:nrow(psqi)) {
#   if (is.na(psqi[i, 21]) == 1 & is.na(psqi[i, 22]) == 1 & is.na(psqi[i, 23]) == 1 &
#       is.na(psqi[i, 24]) == 1 & is.na(psqi[i, 25]) == 1 & is.na(psqi[i, 26]) == 1 &
#       is.na(psqi[i, 27]) == 1) {
#     nan <- c(nan,i)
#   } else {
#     nan <- nan
#   }
# }
# psqi <- psqi[-c(nan),]
# psqi <- psqi[, c(1,2,21:28)]
# 
# # save finial result of this scale
# setwd(paste0(filefolder, "/DataArrange/"))
# write.xlsx(psqi, "CCNPPEK_PSQI.xlsx", rowNames = F, colNames = T)
# 
# ############# save each participant's data into tsv file ###########################################
# setwd(paste0(filefolder, "/DataArrange/PSQI"))
# for (i in 1:nrow(psqi)) {
#   data <- psqi[i,]
#   dataname <- paste0("CCNPPEK", psqi[i, 1], "_ses-", psqi[i, 2], "_task-PSQI_beh.tsv")
#   write.table(data, dataname,  sep = "\t",  row.names = F, col.names = T, quote = F,)
# }