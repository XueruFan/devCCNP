# 本代码用来实现计算彩巢数据中IPPA得分
# 范雪如 2024年1月26日

rm(list=ls())


dataDir <- '//172.16.191.42/home/项目管理/CCNP/数据规范化预处理/问卷量表/scale'
resultDir <- '//172.16.191.42/home/项目管理/CCNP/报告制作/Batch4'

setwd(dataDir)

packages <- c("writexl", "tidyr", "reshape2", "openxlsx", "dplyr", "plyr", "stringr", "do")
# sapply(packages, install.packages, character.only = TRUE)
sapply(packages, require, character.only = TRUE)

IPPA <- read.xlsx(file.path(dataDir, "CCNPPEK_IPPA_Batch1234.xlsx"), rowNames = F, colNames = T)


# 在报告中里展示的不是被试原始得分，而是得分占总分（该量表或维度的最大得分）的百分比

# 父亲、母亲和同伴三个子量表的子维度
IPPA$Father_Trust_Per <- scales::percent(IPPA$Father_Trust/(10*5), 0.01)
IPPA$Father_Communication_Per <- scales::percent(IPPA$Father_Communication/(9*5), 0.01)
IPPA$Father_Alienation_Per <- scales::percent(IPPA$Father_Alienation/(6*5), 0.01)
IPPA$Mother_Trust_Per <- scales::percent(IPPA$Mother_Trust/(10*5), 0.01)
IPPA$Mother_Communication_Per <- scales::percent(IPPA$Mother_Communication/(9*5), 0.01)
IPPA$Mother_Alienation_Per <- scales::percent(IPPA$Mother_Alienation/(6*5), 0.01)
IPPA$Peer_Trust_Per <- scales::percent(IPPA$Peer_Trust/(10), 0.01)
IPPA$Peer_Communication_Per <- scales::percent(IPPA$Peer_Communication/(8*5), 0.01)
IPPA$Peer_Alienation_Per <- scales::percent(IPPA$Peer_Alienation/(7*5), 0.01)

# 父亲、母亲和同伴三个子量表的总分
IPPA$Father_Per <- scales::percent(IPPA$Father/(25*5), 0.01)
IPPA$Mother_Per <- scales::percent(IPPA$Mother/(25*5), 0.01)
IPPA$Peer_Per <- scales::percent(IPPA$Peer/(25*5), 0.01)

# save result for making report
write.xlsx(IPPA[, c(1,2,15:ncol(IPPA))], file.path(resultDir, "CCNPPEK_IPPA_report.xlsx"),
           rowNames = F, colNames = T)

