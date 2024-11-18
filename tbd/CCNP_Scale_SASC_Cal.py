# this script is used to arrange CCNP SASC result
# encoding: utf-8
import os
import pandas as pd # 导入pandas库，并将其简称为pd，方便后续代码调用库中的函数和方法
# DataFrame是pandas中的一个核心数据结构"

os.chdir("/Users/xuerufan/Desktop/问卷录入")
df = pd.read_excel('CCNPPEK_Scale_B_All.xlsx')
# df=df.iloc[:,[0,1,113,114,115,116,117,118,119,120,121,122]]
sasc = df[["FID", "P1Q2S01", "P1Q2S02", "P1Q2S03","P1Q2S04", "P1Q2S05", "P1Q2S06", "P1Q2S07", "P1Q2S08","P1Q2S09", "P1Q2S10"]]

sasc.dropna(axis = 0, how = 'any', inplace = True)
# 使用dropna方法移除sasc中含有任何空值的行
# 参数axis=0指定了操作的轴线是行，how='any'意味着只要行中有任何一个空值就将其删除
# inplace=True表示直接在原数据上进行修改，而不是创建一个新的DataFrame

del df

# A=0; B=1; C=2
sasc['害怕否定'] = sasc.iloc[:,[i for i in [1,2,5,6,8,10]]].apply(pd.to_numeric, errors='coerce').sum(axis=1)
sasc['害怕否定'] = sasc.iloc[1:, [-1]]-6
sasc['社交回避及痛苦'] = sasc.iloc[:, [i for i in [3, 4, 7, 9]]].apply(pd.to_numeric, errors='coerce').sum(axis=1)
sasc['社交回避及痛苦'] = sasc.iloc[1:, [-1]]-4
sasc['社交焦虑总分'] = sasc.iloc[:, 1:11].apply(pd.to_numeric, errors='coerce').sum(axis=1)
sasc['社交焦虑总分'] = sasc.iloc[1:, [-1]]-10

sasc = sasc.drop(sasc.index[0]) # 删除了处理后的数据中的第一行

os.chdir("/Users/xuerufan/Desktop/流水线制作材料/材料准备")
sasc.iloc[:, [0, -3, -2, -1]].to_excel('CCNPPEK_SASC_report.xlsx', index=False)
# 这行代码将处理后的数据（只包含特定列）导出到一个新的Excel文件中
# iloc用于选取数据框的行和列，这里选取了第一、第二以及最后三列
# to_excel函数用于将这些数据写入一个新的Excel文间中
# index=False表示在写入时不包含行索引



