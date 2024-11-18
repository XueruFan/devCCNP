clear, clc
datafolder = 'C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入/自处理数据/任务转换原始文件/'; 
addpath(genpath(datafolder))
cd(datafolder)

[txt,raw] = xlsread('CCNPPEK_TaskSwitch_Arr.xlsx');

data = raw(2:end,:);
dataid = str2num(char(data(:, 1))); % 被试编号
datases = str2num(char(data(:, 2))); % 被试轮次
acc = str2num(char(data(:, 3)));
switchcost = str2num(char(data(:, 4)));

psubacc = zeros(length(dataid), 1);
psubswitchcost =psubacc;

for i=1:length(dataid)
    
    pacc = 1-sum(acc(i)<acc)/(length(acc)-1);
    psubacc(i) = pacc;
    
    pswitchcost = 1-sum(switchcost(i)>switchcost)/(length(switchcost)-1);
    psubswitchcost(i) = pswitchcost;
    
end

TSresults = [dataid, datases, acc, switchcost, psubacc, psubswitchcost];
TSresults2 = ["subj", "sess", "acc", "time", "acc_perc", "time_perc"; TSresults];

xlswrite('CCNPPEK_TaskSwitch_report.xlsx', TSresults2, "Sheet1")  