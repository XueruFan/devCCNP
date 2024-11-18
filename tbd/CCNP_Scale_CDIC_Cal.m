% 本代码计算彩巢儿童抑郁量表的得分用于制作报告
clear;clc;
dataDir='/Users/xuerufan/Desktop/问卷录入/';
resultDir = "/Users/xuerufan/Desktop/流水线制作材料/材料准备/";
%% load file
cd(dataDir)
[data,txt,raw] = xlsread('CCNPPEK_Scale_B_All_ver2.xlsx');
%% questionnaire information
questionnaire_name = 'P1Q4S';
inver_list = [2 5 7 8 10 11 13 15 16 18 21 24 25]; % 反向计分题号
dimension = struct('D1',[4 16 17 18 19 20 21 22],'D2',[1 6 8 9 10 11 13],...
    'D3',[2 7 14 25],'D4',[3 15 23 24],'D5',[5 12 26 27]);
%1快感缺乏;2负性情绪;3低自尊;4效能低下;5人际问题
%% reformat
pos = find(contains(raw(1,:),questionnaire_name)); % 找到该问卷在原始数据的哪些列
raw_scores = cell2mat(raw(3:end,pos)); % 从第三行开始，因为第一二行是列名
raw_scores = raw_scores-1; %问卷计分方范围为0-2，录入为1-3，需在原始分基础上减1才能进行后续计算
inversed_scores = 2-raw_scores(:,inver_list);
new_scores = raw_scores;
new_scores(:,inver_list) = inversed_scores;
%% calculate dimension scores
nsub=size(new_scores,1);
dimension_list=fieldnames(dimension);
dimension_scores=zeros(nsub,6);
for i=1:nsub
    for j=1:5
        d=dimension_list{j};
        nan_num=numel(find(isnan(new_scores(i,dimension.(d)))));
        if nan_num==0
            dimension_scores(i,j)=sum(new_scores(i,dimension.(d)));
        elseif nan_num>0 && nan_num< 27 %存在缺失值且并非所有值都缺失的条件下
            m=nanmean(new_scores(i,dimension.(d)));
            s=m.*length(dimension.(d));
            dimension_scores(i,j)=round(s);%利用均值代替缺失值，并四舍五入
        elseif nan_num==27
            dimension_scores(i,j)=NaN;
        end
    end
    dimension_scores(i,6)=sum(dimension_scores(i,1:5));
end
%% save data
title={'快感缺乏','负性情绪','低自尊','效能低下','人际问题','总分'};
dimension_scores=num2cell(dimension_scores);
all_data=[title;dimension_scores];
all_data=[raw([1 3:end],2),all_data];
cd(resultDir)
writecell(all_data,'CCNPPEK_CDIC_report.xlsx')