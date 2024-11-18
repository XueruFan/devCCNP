% 本代码计算彩巢特质焦虑的得分用于制作报告
clear;clc;
dataDir='/Users/xuerufan/Desktop/问卷录入/';
resultDir = "/Users/xuerufan/Desktop/流水线制作材料/材料准备/";
%% load file
cd(dataDir)
[data,txt,raw] = xlsread('CCNPPEK_Scale_B_All_ver2.xlsx');
%% questionnaire information
questionnaire_name='P1Q3S';
inver_list=[1 2 5 8 10 11 15 16 19 20 21 23 24 26 27 30 33 34 36 39];
%% reformat
pos=find(contains(raw(1,:),questionnaire_name));
raw_scores=cell2mat(raw(3:end,pos));
inversed_scores=5-raw_scores(:,inver_list);%分数反向，分数范围为1-4
new_scores=raw_scores;
new_scores(:,inver_list)=inversed_scores;
%% calculate anxiety scores
nsub=size(new_scores,1);
dimension_scores=zeros(nsub,1);%家长反馈只需要特质焦虑，题号为21-40 
for i=1:nsub
    nan_num=numel(find(isnan(new_scores(i,21:40))));
    if nan_num==0
        dimension_scores(i)=sum(new_scores(i,21:40));
    elseif nan_num>0 && nan_num<20
        m=nanmean(new_scores(i,21:40));
        s=m.*20;
        dimension_scores(i)=round(s);%利用均值代替缺失值，并四舍五入
    elseif nan_num==20
        dimension_scores(i)=NaN;
    end
end
%% save data
score_ratio=dimension_scores./80.*100;% 问卷总分为80.
title={'特质焦虑','总分占比'};
dimension_scores=num2cell([dimension_scores score_ratio]);
all_data=[title;dimension_scores];
all_data=[raw([1 3:end],2),all_data];
cd(resultDir)
writecell(all_data,'CCNPPEK_TAI_report.xlsx')