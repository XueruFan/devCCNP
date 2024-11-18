% 本代码计算彩巢自我意识的得分用于制作报告
clear;clc;
dataDir= "C:/系统文件/工作管理/项目管理/彩巢计划/数据相关/问卷录入/";
resultDir = "C:/系统文件/工作管理/项目管理/彩巢计划/报告制作/报告流水线制作/材料准备/";
%% load file
cd(dataDir)
[data,txt,raw] = xlsread('CCNPPEK_Scale_B_All_ver2.xlsx');
%% questionnaire information
questionnaire_name='P1Q1';
inver_list=[1 3 4 6 7 8 10 11 13 14 18 20 22 25 26 28 31 34 37 38 40 43 45 46 47 48 50  ...
    53 56 58 59 61 62 64 65 66 68 71 74 75 77 78 79];
dimension=struct('D1',[12 13 14 21 22 25 34 35 38 45 48 56 59 62 78 80],...
    'D2',[5 7 9 12 16 17 21 26 27 30 31 33 42 49 53 66 70],...
    'D3',[5 8 15 29 33 41 49 54 57 60 63 69 73],...
    'D4',[4 6 7 8 10 20 28 37 39 40 43 50 74 79],...
    'D5',[1 3 6 11 40 46 49 51 58 65 69 77],...
    'D6',[2 8 36 39 43 50 52 60 67 80],...
    'D7',[1:80]);%1行为;2智力与学校情况;3躯体外貌;4焦虑;5合群;6幸福与满足;7总分
%% reformat
pos=find(contains(raw(1,:),questionnaire_name));
raw_scores=cell2mat(raw(3:end,pos));
raw_scores(raw_scores==2)=0;%问卷计分应为是1否0，录入为是1否2，需在原始分基础上更改
inversed_scores=raw_scores(:,inver_list)+1;
inversed_scores(inversed_scores==2)=0;
new_scores=raw_scores;
new_scores(:,inver_list)=inversed_scores;
%% calculate dimension scores
nsub=size(new_scores,1);
dimension_list=fieldnames(dimension);
dimension_scores=zeros(nsub,7);
for i=1:nsub
    for j=1:7
        d=dimension_list{j};
        nan_num=numel(find(isnan(new_scores(i,dimension.(d))))); 
        nEntries = size(dimension.(d),2); %分维度的题目数
        if nan_num==0
            dimension_scores(i,j)=sum(new_scores(i,dimension.(d)));
        elseif nan_num<nEntries %存在缺失值且缺失值小于分维度题目数的一半
            m=nanmean(new_scores(i,dimension.(d)));
            s=m.*length(dimension.(d));
            dimension_scores(i,j)=round(s);%利用均值代替缺失值，并四舍五入
        elseif nan_num>nEntries %缺失值超过分维度题目数的一半
            dimension_scores(i,j)=NaN;
        end
    end
end
%% save data
title={'行为','智力与学校情况','躯体外貌','焦虑','合群','幸福与满足','总分'};
dimension_scores=num2cell(dimension_scores);
all_data=[title;dimension_scores];
all_data=[raw([1 3:end],2),all_data];
cd(resultDir)
writecell(all_data,'CCNPPEK_PHCSS_report_new.xlsx')