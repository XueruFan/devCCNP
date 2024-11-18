% ���������ʳ���ͯ��������ĵ÷�������������
clear;clc;
dataDir='/Users/xuerufan/Desktop/�ʾ�¼��/';
resultDir = "/Users/xuerufan/Desktop/��ˮ����������/����׼��/";
%% load file
cd(dataDir)
[data,txt,raw] = xlsread('CCNPPEK_Scale_B_All_ver2.xlsx');
%% questionnaire information
questionnaire_name = 'P1Q4S';
inver_list = [2 5 7 8 10 11 13 15 16 18 21 24 25]; % ����Ʒ����
dimension = struct('D1',[4 16 17 18 19 20 21 22],'D2',[1 6 8 9 10 11 13],...
    'D3',[2 7 14 25],'D4',[3 15 23 24],'D5',[5 12 26 27]);
%1���ȱ��;2��������;3������;4Ч�ܵ���;5�˼�����
%% reformat
pos = find(contains(raw(1,:),questionnaire_name)); % �ҵ����ʾ���ԭʼ���ݵ���Щ��
raw_scores = cell2mat(raw(3:end,pos)); % �ӵ����п�ʼ����Ϊ��һ����������
raw_scores = raw_scores-1; %�ʾ�Ʒַ���ΧΪ0-2��¼��Ϊ1-3������ԭʼ�ֻ����ϼ�1���ܽ��к�������
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
        elseif nan_num>0 && nan_num< 27 %����ȱʧֵ�Ҳ�������ֵ��ȱʧ��������
            m=nanmean(new_scores(i,dimension.(d)));
            s=m.*length(dimension.(d));
            dimension_scores(i,j)=round(s);%���þ�ֵ����ȱʧֵ������������
        elseif nan_num==27
            dimension_scores(i,j)=NaN;
        end
    end
    dimension_scores(i,6)=sum(dimension_scores(i,1:5));
end
%% save data
title={'���ȱ��','��������','������','Ч�ܵ���','�˼�����','�ܷ�'};
dimension_scores=num2cell(dimension_scores);
all_data=[title;dimension_scores];
all_data=[raw([1 3:end],2),all_data];
cd(resultDir)
writecell(all_data,'CCNPPEK_CDIC_report.xlsx')