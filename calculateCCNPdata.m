% this code is used to calculate CCNP questionnaires' score
% copyright: Xue-Ru Fan @BNU, 27 Dec 2022

filefolder = 'F:\CCNPdataArrange\Scales\RawData';

%% C-PDS
cd(filefolder)
data = readtable('CCNPPEK_C-PDS_raw.xlsx')
