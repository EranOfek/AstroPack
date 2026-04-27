function [Result] = compareVersions(Dir0, Dir1, Args)
    % compare 2 versions of LAST pipeline output
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2026 Apr) 
    % Example: Dir0 = '/mnt/archimedes/LASTunitTest/2025/05/03/proc/215711v0/';
    %          Dir1 = '/mnt/archimedes/LASTunitTest/2025/05/03/proc/215701v1/';
    %          Res = pipeline.last.quality.compareVersions(Dir0, Dir1);
    arguments
        Dir0
        Dir1
        Args.CoaddOnly         = false        
    end
    % load the data:
    if Args.CoaddOnly
        [AllSI0, Coadd0, MS0] = pipeline.last.load.loadVisit(Dir0,'TempName_IndivIm',[],'TempName_MS',[]);
        [AllSI1, Coadd1, MS1] = pipeline.last.load.loadVisit(Dir1,'TempName_IndivIm',[],'TempName_MS',[]);        
    else
        [AllSI0, Coadd0, MS0] = pipeline.last.load.loadVisit(Dir0);
        [AllSI1, Coadd1, MS1] = pipeline.last.load.loadVisit(Dir1);
    end
    
end
