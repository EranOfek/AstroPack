function [Result] = compareVersions(Dir0, Dir1, Args)
    % compare 2 versions of LAST pipeline output
    %     Optional detailed description
    % Input  : - output visit catalog of pipeline version 0
    %          - output visit catalog of pipeline version 1
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2026 Apr) 
    % Example: Dir0 = '/mnt/archimedes/LASTunitTest/2025/05/03/proc/215711v0/';
    %          Dir1 = '/mnt/archimedes/LASTunitTest/2025/05/03/proc/215701v1/';
    %          Res = pipeline.last.quality.compareVersions(Dir0, Dir1);
    arguments
        Dir0
        Dir1
        Args.CoaddOnly  = false       
        
        Args.PropNew    = {'RA', 'Dec', 'XPEAK', 'YPEAK', 'X1', 'Y1', 'X', 'Y', ...
            'FLUX_APER_3', 'MAG_APER_3', 'MAG_AB_APER_3', 'MAG_PSF', 'MAG_AB_PSF'};
        Args.PropOld    = {'RA', 'Dec', 'XPEAK', 'YPEAK', 'X1', 'Y1', 'X', 'Y', ...
            'FLUX_APER_3', 'MAG_APER_3', 'MAG_PSF'};
    end
    %
    Result = [];
    % load the data:
    if Args.CoaddOnly
        [AllSI0, Coadd0, MS0] = pipeline.last.load.loadVisit(Dir0,'TempName_IndivIm',[],'TempName_MS',[]);
        [AllSI1, Coadd1, MS1] = pipeline.last.load.loadVisit(Dir1,'TempName_IndivIm',[],'TempName_MS',[]);        
    else
        [AllSI0, Coadd0, MS0] = pipeline.last.load.loadVisit(Dir0);
        [AllSI1, Coadd1, MS1] = pipeline.last.load.loadVisit(Dir1);
    end
    
    % 1. overlap 
    R0 = pipeline.last.quality.overlapSources(Coadd0,'Prop',Args.PropOld,'CroppingScheme','old');
    R1 = pipeline.last.quality.overlapSources(Coadd1,'Prop',Args.PropNew,'CroppingScheme','new');
    
    
end
% 
% Overlap regions - for each one of the following quantities: APER_MAG_3, APER_MAG_AB_3, PSF_MAG, PSF_AB_MAG, RA, Dec
% * median of all differences over all overlaps and epochs.
% * std of all differences -"-A3. max(abs(of all differences)) -"-
% * rms vs mag plot, over all epochs, for MAG_APER_3, MAG_PSF (AB and not), RA, Dec.
% * two plots for quantity - one for crop 1, and another for crop 10.
% * For each such plot, derive the asymptotic value.
% * Histograms and numbers
% * For each bit mask flag type: the mean and std number of flags per image.
% * Histogram of asymptotic rms for astrometry and photometry (over all epochs and crops)
% * mean asymptotic values for each crop
% * Manual tests
% * Is 2nd moment reasonable?
% * Is FWHM ok?
% * Is lim mag ok?
% * Is sky brightness (mag/sq arcsec) ok?
