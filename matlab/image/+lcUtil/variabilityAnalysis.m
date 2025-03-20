function [AC, Result] = variabilityAnalysis(Obj, Args)
    % Perform variability analysis and search on a MatchedSources object.
    %   The outputs include: a modified calibrated MatchedSources object with the
    %   added information, and a table of selected variable candidates with
    %   their properties.
    %
    % Input  : - A single element MatchedSources object.
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - An updated MatchedSources object.
    %          - An AstroCatalog object with a table with the selected variable candidates.
    % Author : Eran Ofek (2025 Mar) 
    % Example: MS=MatchedSources.read({'/marvin/LAST.01.01.01/2025/03/15/proc/004325v0/LAST.01.01.01_20250316.004635.263_clear_1362_000_001_010_sci_merged_MergedMat_1.hdf5'})
    %          [AC, Result] = lcUtil.variabilityAnalysis(MS)

    arguments
        Obj MatchedSources
        Args.Visit                    = NaN;
        Args.RemoveFlags              = {'Saturated', 'NearEdge', 'Overlap', 'NaN', 'Negative'};
        Args.BitDict                  = BitDictionary;
        Args.FieldFlags               = 'FLAGS';
        Args.FieldMag                 = 'MAG_BEST';
        Args.FieldMagErr              = 'MAGERR_PSF';
        Args.FieldSN                  = 'SN_3';
        Args.Detrend2D logical        = true;
        Args.zp_fit2DArgs             = {};
        Args.DetrendZP logical        = true;
        Args.zp_meddiffArgs           = {};
        
        %
        Args.PS_MaxFreq               = 86400./60;
        Args.PS_ThresholdNp           = 12;
        Args.PS_Threshold             = 12;
        
        Args.RMS_NsigmaPred           = 10;
        Args.RMS_MinNdet              = 10;
        
        Args.Poly1_MinDeltaChi2       = 15;
        Args.Poly5_MinDeltaChi2       = 25;
        
        Args.RM_MinAbsSN              = 8
        
        Args.FlareNaN_MinSN           = 8;
        Args.FlareNaN_MinNdet         = 1;  % 1 produce a lot of bad detections, but may be useful for satellite glints?
        
        Args.SearchRadius             = 60;

        Args.CreateNewObj logical     = false;
    end
    RAD = 180./pi;


    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    
    Iobj = 1;
    
    % populate:
    NsrcAll = Result(Iobj).Nsrc;
    Result(Iobj).bestMag;
    Result(Iobj).addSrcData;
    
    % clean data
    F = Result(Iobj).searchFlags('UseSrcData',true, 'BitDic',Args.BitDict, 'PropFlags',Args.FieldFlags, 'FlagsList',Args.RemoveFlags);
    Result.selectBySrcIndex(~F, 'CreateNewObj',false);
    NsrcGood = Result.Nsrc;
    
    % detrend data
    if Args.Detrend2D
        Result = lcUtil.zp_fit2D(Result(Iobj), 'FieldMag',Args.FieldMag, 'FieldMagErr',Args.FieldMagErr, 'CreateNewObj',false, 'BitDict',Args.BitDict, Args.zp_fit2DArgs{:});
    end
    if Args.DetrendZP
        Rzp = lcUtil.zp_meddiff(Result(Iobj), 'MagField',Args.FieldMag, 'MagErrField',Args.FieldMagErr, 'BitDict',Args.BitDict, Args.zp_meddiffArgs{:});
        Result.applyZP(Rzp);
    end
    
    % MatchedSources stat:
    % Nsrc, NsrcAll, Nep, MinJD, MaxJD, Duration, MidJD, Node, Mount,
    % Camera, CropID, Visit, VisitDate, FullFileNames
    MinJD = min(Result(Iobj).JD);
    MaxJD = max(Result(Iobj).JD);
    
    %TableStat = [Nsrc, NsrcAll, Result(Iobj).Nepoch, MinJD, MaxJD, MaxJD-MinJD, 0.5.*(MinJD + MaxJD), ...
    %             Node, Mount, Camera, CropID, Visit, VisitDate, FullFileNames];
    
    % power spectrum
    TablePS       = lcUtil.reportPowerSpec(Result, 'FieldMag',Args.FieldMag,...
                                                   'MaxFreq',Args.PS_MaxFreq,...
                                                   'ThresholdNp',Args.PS_ThresholdNp,...
                                                   'Threshold',0);
            
    % rms
    TableRMS      = lcUtil.reportRMS(Result, 'FieldMag',Args.FieldMag,...
                                             'ThresholdRMSpred',-Inf,...
                                             'ThresholdNdet',0);

    
    
    % polynomail fitting
    TablePolyHyp  = lcUtil.reportPolyHyp(Result, 'FieldMag',Args.FieldMag);
    
    % run mean filter
    TableRMF      = lcUtil.reportRunMean(Result, 'FieldMag',Args.FieldMag);
    
    % flare above NaN
    TableFlareNan = lcUtil.reportFlareAboveNan(Result, 'MinSN',Args.FlareNaN_MinSN, 'FieldSN',Args.FieldSN);
    
    % correlations
    TableCorr     = lcUtil.reportCorr(Result);
    
    % proper motion
    TableMotion   = lcUtil.reportMotion(Result);
    
    % Positions and SN
    TableMain = array2table([Result.SrcData.RA(:), Result.SrcData.Dec(:), Result.SrcData.(Args.FieldSN)(:), Result.SrcData.(Args.FieldFlags)(:)]);
    TableMain.Properties.VariableNames = {'RA', 'Dec', 'SN', 'FLAGS'};
    
    % merged Table
    Table = [TableMain, TablePS, TableRMS, TablePolyHyp, TableRMF, TableFlareNan, TableCorr, TableMotion];
    
    
    % select
    Flag = Table.MaxPower>Args.PS_Threshold | ...
           (Table.RMS_NsigmaPred>Args.RMS_NsigmaPred & Table.Ndet>=Args.RMS_MinNdet) | ...
           Table.Poly1_DeltaChi2>Args.Poly1_MinDeltaChi2 | ...
           Table.Poly5_DeltaChi2>Args.Poly5_MinDeltaChi2 | ...
           Table.RM_MinSN_Win2>Args.RM_MinAbsSN | ...
           Table.RM_MaxSN_Win2>Args.RM_MinAbsSN | ...
           Table.RM_MinSN_Win3>Args.RM_MinAbsSN | ...
           Table.RM_MaxSN_Win3>Args.RM_MinAbsSN | ...
           Table.RM_MinSN_Win4>Args.RM_MinAbsSN | ...
           Table.RM_MaxSN_Win4>Args.RM_MinAbsSN | ...
           Table.RM_MinSN_Win5>Args.RM_MinAbsSN | ...
           Table.RM_MaxSN_Win5>Args.RM_MinAbsSN | ...
           (Table.FlareNanFlag & Table.Ndet>=Args.FlareNaN_MinNdet);
       
    %
    Table = Table(Flag,:);
    
    Nsrc = size(Table,1);
    
    TableNstat = array2table([Nsrc, NsrcGood, NsrcAll, MinJD, MaxJD].*ones(Nsrc,1));
    TableNstat.Properties.VariableNames = {'Nfound', 'NsrcGood', 'NsrcAll', 'MinJD', 'MaxJD'};
    
    if ~iscell(Result.FileName)
        Result.FileName = {Result.FileName};
    end
    FN = FileNames.generateFromFileName(Result.FileName);
    
    ProjName = FN.ProjName{1};
    FieldID  = FN.FieldID{1};
    CropID   = FN.CropID(1);
    Nfiles   = numel(FN.Time);
    Visit    = Args.Visit;
    
    TableFile = {ProjName, FieldID, CropID, Nfiles, Visit};
    TableFile = repmat(TableFile,Nsrc,1);
    TableFile = cell2table(TableFile);
    TableFile.Properties.VariableNames = {'ProjName', 'FieldID', 'CropID', 'Nfiles', 'Visit'};
    
    Table = [Table, TableNstat, TableFile];
    

    AC = AstroCatalog;
    AC.Catalog  = Table;
    %AC.ColNames = Table.Properties.VariableNames;
    AC.Name = Result.FileName;


    MergedCatBitMask = uint32(zeros(Nsrc, 1));
    InfoGAIA         = nan(Nsrc, 11);
    for Isrc=1:1:Nsrc
        MergedCat = catsHTM.cone_search('MergedCat',AC.Catalog.RA(Isrc)./RAD, AC.Catalog.Dec(Isrc)./RAD, Args.SearchRadius, 'OutType','astrocatalog');
        Dist = celestial.coo.sphere_dist_fast(AC.Catalog.RA(Isrc)./RAD, AC.Catalog.Dec(Isrc)./RAD, MergedCat.Catalog(:,1), MergedCat.Catalog(:,2)).*RAD.*3600;
        Flag = Dist<MergedCat.Catalog(:,4);
        if sum(Flag)>0
            MergedCatBitMask(Isrc) = tools.array.bitor_array(uint32(MergedCat.Catalog(Flag,3)),1,true);
        end


        GAIA = catsHTM.cone_search('GAIADR3',AC.Catalog.RA(Isrc)./RAD, AC.Catalog.Dec(Isrc)./RAD, Args.SearchRadius, 'OutType','astrocatalog');
        % apply PM
        
        EpochIn = 2016; %GAIA.Catalog(1,3);
        if GAIA.sizeCatalog>0
            GAIA = imProc.cat.applyProperMotion(GAIA, EpochIn, MinJD,'EpochInUnits','J','EpochOutUnits','JD','ApplyPlx',false);
        end

        Dist = celestial.coo.sphere_dist_fast(AC.Catalog.RA(Isrc)./RAD, AC.Catalog.Dec(Isrc)./RAD, GAIA.Catalog(:,1), GAIA.Catalog(:,2)).*RAD.*3600;
        
        [MinDist, MinI] = min(Dist);
        Nstar5          = numel(Dist<5);
        
        InfoGAIA(Isrc,:) = [MinDist, Nstar5, GAIA.Table.Plx(MinI),...
                                     GAIA.Table.ErrPlx(MinI),...
                                     GAIA.Table.phot_bp_mean_mag(MinI),...
                                     GAIA.Table.phot_rp_mean_mag(MinI),...
                                     GAIA.Table.phot_g_mean_mag(MinI),...
                                     GAIA.Table.teff_gspphot(MinI),...
                                     GAIA.Table.logg_gspphot(MinI),...
                                     GAIA.Table.non_single_star(MinI),...
                                     GAIA.Table.astrometric_excess_noise(MinI)];


    end
    AC.Catalog.MergedCat = MergedCatBitMask;
    InfoGAIA = array2table(InfoGAIA, 'VariableNames',{'GAIA_MinDist','GAIA_Nstar5','GAIA_Plx','GAIA_ErrPlx','GAIA_Bp','GAIA_Rp','GAIA_G','GAIA_Teff','GAIA_logg','GAIA_NonSingle','GAIA_ExcessNoise'});
    AC.Catalog = [AC.Catalog, InfoGAIA];
    AC.ColNames = AC.Catalog.Properties.VariableNames;
end
