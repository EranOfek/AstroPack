function Result = mergeCatalogs(Obj, Args)
    %
    
    arguments
        Obj
        Args.CooType                 = 'sphere';
        Args.Radius                  = 2;
        Args.RadiusUnits             = 'arcsec';
        
        Args.RelPhot logical         = true;
        Args.FitPM logical           = true;
        Args.fitMotionArgs cell      = {'Prob',1e-5};
        
        
        
        Args.ColPrefix cell          = {'Mean_', 'Med_', 'Std_', 'Err_'};
        Args.ColGeneratingFun        = {@mean, @median, @std, @tools.math.stat.mean_error};
        Args.GeneratingFunArgs       = { {1,'omitnan'}, {1,'omitnan'}, {[],1,'omitnan'}, {1} };
        Args.ColsToApplyFun          = {'RA','Dec','MAG_PSF'};   
        
        Args.ColName_PM_DeltaChi2    = 'PM_DeltaChi2';
        Args.ColName_PM_RA           = 'PM_RA';
        Args.ColName_PM_Dec          = 'PM_Dec';
        Args.ColName_Ep_RA           = 'EpochRA';
        Args.ColName_Ep_Dec          = 'EpochDec';
        Args.ColName_PM_ErrRA        = 'PM_ErrRA';
        Args.ColName_PM_ErrDec       = 'PM_ErrDec';
        Args.ColName_Ep_ErrRA        = 'EpochErrRA';
        Args.ColName_Ep_ErrDec       = 'EpochErrDec';
        
        Args.ColName_Nobs            = 'Nobs';   % if empty do not add
        Args.ColName_Epoch           = 'Epoch';
        Args.EpochUnits              = 'JD';
        
        Args.FitPoly(1,1) logical    = true;
        
        
        
        Args.unifiedSourcesCatalogArgs cell     = {};
        Args.matchArgs cell          = {};
        Args.MatchedColums           = {'RA','Dec','X','Y','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
    end
    
    % find all unique sources
    [Nepochs, Nfields] = size(Obj);
    JD  = julday(Obj(:,1));     

    for Ifields=1:1:Nfields
        [~, ~, Matched(Ifields,:)] = imProc.match.unifiedSourcesCatalog(Obj(:,Ifields), 'CooType',Args.CooType,...
                                                         'Radius',Args.Radius,...
                                                         'RadiusUnits',Args.RadiusUnits,...
                                                         Args.unifiedSourcesCatalogArgs{:});
                                                     
        MatchedS(Ifields) = MatchedSources;
        MatchedS(Ifields).addMatrix(Matched(Ifields,:), Args.MatchedColums);
        % populate JD
        MatchedS(Ifields).JD = JD;  
        
        % image 2 image X shift (pix)
        Summary(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.X,1,1), 2, 'omitnan');
        Summary(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.Y,1,1), 2, 'omitnan');
    
        % relative photometry
        if Args.RelPhot
            [ResZP(Ifields), MatchedS(Ifields)] = lcUtil.zp_lsq(MatchedS(Ifields), 'MagField','MAG_CONV_2', 'MagErrField','MAGERR_CONV_2');
        end
        
    end    
    
    % variability
    % Mean_{MagCol}
    % Med_{MagCol}
    % Std_{MagCol}
    % RStd_{MagCol}
    % MaxMedDev_{MagCol}
    % Max2MedDev_{MagCol}
    % Nobs_{MagCol}
    % CombFLAGS
    MagStat = statSummary(MatchedS, 'FieldNameDic','MAG_CONV_2', 'FlagsNameDic','FLAGS');
    
    % fit proper motion
    if Args.FitPM
        FitMotion = lcUtil.fitMotion(MatchedS, Args.fitMotionArgs{:});
    end
    
    
    
    
    I= 9;
    
    semilogy(nanmedian(MatchedS(I).Data.MAG_CONV_2,1),  nanstd(MatchedS(I).Data.MAG_CONV_2,[],1),'.')
    hold on;
    semilogy(nanmedian(CorrMS(I).Data.MAG_CONV_2,1),  nanstd(CorrMS(I).Data.MAG_CONV_2,[],1),'.')
    
    semilogy(nanmedian(MatchedS(I).Data.MAG_CONV_2,1),  nanstd(MatchedS(I).Data.Dec,[],1).*3600,'.')
    
    
    
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
    end
    
                                                     
    % Match catalogs by RA/Dec or X/Y
    [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = imProc.match.match(Obj, AllSources, 'CooType',Args.CooType,...
                                                                                         'Radius',Args.Radius,...
                                                                                         'RadiusUnits',Args.RadiusUnits,...
                                                                                         Args.matchArgs{:});
    
    % Define the matched columns
    MatchedColums = unique([Args.MatchedColums(:), Args.ColsToCalcMean(:); Args.ColsToCalcMedian(:); Args.ColsToCalcStd(:); Args.ColsToCalcErr(:)]);
    
    % Generate a matched sources matrix
    MatchedS = MatchedSources;
    MatchedS.addMatrix(MatchedObj, Args.MatchedColums);
    
    
    % Mean position
    Ncol    = numel(Args.ColsToApplyFun);
    Nprefix = numel(Args.ColPrefix);
    
    % allocate table data variables
    NtotCol = Ncol.*Nprefix
    NewTableColName
    NewTable
    
    ColInd  = 0;
    for Icol=1:1:Ncol
        % get data for Column
        ColData = MatchedS.Data.(Args.ColsToApplyFun{Icol});
        
        for Iprefix=1:1:Nprefix
            ColInd = ColInd + 1;
            
            % apply function on ColData (column wise)
            NewColumn = Args.ColGeneratingFun(ColData, Args.GeneratingFunArgs{Iprefix}{:});
            
            % NewColumn name
            NewColName = sprintf('%s%s', Args.ColPrefix{Iprefix}, Args.ColsToApplyFun{Icol});
            
            % store NewColumn in table
            NewTableColName{ColInd} = NewColName;
            NewTable(:,ColInd) = NewColumn(:);
        end
    end
    
    
    
    
    
    
    % relative photometry
    
    % photometric variability properties
    
    % lcUtil.fitPolyHyp
    
    
    % Photometric template matching
    
    % treat unmatched sources
    
    % match to external catalogs
    
    
end