function Result = mergeCatalogs(Obj, Args)
    %
    
    arguments
        Obj
        Args.CooType                 = 'sphere';
        
        Args.ColPrefix cell          = {'Mean_', 'Med_', 'Std_', 'Err_'};
        Args.ColGeneratingFun        = {@mean, @median, @std, @tools.math.stat.mean_error};
        Args.GeneratingFunArgs       = { {1,'omitnan'}, {1,'omitnan'}, {[],1,'omitnan'}, {1} };
        Args.ColsToApplyFun          = {'RA','Dec','MAG_PSF'};   
        
        Args.FitPM(1,1) logical      = true;
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
        Args.MatchedColums           = {'RA','Dec','PSF_MAG','PSF_MAGERR','APER_MAG_1_','APER_MAGERR_1_','APER_MAG_2_','APER_MAGERR_2_'});
        Args.fitMotionArgs cell      = {'Prob',1e-5};
    end
    
    % find all unique sources
    AllSources = imProc.match.unifiedSourcesCatalog(Obj, 'CooType',Args.CooType, Args.unifiedSourcesCatalogArgs{:});
    
    % Match catalogs by RA/Dec or X/Y
    [MatchedObj, UnMatchedObj, TruelyUnMatchedObj] = imProc.match.match(Obj, AllSources, 'CooType',Args.CooType, Args.matchArgs{:});
    
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
    
    % fit PM
    if Args.FitPM
        FitMotion = lcUtil.fitMotion(Obj, Args.fitMotionArgs{:});
        
        
        
        
        
    end
    
    
    
    
    % relative photometry
    
    % photometric variability properties
    
    % Photometric template matching
    
    % treat unmatched sources
    
    % match to external catalogs
    
    
end