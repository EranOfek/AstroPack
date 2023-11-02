function Result=regress(Time, Mag, Err, Args)
    % Detrend a time series by regression against external parameters
    % Input  : - A column vector of time.
    %          - A column vector of magnitudes.
    %          - A column vector, or scalar, of mag errors.
    %            If empty, then set Error to 1.
    %            Default is [].
    %          * ...,key,val,...
    %            'RegressCols' - A matrix with the external regression
    %                   parameters in different columns.
    %                   Must provided.
    %            'SysErr' - Systematic error to add in quadrature to the
    %                   errors. Default is 0.01.
    %            'IncludeConst' - Include constant, as the last column, in
    %                   the regression columns matrix.
    %                   Default is true.
    %            'Method' - Default is 'lscov'.
    % Output : - A structure with the following fields:
    %            .Time
    %            .Resid
    %            .Err
    %            .Std - std of residuals.
    %            .PreStd - std of magnitudes prior to regression.
    %            .Par 
    %            .ParErr
    %            .PreMean - Mean of magnitudes prior to regression.
    % Author : Eran Ofek (Oct 2023)
    % Example: Cat=cats.compact.WD_DR2;                                    
    %          AllT=VO.ZTF.wgetList_ztf_phot(Cat.Cat(1:10,1:2));
    %          Result=timeSeries.detrend.regress(AllT(2).LC.hjd,AllT(2).LC.mag, AllT(2).LC.magerr, 'RegressCols',[AllT(2).LC.ccdid, AllT(2).LC.clrcoeff, AllT(2).LC.airmass]);
    
    arguments
        Time
        Mag
        Err                        = [];
        Args.RegressCols           = [];
        Args.SysErr                = 0.01;
        Args.IncludeConst logical  = true;
        Args.Method                = 'lscov';  % 'lscov'
    end
    
    Ncol = size(Args.RegressCols,2);
    
    if isempty(Err)
        Err = 1;
    else
        Err = sqrt(Err.^2 + Args.SysErr.^2);
    end
    
    Nobs = numel(Time);
    
    % design matrix
    if Args.IncludeConst
        H = [Args.RegressCols, ones(Nobs,1)];
    else
        H = [Args.RegressCols];
    end
    
    switch lower(Args.Method)
        case 'lscov'
            [Par,ParErr] = lscov(H, Mag, 1./(Err.^2));
            Resid        = Mag - H*Par;
            
            Result.Time   = Time;
            Result.Resid  = Resid;
            Result.Err    = Err;
            Result.Std    = std(Resid);
            Result.PreStd = std(Mag);
            Result.Par    = Par;
            Result.ParErr = ParErr;
            Result.PreMean= mean(Mag);
            
        otherwise
            error('Unknown Method option');
    end
    
    
end