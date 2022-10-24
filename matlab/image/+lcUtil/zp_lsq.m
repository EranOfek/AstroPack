function [Result, CorrMS] = zp_lsq(MS, Args)
    % Apply a relative photometry least square ZP solution to a light curve
    % in MatchedSources object.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,...
    %            'MagField' - Mag field in the MatchedSources object data.
    %                   Default is 'MAG'.
    %            'MagErrField' - Mag error field in the MatchedSources object data.
    %                   Default is 'MAGERR'.
    %            'UseSparse' - A logical indicating if to generate
    %                   a sparse design matrix. Default is false.
    %            'SrcProp' - A cell array of additional properties
    %                   to add to the design matrix.
    %                   Each cell element can be a row vector
    %                   (property per source; e.g., color), a
    %                   column vector (property per epoch; e.g.,
    %                   air mass), or a matrix of size Nep X Nsrc.
    %                   These properties will be added to the
    %                   design matrix according to the scheme
    %                   dictated by 'SrcPropCoefType'.
    %                   Default is {}.
    %            'SrcPropCoefType' - A vector of numbers, each
    %                   element corresponds to a cell element in
    %                   'SrcProp'. The numbres may be one of the
    %                   following:
    %                   1 - will add a single column to the design
    %                   matrix (i.e., a single coef.).
    %                   2 - will add a column per epoch.
    %            'MinNepoch' - Use sources which the min. number of epochs
    %                   they appear (not NaN) is larger than this number.
    %                   Default is 10.
    %            'Niter' - Number of fitting iterations.
    %                   In the second iteration lscov will be used with the
    %                   errors estimated from the mag vs. std plot.
    %                   Default is 2.
    %            'UseBL' - A logical indicating if to use the "\" operator
    %                   in the first iteration. Default is true.
    %            'CalibMag' - A vector of calibrated (external) magnitude
    %                   per source. The resulted magnitude will calibrated
    %                   according to this magnitudes.
    %            'Plot' - A logical indicating if to plot a mag. vs. std.
    %                   plot for each iteration. Default is false.
    % Output : - A structure containing the best fit information:
    %            .FlagMin - A vector of flags (one per source) indicating
    %                   if the source was used. I.e., number of epochs
    %                   larger than MinNepoch.
    %            .FlagSrc - A vector of flags (Nsrc X Nep) indicating if
    %                   the source was used in the final iteration
    %                   calibration.
    %            .FitZP - A vector of fitted ZP [mag].
    %            .FitMeanMag - A vector of fitted Mean Mag [mag].
    %            .FitExtra - A vector of extra fitted parameters.
    %            .MeanMag - A vector of mean mags.
    %            .AllResid - An array of all residuals from best fit.
    %            .StdSrc - A vector of Std of residuals per source.
    %            .StdEpoch - A vector of std of residuals per epoch.
    % Author : Eran Ofek (Sep 2021)
    % Example: Fzp   = 1 + rand(100,1);
    %          Fstar = rand(1,200).*3900 + 100; 
    %          Flux = Fzp.*Fstar;
    %          Flux = poissrnd(Flux);
    %          FluxErr = sqrt(Flux);
    %          Mag     = 22-2.5.*log10(Flux);
    %          MeanMag = mean(Mag);
    %          MagErr  = 1.086.*FluxErr./Flux;
    %          MS = MatchedSources;
    %          MS.addMatrix({Mag, MagErr},{'MAG','MAGERR'});
    %
    %          [R, CorrMS] = lcUtil.zp_lsq(MS);
    %          [R, CorrMS] = lcUtil.zp_lsq(MS, 'CalibMag',MeanMag);
    %          std(R.FitZP +2.5.*log10(Fzp))  % verify that std is small
   
    arguments
        MS(1,1) MatchedSources
        Args.MagField char          = 'MAG';
        Args.MagErrField char       = 'MAGERR';
        
        Args.UseSparse(1,1) logical = false;
        Args.SrcProp cell           = {};
        Args.SrcPropCoefType        = 1;
        Args.MinNepoch              = 10;
        Args.Niter                  = 2;
        Args.UseBL(1,1) logical     = true;   % use \ operator in first iteration
        
        Args.CalibMag               = [];
        
        Args.Plot(1,1) logical      = false;
    end
    
    Mag    = getMatrix(MS, Args.MagField);
    MagErr = getMatrix(MS, Args.MagErrField);
    
    % select sources with minimum number of observations
    NdetPerSrc = sum(~isnan(Mag),1);
    FlagMin    = NdetPerSrc>Args.MinNepoch;

    Mag    = Mag(:,FlagMin);
    MagErr = MagErr(:,FlagMin);

    [Nep, Nsrc] = size(Mag);
    
    % construct the design matrix for the LSQ fit
    % FFU: add color/etc terms
    H       = MatchedSources.designMatrixCalib(Nep, Nsrc, 'UseSparse',Args.UseSparse,...
                                                          'SrcProp',Args.SrcProp,...
                                                          'SrcPropCoefType',Args.SrcPropCoefType);
    %Mag     = Mag(:);
    InvVar  = 1./(MagErr.^2); 
    FlagSrc = ~isnan(Mag); 
    
    for I=1:1:Args.Niter
        if (I==1 && Args.Niter>1) || Args.UseBL
            Par    = H(FlagSrc,:)\Mag(FlagSrc);
            ParErr = nan(size(Par));
        else
            % use lscov
            [Par, ParErr] = lscov(H(FlagSrc,:), Mag(FlagSrc), InvVar(FlagSrc));
        end
        
        AllResid   = Mag(:) - H*Par;
        MeanMag    = mean(Mag, 1, 'omitnan');
        
        AllResid   = reshape(AllResid, Nep, Nsrc);
        StdSrc     = std(AllResid, [], 1, 'omitnan');
        StdEpoch   = std(AllResid, [], 2, 'omitnan');
        
        
        % need another function calib.std_vs_mag
        if I<Args.Niter
            [FlagSrc,Res] = imUtil.calib.resid_vs_mag(Mag(:), AllResid(:));
        
            % update the InvVar based on the rms vs. mag diagram
            InvVar = 1./(Res.InterpStdResid.^2);
        end
        
        if Args.Plot
            %semilogy(FitMeanMag(:),Std(:),'.')
            semilogy(MeanMag(:),StdSrc(:),'.')
            hold on;  
        end
        
    end
    FitZP      = Par(Nsrc+(1:Nep));
    FitMeanMag = Par(1:Nsrc);
    if numel(Par)>(Nsrc+Nep)
        % extra fitted parameters (e.g., color term)
        FitExtra   = Par(Nsrc+Nep+1:end);
    else
        FitExtra   = [];
    end

    
    Result.FlagMin    = FlagMin;
    Result.FlagSrc    = FlagSrc;
    Result.FitZP      = FitZP;
    Result.FitMeanMag = FitMeanMag;
    Result.FitExtra   = FitExtra;
    Result.MeanMag    = MeanMag;
    Result.AllResid   = AllResid;
    Result.StdSrc     = StdSrc;
    Result.StdEpoch     = StdEpoch;

    if Args.Plot
        H = xlabel('Mag');
        H.FontSize    = 18;
        H.Interpreter = 'latex';
        H = ylabel('rms [mag]');
        H.FontSize    = 18;
        H.Interpreter = 'latex';
    end

    if nargout>1
        % create a copy of the MatchedSources object
        CorrMS = MS.copy();
        
        % calibrate against an external catalog info:
        if isempty(Args.CalibMag)
            Add = 0;
        else
            Add = median(Args.CalibMag(FlagMin) - Result.FitMeanMag(:), 1, 'omitnan');
        end
        CorrMS.addMatrix({Mag - Result.FitZP + Add}, {'Mag'});
    end
end