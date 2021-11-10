function Result = fitMultiProperMotion(Time, RA, Dec, ErrRA, ErrDec, Args)
    % Simultanoulsy (fast) fit proper motion and stationary model to observations
    %   The fit is done in RA/Dec space, and no cos(delta) is applied
    %   Perform hypothesis testing between the proper motion and stationary
    %   models.
    % Input  : - A vector of times.
    %          - An array of RA [Epoch X Source]
    %          - An array of Dec [Epoch X Source]
    %          - An array of errors in RA, in the same units as RA.
    %            Default is 1./(3600.*100).
    %          - An array of errors in Dec, in the same units as Dec.
    %            Default is 1./(3600.*100).
    %          * ...,key,val,...
    %            'MinNobs' - minimum number of data points required for fit
    %                   (used only when the number of observations is not
    %                   the same for all sources).
    %                   Default is 3.
    %            'Prob' - Vector of probabilities for which to calculate
    %                   the probably difference between H1 and H0.
    %                   Default is [1e-3 1e-5].
    %            'Units' - Units of RA/Dec. This is used only for the the
    %                   checking that the RA data is not crossing zero.
    %                   Default is 'deg'.
    %            'RenormErr' - A logical flag inducating if to normalize
    %                   the chi2 for the H1 hypothesis to 1.
    %                   Default is true.
    % Output : - A structure with the following fields:
    %            .MeanT - Mean epoch relative to which the fit is done.
    %            .RA.ParH1 - Parameters for H1 [pos; vel] for each source.
    %            .RA.ParH0 - Parameters for H0 [pos] for each source.
    %            .RA.Chi2_H1 - \chi^2 for H1
    %            .RA.Chi2_H0 - \chi^2 for H0
    %            .RA.Nobs - Number of observations for each source.
    %            .RA.DeltaChi2 - \Delta\chi^2 between H0 and H1.
    %            .RA.FlagH1 - A logical indicating if the source has a
    %                   prefreed H1 model over H0. each line, for each
    %                   requested proabability.
    %            .RA.StdResid_H1 - std of H1 residuals.
    %            .RA.StdResid_H0 - std of H0 residuals.
    %            .RA.StudentT_H1 - student-t statistics (mu/rms) for RA.
    %                   Dof is Nobs-2.
    %            .Dec. - the same as .RA, but for the Dec axis.
    % Author : Eran Ofek (May 2021)
    % Example: Time=(1:1:20)'; RA = randn(20,1e5)./(3600.*100); Dec=randn(20,1e5)./(3600.*200);
    %          RA(:,1) = (0.1:0.1:2)'./(3600);
    %          Result = celestial.pm.fitMultiProperMotion(Time, RA, Dec);

    arguments
        Time
        RA
        Dec
        ErrRA        = 1./(3600.*100);
        ErrDec       = 1./(3600.*100);
        Args.MinNobs = 3;
        Args.Prob    = [1e-3 1e-5];
        Args.Units   = 'deg';
        Args.RenormErr(1,1) logical = true;
    end
    DeltaDoF  = 1;
    ProbDeltaChi2 = chi2inv(1-Args.Prob, DeltaDoF);
    
    
    % verify that RA is not not near RA=0
    TPI = convert.angular('deg',Args.Units,360);
    FlagPi = range(RA)>(TPI.*0.5);
    IndPi  = find(FlagPi);
    Npi    = numel(IndPi);
    for Ipi=1:1:Npi
        FI = RA(:,IndPi(Ipi))>(TPI.*0.5);
        RA(FI,IndPi(Ipi)) = RA(FI,IndPi(Ipi)) - TPI;
    end
        
    MeanT = mean(Time);
    N     = numel(Time);
    Nsrc  = size(RA,2);
    
    Result.MeanT = MeanT;
    
    H1 = [ones(N,1), Time(:)-MeanT];
    H0 = ones(N,1);
    
    for I=1:1:2
        if I==1
            % RA fit
            Y = RA;
            ErrY = ErrRA;
            PropStr = 'RA';
        else
            % Dec fit
            Y = Dec;
            ErrY = ErrDec;
            PropStr = 'Dec';
        end
        
        if any(isnan(Y),'all')
            % use loop and omit NaNs
            ParH1  = nan(2,Nsrc);
            ParH0  = nan(1,Nsrc);
            FlagNN = ~isnan(Y);
            Nobs   = sum(FlagNN, 1);
            for Isrc=1:1:Nsrc
                if Nobs(Isrc)>=Args.MinNobs
                    ParH1(:,Isrc) = H1(FlagNN(:,Isrc),:)\Y(FlagNN(:,Isrc),Isrc);
                    ParH0(:,Isrc) = H0(FlagNN(:,Isrc),:)\Y(FlagNN(:,Isrc),Isrc);
                    
                    % calculate the std of Times and T0
                    T0   = mean(Time(FlagNN(:,Isrc)));
                    StdT = std(Time(FlagNN(:,Isrc)));
                end
            end

        else
            % no NaN - fit simultanously
            ParH1 = H1 \ Y;
            ParH0 = H0 \ Y;
            Nobs  = size(Y,1);
            
            T0   = mean(Time);
            StdT = std(Time);
        end

        Y_calcH1 = H1 * ParH1;
        Y_calcH0 = H0 * ParH0;

        ResidH1  = Y - Y_calcH1;
        ResidH0  = Y - Y_calcH0;
        
        Result.(PropStr).StdResid_H1 = std(ResidH1,[],1,'omitnan');
        Result.(PropStr).StdResid_H0 = std(ResidH0,[],1,'omitnan');
        
        % store RA/Dec info
        Result.(PropStr).ParH1 = ParH1;
        Result.(PropStr).ParH0 = ParH0;
        
        Result.(PropStr).Chi2_H1 = nansum((ResidH1./ErrY).^2, 1);
        
        Result.(PropStr).Chi2_H0 = nansum((ResidH0./ErrY).^2, 1);     
        
        % this is incorrect - need to use student-t distribution
        % because the std is practically unknown
        
        % but must renorm T...
        Result.(PropStr).StudentT_H1 = ParH1(2,:)./Result.(PropStr).StdResid_H1./StdT;        
        
        if Args.RenormErr
            
            %Result.(PropStr).Chi2_H0 = Result.(PropStr).Chi2_H0./Result.(PropStr).Chi2_H1;
            %Result.(PropStr).Chi2_H1 = ones(size(Result.(PropStr).Chi2_H1));
            
            Result.(PropStr).Chi2_H1 = Result.(PropStr).Chi2_H1./Result.(PropStr).Chi2_H0;
            Result.(PropStr).Chi2_H0 = ones(size(Result.(PropStr).Chi2_H0));
        end
        
        Result.(PropStr).Nobs    = Nobs;

        Result.(PropStr).DeltaChi2 = Result.(PropStr).Chi2_H0 - Result.(PropStr).Chi2_H1;
        Result.(PropStr).FlagH1    = Result.(PropStr).DeltaChi2 > ProbDeltaChi2(:);
        
        
    end
    
end
