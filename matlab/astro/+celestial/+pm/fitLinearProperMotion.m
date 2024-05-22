function [Result, Igood] = fitLinearProperMotion(Time, RA, Dec, ErrRA, ErrDec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Mar) 
    % Example: celestial.pm.fitLinearProperMotion

    arguments
        Time
        RA
        Dec
        ErrRA                        = [];
        ErrDec                       = [];
        Args.DimEpoch                = 1;
        Args.MinNObs                 = 3;
        Args.Units                   = 'deg';
        Args.UnitsErr                = 'deg';
        Args.Niter                   = 2;

        Args.StdFun                  = @tools.math.stat.std_mad;
        Args.StdFunArgs cell         = {};
        Args.MeanFun                 = @median;
        Args.MeanFunArgs cell        = {1, 'omitnan'};
        Args.SigmaClip               = [3.0 3.0]
    end
    RAD = 180./pi;

    if numel(Args.SigmaClip)==1
        Args.SigmaClip = repmat(Args.SigmaClip,1,2);
    end
    Args.SigmaClip(1) = -abs(Args.SigmaClip(1));
    

    % Make sure that Epoch is in the 1st dimension
    Time = Time(:);
    if Args.DimEpoch==2
        RA  = RA.';
        Dec = Dec.';
    end

    [Nepoch, Nsrc] = size(RA);
    

    % convert to radians:
    Conv    = convert.angular(Args.Units, 'rad');
    RA      = RA.*Conv;    % [rad]
    Dec     = Dec.*Conv;   % [rad]
    if ~isempty(ErrRA)
        UseErr  = true;
        ConvErr = convert.angular(Args.UnitsErr, 'rad');
        ErrRA   = ErrRA.*ConvErr;
        ErrDec  = ErrDec.*ConvErr;
    else
        UseErr  = false;
    end

    % Subtract mean time
    Result.MeanTime = median(Time);
    Time     = Time - Result.MeanTime;


    if UseErr


    else
        
        H0ra  = ones(Nepoch,1);
        H0dec = H0ra;
        H1ra  = [Time, ones(Nepoch,1)];
        H1dec = H1ra;

        Result.Nobs     = zeros(1,Nsrc);
        Result.Ngood    = zeros(1,Nsrc);
        Result.RA0      = nan(1,Nsrc);
        Result.Dec0     = nan(1,Nsrc);
        Result.MuRA     = nan(1,Nsrc);
        Result.MuDec    = nan(1,Nsrc);
        Result.SigmaRA  = nan(1,Nsrc);
        Result.SigmaDec = nan(1,Nsrc);
        Result.Chi2_H0  = nan(1,Nsrc);
        Result.Chi2_H1  = nan(1,Nsrc);

        for Isrc=1:1:Nsrc
            % RA / H0
            FlagGood  = ~isnan(RA(:,Isrc)) & ~isnan(Dec(:,Isrc));
            Igood     = find(FlagGood);
            Nobs     = numel(Igood);

            if Nobs>Args.MinNObs

                Par0RA    = H0ra(Igood)\RA(Igood,Isrc);
                Resid0RA  = RA(:,Isrc) - H0ra*Par0RA;
                % RA / H1 
                Par1RA    = H1ra(Igood,:)\RA(Igood,Isrc);
                Resid1RA  = RA(:,Isrc) - H1ra*Par1RA;
                
                % Dec / H0
                Par0Dec   = H0dec(Igood)\Dec(Igood,Isrc);
                Resid0Dec = Dec(:,Isrc) - H0dec*Par0Dec;
                % Dec / H1
                Par1Dec   = H1dec(Igood,:)\Dec(Igood,Isrc);
                Resid1Dec = Dec(:,Isrc) - H1dec*Par1Dec;
    
                for Iiter=2:1:Args.Niter
                    
                    Std1RA   = Args.StdFun(Resid1RA, Args.StdFunArgs{:});
                    Std1Dec  = Args.StdFun(Resid1Dec, Args.StdFunArgs{:});
                    Mean1RA  = Args.MeanFun(Resid1RA, Args.MeanFunArgs{:});
                    Mean1Dec = Args.MeanFun(Resid1Dec, Args.MeanFunArgs{:});
                    
                    Z1RA  = (Resid1RA - Mean1RA)./Std1RA;
                    Z1Dec = (Resid1Dec - Mean1Dec)./Std1Dec;
    
                    FlagGood = Z1RA>Args.SigmaClip(1) & Z1Dec<Args.SigmaClip(2) & Z1Dec>Args.SigmaClip(1) & Z1Dec<Args.SigmaClip(2) & ...
                               ~isnan(RA(:,Isrc)) & ~isnan(Dec(:,Isrc));
                    Igood    = find(FlagGood);
                    Ngood    = numel(Igood);
    
                    % RA / H0
                    Par0RA    = H0ra(Igood)\RA(Igood,Isrc);
                    Resid0RA  = RA(:,Isrc) - H0ra*Par0RA;
                    % RA / H1 
                    Par1RA    = H1ra(Igood,:)\RA(Igood,Isrc);
                    Resid1RA  = RA(:,Isrc) - H1ra*Par1RA;
                    
                    % Dec / H0
                    Par0Dec   = H0dec(Igood)\Dec(Igood,Isrc);
                    Resid0Dec = Dec(:,Isrc) - H0dec*Par0Dec;
                    % Dec / H1
                    Par1Dec   = H1dec(Igood,:)\Dec(Igood,Isrc);
                    Resid1Dec = Dec(:,Isrc) - H1dec*Par1Dec;
    
                end
    
                Std0RA   = Args.StdFun(Resid0RA(Igood));
                Std0Dec  = Args.StdFun(Resid0Dec(Igood));
                Std1RA   = Args.StdFun(Resid1RA(Igood));
                Std1Dec  = Args.StdFun(Resid1Dec(Igood));
                Mean1RA  = Args.MeanFun(Resid1RA(Igood));
                Mean1Dec = Args.MeanFun(Resid1Dec(Igood));
                Ngood    = numel(Igood);
    
                
                Result.Nobs(Isrc)    = Nobs;
                Result.Ngood(Isrc)   = Ngood;
                Result.RA0(Isrc)     = Par1RA(2).*RAD;
                Result.Dec0(Isrc)    = Par1Dec(2).*RAD;
                Result.MuRA(Isrc)    = Par1RA(1).* cos(Par1Dec(2)).*RAD;  %cos(Dec0);
                Result.MuDec(Isrc)   = Par1Dec(1).*RAD;
                Result.SigmaRA(Isrc)  = Std0RA.* cos(Par1Dec(2)).*RAD; %cos(Dec0);
                Result.SigmaDec(Isrc) = Std0Dec.*RAD;
                % no multiplication by cos(Dec) as it affect bot
                % denominator and numerator
                Result.Chi2_H0(Isrc)  = sum((Resid0RA./Std0RA).^2 + (Resid0Dec./Std0Dec).^2, 1, 'omitnan');
                Result.Chi2_H1(Isrc)  = sum((Resid1RA./Std0RA).^2 + (Resid1Dec./Std0Dec).^2, 1, 'omitnan');
            end
        end
                
        % Result.LogL  = -sum( (Result.MuRA.^2.*Time.^2 + 2.*Result.MuRA.*Time.*(Result.RA0 - RA))./(2.*Result.SigmaRA.^2) + ...
        %                     (Result.MuDec.^2.*Time.^2 + 2.*Result.MuDec.*Time.*(Result.Dec0 - Dec))./(2.*Result.SigmaRA.^2), 1, 'omitnan');
        Result.DeltaChi2 = Result.Chi2_H0 - Result.Chi2_H1;
        Result.Prob  = chi2cdf(Result.DeltaChi2, 2);
           
    end

end
