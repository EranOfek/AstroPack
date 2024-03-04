function [Result] = fitLinearProperMotion(Time, RA, Dec, ErrRA, ErrDec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Mar) 
    % Example: 

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
        Args.SigmaClip               = [2.5 2.5]
    end

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
    MeanTime = median(Time);
    Time     = Time - MeanTime;


    if UseErr


    else
        
        H0ra  = ones(Nepoch,1);
        H0dec = H0ra;
        H1ra  = [Time, ones(Nepoch,1)];
        H1dec = H1ra;



        for Isrc=1:1:Nsrc
            % RA / H0
            FlagGood  = ~isnan(RA(:,Isrc)) & ~isnan(Dec(:,Isrc));
            Igood     = find(FlagGood);
            Ngood     = numel(Igood);

            if Ngood>Args.MinNObs

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
                    Resid0RA  = RA - H0ra*Par0RA;
                    % RA / H1 
                    Par1RA    = H1ra(Igood,:)\RA(Igood,Isrc);
                    Resid1RA  = RA - H1ra*Par1RA;
                    
                    % Dec / H0
                    Par0Dec   = H0dec(Igood)\Dec(Igood,Isrc);
                    Resid0Dec = Dec - H0dec*Par0Dec;
                    % Dec / H1
                    Par1Dec   = H1dec(Igood,:)\Dec(Igood,Isrc);
                    Resid1Dec = Dec - H1dec*Par1Dec;
    
                end
    
                Std1RA   = Args.StdFun(Resid1RA(Igood));
                Std1Dec  = Args.StdFun(Resid1Dec(Igood));
                Mean1RA  = Args.MeanFun(Resid1RA(Igood));
                Mean1Dec = Args.MeanFun(Resid1Dec(Igood));
                Ngood    = numel(Igood);
    
                L = (-(RA - Par1RA(2)).*Par1RA(1) + Par1RA(1).^2)./Std1RA.^2  +  (-(Dec - Par1Dec(2)).*Par1Dec(1) + Par1Dec(1).^2)./Std1Dec.^2;
    
    
                TotalPM  = Par1RA
                Par1RA(1)./(Std1RA./sqrt(Ngood))
            end

        end
    end


end
