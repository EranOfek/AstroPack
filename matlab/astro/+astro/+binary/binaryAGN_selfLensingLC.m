function [Result] = binaryAGN_selfLensingLC(Args)
    % Binary AGN accretion disk self lensing light curve
    % Input  : * ...,key,val,... 
    %          See code.
    % Output : - A structure with results.
    % Author : Eran Ofek (2024 Jan) 
    % Example: R=astro.binary.binaryAGN_selfLensingLC

    arguments
        Args.M         = 1e8;
        Args.Mdot      = 0.1;
        Args.MassUnits = 'SunM'
        Args.TimeUnits = 'yr';
        Args.Rin       = 10;  % Or vector of R
        Args.Rout      = 300;
        Args.RUnits    = 'rs';
        Args.Nstep     = 100;
        %Args.Wave      = (5000:100:6000).'; %(4000:100:5000).'; %(100:100:1e4).';  % Ang        
        Args.Wave      = (4000:100:5000).'; %(4000:100:5000).'; %(100:100:1e4).';  % Ang        
        %Args.Wave      = (1000:100:2000).'; %(100:100:1e4).';  % Ang        

        Args.Sep       = 0.1.*constant.pc;  % cm
        Args.Dl        = 1e9; % pc

        Args.ImpactPar  = 0;
        Args.BetaVec    = (0:0.1:1)';  % [src rad units]
        Args.Nsim       = 1e6;
    end
    

    [IntegratedSpec, Wave, VecR, T, Ibb] = astro.spec.accretionDiskSpec('M',Args.M,...
                                                                            'Mdot',Args.Mdot,...
                                                                            'MassUnits',Args.MassUnits,...
                                                                            'TimeUnits',Args.TimeUnits,...
                                                                            'Rin',Args.Rin,...
                                                                            'Rout',Args.Rout,...
                                                                            'RUnits',Args.RUnits,...
                                                                            'Nstep',Args.Nstep,...
                                                                            'Wave',Args.Wave);

    % two MBH with the same mass:
    K=celestial.Kepler.kepler3law(2.* Args.M.*constant.SunM, 'a', Args.Sep); %3600);
    Dls = K.a./constant.pc;

    % accretion disk profile - face on
    FluxAsFunRadius = sum(Ibb, 1);
    accDiskProfile = @(RR) interp1(VecR./max(VecR), FluxAsFunRadius, RR)./FluxAsFunRadius(2);
    RR = (0:0.05:1)';
    FunLimbMatrix  = [RR, accDiskProfile(RR)];
    IsN = isnan(FunLimbMatrix(:,2));
    FunLimbMatrix(IsN,2) = 0;

    % set LensRad to Rs
    Rs   = 2.*constant.G.*Args.M./(constant.c.^2);

    % the two BH have the same mass:
    Beta = sqrt(Args.ImpactPar.^2 + Args.BetaVec.^2);
    [TM,ResSL]=astro.binary.self_microlensing(Beta, 'Dls',Dls, 'Dl',Args.Dl, 'Mass',Args.M, 'SrcRad',max(VecR), 'SrcRadUnits','cm',...
                                                            'FunLimb', FunLimbMatrix, 'LensRad',Rs, 'Algo','2d','Nsim',Args.Nsim);

    % Impact par to time
    Time = Args.BetaVec.*max(VecR)./K.v./(86400.*365.25);  % [yr]
    %plot(Time, TM)
    %hold on;

    Result.Time          = Time(:);
    Result.Amplification = TM(:);
    Result.Beta          = Beta;
    Result.ImpactPar     = Args.ImpactPar;
    Result.ResSL         = ResSL;
    Result.VecR          = VecR;
    Result.Rs            = Rs;
end
    
    
    