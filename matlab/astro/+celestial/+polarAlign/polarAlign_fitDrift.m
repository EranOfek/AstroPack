function [Result] = polarAlign_fitDrift(HA, Dec, RateAlpha, RateDelta, Args)
    % Fit polar align axis shift to tracking errors (drift method).
    % Input  : - HA [deg]
    %          - Dec [deg]
    %          - RA tracking error drift [deg/day]
    %          - Dec tracking error drift [deg/day]
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - A structure array with solution per iteration.
    %            The last entry is for the best high resolution fit.
    % Author : Eran Ofek (2025 Nov) 
    % Example: R=celestial.polarAlign.polarAlign_fitDrift(HA, Dec, RateAlpha, RateDelta);
    %          I=1;surface(R(I).DAz, R(I).DAlt, log10(R(I).Chi2)'); shading interp; colorbar

    arguments
        HA
        Dec
        RateAlpha
        RateDelta
        Args.RateAlphaArcsec   = true;
        Args.Phi               = 30.05;
        Args.DAzRange     = [-1 1];
        Args.DAltRange    = [-1 1];
        Args.Resolution   = 0.05;
        Args.IterDecrease = 5;
        Args.Niter        = 4;

        Args.SiderealRate      = 360.985647;   % [deg/day]  (sidereal rate vs mean solar day)
    end
    ARCSEC_DEG = 3600;
    MIN_DAY    = 1440;

    
    DAzVec      = (Args.DAzRange(1):Args.Resolution:Args.DAzRange(2));
    DAltVec     = (Args.DAltRange(1):Args.Resolution:Args.DAltRange(2));
    DAzVecIter  = DAzVec;
    DAltVecIter = DAltVec;

    for Iiter=1:1:Args.Niter
        
        Ndaz  = numel(DAzVecIter);
        Ndalt = numel(DAltVecIter); 

        Result(Iiter).DAz  = DAzVecIter;
        Result(Iiter).DAlt = DAltVecIter;
        Result(Iiter).Chi2_Alpha = nan(Ndaz,Ndalt);
        Result(Iiter).Chi2_Delta = nan(Ndaz,Ndalt);
        Result(Iiter).Chi2       = nan(Ndaz,Ndalt);

        for Idaz=1:1:Ndaz
            for Idalt=1:1:Ndalt
                DAz  = DAzVecIter(Idaz);
                DAlt = DAltVecIter(Idalt);
    
                [DAlpha, DDelta] = celestial.polarAlign.trackingErrorRates(DAz, DAlt, HA, Dec, 'Phi',Args.Phi, 'OmegaDegPerDay',Args.SiderealRate);  % [deg/day]
                if Args.RateAlphaArcsec
                    DAlpha = DAlpha.*cosd(Dec); 
                end
                %DDelta = DDelta;
    
                Result(Iiter).Chi2_Alpha(Idaz,Idalt) = sum((DAlpha - RateAlpha).^2);
                Result(Iiter).Chi2_Delta(Idaz,Idalt) = sum((DDelta - RateDelta).^2);
                Result(Iiter).Chi2(Idaz,Idalt) = Result(Iiter).Chi2_Alpha(Idaz,Idalt) + Result(Iiter).Chi2_Delta(Idaz,Idalt);
    
            end
        end
        
        [~,MinInd] = tools.math.stat.minnd(Result(Iiter).Chi2);
        BestDAz     = DAzVecIter(MinInd(1));
        BestDAlt    = DAltVecIter(MinInd(2));
        DAzVecIter  = BestDAz + DAzVec./(Args.IterDecrease.^Iiter);
        DAltVecIter = BestDAlt + DAltVec./(Args.IterDecrease.^Iiter);
        Result(Iiter).BestDAz  = BestDAz;
        Result(Iiter).BestDAlt = BestDAlt;

    end
          
end
