function [DAlphaDtDegPerDay, DDeltaDtDegPerDay,HA, Dec] = trackingErrorRates(DAz, DAlt, HA, Dec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: [DAlphaDtDegPerDay, DDeltaDtDegPerDay] = celestial.polarAlign.trackingErrorRates(DAz, DAlt, Ha, Dec)
    %          [DA, DD,HA,Dec] = celestial.polarAlign.trackingErrorRates(0.01, 0);
    % scatter(HA(:),Dec(:),30,DA(:).*3600./1440.*cosd(Dec(:)),'filled'); colorbar; title('dotRA [arcsec/min]')
    % scatter(HA(:),Dec(:),30,DD(:).*3600./1440,'filled'); colorbar; title('dotDec [arcsec/min]')

    arguments
        
        DAz
        DAlt
        HA             = 'grid';
        Dec            = [];
        Args.Phi            = 30.05;
        Args.OmegaDegPerDay = 360.985647;  % deg/day (sidereal rate vs mean solar day)
        Args.ApplyRefraction = false;
    end
    RAD = 180./pi;

    if strcmp(HA, 'grid')
        VecHA  = (-180:15:180);
        VecDec = (-60:5:89);
        [HA, Dec] = meshgrid(VecHA, VecDec);
        
    end

    % Declination drift (deg/day)
    DDeltaDtDegPerDay = Args.OmegaDegPerDay * cosd(Args.Phi) .* (DAz .* cosd(HA) + DAlt .* sind(HA));

    % Right ascension drift (deg/day), PA measured East of North
    DAlphaDtDegPerDay = Args.OmegaDegPerDay * ( ...
        -DAz  .* cosd(Args.Phi) .* sind(HA) .* tand(Dec) + ...
         DAlt .* (cosd(Args.Phi) .* cosd(HA) .* tand(Dec) - sind(Args.Phi)) );


    if Args.ApplyRefraction
        % 1) True topocentric Az/Alt at two instants separated by ΔHA = +1 deg
        [Az1, Alt1] = celestial.coo.hadec2azalt(HA,   Dec, Args.Phi, 'deg');
        [Az2, Alt2] = celestial.coo.hadec2azalt(HA+1, Dec, Args.Phi, 'deg');
    
        % 2) Add refraction to altitude (celestial.coo.refraction expects zenith distance in rad, returns rad)
        % RAD is 180/pi (deg per rad)
        Z1 = (90 - Alt1) ./ RAD;     % rad
        Z2 = (90 - Alt2) ./ RAD;     % rad
        AltRef1 = Alt1 + RAD .* celestial.coo.refraction(Z1);  % deg
        AltRef2 = Alt2 + RAD .* celestial.coo.refraction(Z2);  % deg
    
        %AltRef1 = min(AltRef1,90);
        %AltRef2 = min(AltRef2,90);


        % 3) Convert apparent (refracted) Az/Alt back to apparent HA/Dec
        [HA1, Dec1] = celestial.coo.azalt2hadec(Az1, AltRef1, Args.Phi, 'deg');
        [HA2, Dec2] = celestial.coo.azalt2hadec(Az2, AltRef2, Args.Phi, 'deg');
    
        % 4) Robust angle differences (deg) in [-180,180]
        dHA  = mod(HA2 - HA1 + 180, 360) - 180;
        dDec = Dec2 - Dec1;  % Dec is not periodic; a simple diff is fine away from poles
    
        dHA(Alt1>70)  = 0;
        dDec(Alt1>70) = 0;

        % 5) Convert finite differences to rates (deg/day) and add to misalignment terms
        %    Dec_true has zero time derivative, so add full apparent rate:
        DDeltaDtDegPerDay = DDeltaDtDegPerDay + dDec * 360;
    
        %    HA_true increases at +360 deg/day, so add only the *excess* over sidereal:
        DAlphaDtDegPerDay = DAlphaDtDegPerDay + (dHA - 1) * 360;
    
    end

end
