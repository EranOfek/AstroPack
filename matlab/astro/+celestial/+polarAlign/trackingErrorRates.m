function [DAlphaDtDegPerDay, DDeltaDtDegPerDay,HA, Dec] = trackingErrorRates(DAz, DAlt, HA, Dec, Args)
    % Calculate an equatorial mount tracking errors as a fun of HA and Dec
    % and as a function of axis shift in Az and Alt.
    % Input  : - Axis shift in Az [deg] measured upward.
    %          - Axis shift in Alt [deg] measured eastward.
    %          - HA [deg] in which to calculate tracking errors.
    %            Default is to use a predefined all sky grid.
    %          - Dec [deg] in which to calculate tracking errors.
    %          * ...,key,val,... 
    %            'Phi' - Mount geodetic latitude [deg]. Default is 30.05298
    %            'OmegaDegPerDay' - sidereal rate vs mean solar day.
    %                   Default is 360.985647
    %            'ApplyRefraction' - Default is false.
    % Output : - HA tracking error [deg/day].
    %          - Dec tracking error [deg/day].
    %          - HA
    %          - Dec
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
        Args.Phi            = 30.05298;
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
        error('ApplyRefraction option doesnt work')
        % Precompute sines/cosines
        sPhi = sind(Args.Phi);  cPhi = cosd(Args.Phi);
        sD   = sind(Dec);   cD   = cosd(Dec);
        sH   = sind(HA);    cH   = cosd(HA);
    
        % Zenith distance
        cosZ = sPhi.*sD + cPhi.*cD.*cH;
        cosZ = max(min(cosZ,1),-1);
        Z = acosd(cosZ);
        sZ = sind(Z);  % >=0
    
        % Parallactic angle components (robust near zenith)
        % sin q = (sin H cos Phi) / sin Z
        % cos q = (sin Phi cos Delt - cos Phi sin Delt cos H) / sin Z
        Sq = (sH .* cPhi) ./ max(sZ, eps);
        Cq = (sPhi.*cD - cPhi.*sD.*cH) ./ max(sZ, eps);
    
        % Refraction and its derivative (deg & deg/deg)
        Rdeg = celestial.coo.refraction(Z./RAD) .* RAD;
        % Central-difference derivative dR/dZ (deg/deg), small step in degrees
        dZ = 1e-3;  % 3.6 arcsec; small but safe
        Rp = celestial.coo.refraction(max(Z + dZ, 0)./RAD) .* RAD;
        Rm = celestial.coo.refraction(max(Z - dZ, 0)./RAD) .* RAD;
        dRdZ = (Rp - Rm) ./ (2*dZ);
    
        % Geometry rates (deg/day)
        dZdt = Args.OmegaDegPerDay .* (cPhi.*cD.*sH) ./ max(sZ, eps);
        dqdt = Args.OmegaDegPerDay .* (cPhi.*cH - sPhi.*tand(Dec)) ./ max(sZ, eps);
    
        

        % Tracking errors due to refraction (deg/day)
        DDeltaDtDegPerDay = DDeltaDtDegPerDay + dRdZ .* dZdt .* Cq - Rdeg .* dqdt .* Sq;
        DAlphaDtDegPerDay = DAlphaDtDegPerDay + (dRdZ .* dZdt .* Sq + Rdeg .* dqdt .* Cq) ./ max(cD, eps);

        DDeltaDtDegPerDay(Z>80 | Z<10) = NaN;
        DAlphaDtDegPerDay(Z>80 | Z<10) = NaN;











        % DeltaHA = 1;  % deg
        % % 1) True topocentric Az/Alt at two instants separated by ΔHA = +1 deg
        % [Az1, Alt1] = celestial.coo.hadec2azalt(HA,         Dec, Args.Phi, 'deg');
        % [Az2, Alt2] = celestial.coo.hadec2azalt(HA+DeltaHA, Dec, Args.Phi, 'deg');
        % dAzdt  = Az2  - Az1;
        % dAltdt = Alt2 - Alt1;
        % 
        % AltRef1 = Alt1 + RAD .* celestial.coo.refraction((90-Alt1)./RAD);
        % AltRef2 = Alt2 + RAD .* celestial.coo.refraction((90-Alt2)./RAD);
        % dAltdtRef = AltRef2 - AltRef1;
        % 
        % [HA1, Dec1] = celestial.coo.azalt2hadec(Az1, Alt1,            Args.Phi, 'deg');
        % [HA2, Dec2] = celestial.coo.azalt2hadec(Az1, Alt1+dAltdtRef,  Args.Phi, 'deg');
        % 
        % 
        % % 4) Robust angle differences (deg) in [-180,180]
        % dHA  = mod(HA2 - HA1 + 180, 360) - 180;
        % dDec = Dec2 - Dec1;  % Dec is not periodic; a simple diff is fine away from poles
        % 
        % dHA(Alt1>80 | Alt1<10)  = NaN;
        % dDec(Alt1>80 | Alt1<10) = NaN;
        % 
        % % 5) Convert finite differences to rates (deg/day) and add to misalignment terms
        % %    Dec_true has zero time derivative, so add full apparent rate:
        % DDeltaDtDegPerDay = DDeltaDtDegPerDay + dDec * 360./DeltaHA;
        % 
        % %    HA_true increases at +360 deg/day, so add only the *excess* over sidereal:
        % DAlphaDtDegPerDay = DAlphaDtDegPerDay + (dHA - DeltaHA) * 360./DeltaHA;
        % 
        % DDeltaDtDegPerDay(Alt1<10) = NaN;
        % DAlphaDtDegPerDay(Alt1<10) = NaN;
    end

end
