function [PWV_cm, TauAod500, Center_Ang, Norm] = atmParFromAirmass(AIRMASS)
    % Degree-2 polynomial fits of median atmospheric parameters vs AIRMASS.
    % Coefficients come from a fit of Simone-pipeline outputs across 835
    % LAST visits (`comparePhotCal_16062026_all.csv`, June 2026), covering
    % AIRMASS 1.28-1.92. Useful as physics-based initial values for
    % nonlinear fits (PhotCalibTrans.calibrate) so lsqnonlin starts closer
    % to the minimum than the flat class defaults would allow.
    %
    % All outputs use codebase-native units:
    %   PWV_cm     - cm precipitable water vapor
    %   TauAod500  - dimensionless aerosol optical depth at 500 nm
    %   Center_Ang - Ångström (CSV values were in nm; here x10)
    %   Norm       - dimensionless (kept optional; Norm is always fit anyway)
    %
    % R^2 of the polynomial fits: PWV 0.37, AOD 0.36, Center 0.59, Norm 0.80.
    % Where R^2 is low, the polynomial is only mildly better than the
    % class default; where it is high (Norm), the parameter gets refit
    % anyway so the initialisation is a nicety, not a necessity.
    % Input  : - AIRMASS (scalar >= 1). Any non-finite value returns
    %            the class-default LAST atmospheric values.
    % Output : - PWV_cm     [cm]
    %          - TauAod500  [dimensionless]
    %          - Center_Ang [Angstrom]
    %          - Norm       [dimensionless]
    % Author : D. Kovaleva (July 2026)
    % Example: [PWV, AOD, CenterAng, Norm] = astro.transmission.atmParFromAirmass(1.5);
    %          fprintf('At AM=1.5: PWV=%.2f cm, AOD=%.4f, Center=%.0f A, Norm=%.3f\n', ...
    %              PWV, AOD, CenterAng, Norm);

    arguments
        AIRMASS double   % scalar or vector; each element must be >= 1
    end

    AIRMASS = AIRMASS(:);   % column shape

    % --- Degree-2 fits (highest-order first) ---
    % polyval is elementwise on the AIRMASS vector, so per-source
    % initialisation drops in naturally (call with an N-vector, receive
    % four N-vectors back).
    PWV_cm     = polyval([  8.24,  -23.49,   18.08 ],   AIRMASS);
    TauAod500  = polyval([ -0.0410,   0.1163,   0.001778], AIRMASS);
    Center_nm  = polyval([  76.35, -220.0,    725.0  ],   AIRMASS);
    Center_Ang = Center_nm * 10;
    Norm       = polyval([ -1.797,   5.108,   -3.161 ],   AIRMASS);

    % --- Class-default fallback for any bad AIRMASS entry ---------------
    % Preserves the previous scalar behaviour where a bad AM returned
    % predefSeqCompositeFun's constant defaults; here it is applied
    % element-wise so a mixed-good/bad vector still gets valid entries.
    Bad = ~isfinite(AIRMASS) | AIRMASS < 1;
    if any(Bad)
        PWV_cm(Bad)     = 1.4;
        TauAod500(Bad)  = 0.084;
        Center_Ang(Bad) = 5709.73;
        Norm(Bad)       = 0.5;
    end

    % --- Physical clamps (predefSeqCompositeFun bounds) -----------------
    PWV_cm     = max(0.0,   min(10.0,   PWV_cm));
    TauAod500  = max(0.0,   min(1.0,    TauAod500));
    Center_Ang = max(3000,  min(10000,  Center_Ang));
    Norm       = max(1e-6,  min(10.0,   Norm));
end
