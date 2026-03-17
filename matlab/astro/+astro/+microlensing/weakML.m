function [Rshift, Xshift, Yshift, ScanShift, NormShift] = weakML(CenterRA, CenterDec, RA, Dec, Args)
    % Predict the astrometric shift due to microlensing in the weak regime.
    %   The function calculate the astrometric shift due to a microlens
    %   with a specific ThetaE for a list of positions around the
    %   microlens.
    % Input  : - Microlens RA.
    %          - Microlens Dec.
    %          - A vector of RA positions around the mirolens.
    %          - A vector of Dec positions.
    %          * ...,key,val,... 
    %            'CooUnits' - Units of the RA, Dec inputs.
    %                   Default is 'deg'.
    %            'ThetaE' - Einstein Radius (ThetaE) [arcsec].
    %                   Default is 0.018.
    %            'ScanningPA' - Scanninf position angle [rad].
    %                   Default is 0.
    %            'Resultion' - Below this angular distance take into
    %                   acoount both positive and negative images. Otherwise, use
    %                   only positive image [arcsec].
    %                   Default is 0.05.
    % Output : - Radial shift for each source (positive outward) [arcsec].
    %          - X shift (-RA) for each source [arcsec].
    %          - Y shift (+Dec) for eachj source [arcsec].
    %          - Shift in scanning direction [arcsec].
    %          - Shift in normal to the scanning direction [arcsec].
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Rshift, Xshift, Yshift, ScanShift, NormShift] = astro.microlensing.weakML(100,10,100+30./3600,10)

    arguments
        CenterRA
        CenterDec
        RA
        Dec
        Args.CooUnits          = 'deg';
        Args.ThetaE            = 0.018;  % [arcsec]
        Args.ScanningPA        = 0;      % [rad]
        Args.Resolution        = 0.05;   % [arcsec] below that take the weighted avergae of the two images
    end
    RAD = 180./pi;
    ARCSEC_DEG = 3600;

    if strcmp(Args.CooUnits,'deg')
        CenterRA  = CenterRA./RAD;
        CenterDec = CenterDec./RAD;
        RA        = RA./RAD;
        Dec       = Dec./RAD;
    end
        

    % Beta=Dist - impact parameter:
    Beta = celestial.coo.sphere_dist_fast(CenterRA, CenterDec, RA, Dec);
    [PA, Alpha] = celestial.coo.position_angle(CenterRA, CenterDec, RA, Dec);
    ScanningAlpha = pi./2 - Args.ScanningPA;

    BetaAS = Beta.*RAD.*ARCSEC_DEG;  % [arcsec]

    Theta = 0.5.*(BetaAS + sqrt(BetaAS.^2 + 4.*Args.ThetaE.^2));

    FlagComb = BetaAS<Args.Resolution;

    if any(FlagComb)
        % need to take into account the two microlensing images
        ThetaP = 0.5.*(BetaAS + sqrt(BetaAS.^2 + 4.*Args.ThetaE.^2));
        ThetaM = 0.5.*(BetaAS - sqrt(BetaAS.^2 + 4.*Args.ThetaE.^2));
        MuP    = 1./(1 - (Args.ThetaE./ThetaP).^4);
        MuM    = 1./(1 - (Args.ThetaE./ThetaM).^4);

        ThetaC  = (MuP.*ThetaP + MuM.*ThetaM)./(MuP+MuM);
        Theta(FlagComb) = ThetaC(FlagC);
    end
    Rshift    = Theta - BetaAS;  % radial shift [arcsec]
    Xshift    = Rshift.*cos(Alpha);
    Yshift    = Rshift.*sin(Alpha);
    % X/Y shift projected on the scanning direction
    ScanShift     = Rshift.*cos(Alpha - ScanningAlpha);
    % X/Y shift projected on the normal-to-scanning direction
    NormShift     = Rshift.*sin(Alpha - ScanningAlpha);

end
