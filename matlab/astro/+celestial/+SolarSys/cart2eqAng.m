function [RA, Dec, Delta] = cart2eqAng(U, Args)
    % Topocentric cartesian ecliptic (or eq.) to equatorial J2000 RA, Dec
    % Input  : - (U) A 3xN array of topocentric cartesian coordinates 
    %            of the target (consistent units).
    %          * ...,key,val,...
    %            'InputSys' - Input coordinate system:
    %                   'ec' - Ecliptic J2000 (Default).
    %                   'eq' - Equatorial J2000 
    %            'Delta' - An optional Observer-Target distance (same units
    %                   as U). If empty, then calculate from U.
    %                   Default is [].
    %            'Aberration' - Apply abberation of light.
    %                   Default is false.
    %            'E_dotH' - A 3xN array of observer velocity.
    %                   Needed only if Aberration is true.
    %                   Default is [].
    %            'OutUnitsDeg' - A logical indicating if the output angles
    %                   (RA/Dec) are in deg (true), or radians (false).
    %                   Default is true.
    % Output : - A vector of J2000 RA.
    %          - A vector of J2000 Dec.
    %          - A vector of Delta (observer-target distance).
    % Author : Eran Ofek (Nov 2023)
    % Example: [RA, Dec, Delta] = celestial.SolarSys.cart2eqAng(rand(3,5));
    
    arguments
        U                               % Topocentric cartesian ecliptic coo of target
        Args.InputSys            = 'ec';  % 'ec'|'eq'
        Args.Delta               = [];
        Args.Aberration logical  = false;
        Args.E_dotH              = [];  % used only if abber=true
        Args.OutUnitsDeg logical = true;
    end
    RAD = 180./pi;
    
    if isempty(Args.Delta)
        Delta = sqrt(sum(U.^2, 1));
    else
        Delta = Args.Delta;
    end
    
    % ignore light deflection
    if Args.Aberration
        U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta);
    else
        U2 = U;
    end

    switch lower(Args.InputSys)
        case 'ec'
            % Rotate from Ecliptic to Equatorial reference frame
            RotMat = celestial.coo.rotm_coo('E');
            Equatorial_U2 = RotMat * U2;
        case 'eq'
            Equatorial_U2 = U2;
        otherwise
            error('Unknown InputSys option');
    end
    
    RA  = atan2(Equatorial_U2(2,:), Equatorial_U2(1,:));
    Dec = atan(Equatorial_U2(3,:)./sqrt( Equatorial_U2(1,:).^2 + Equatorial_U2(2,:).^2  ));

    RA = mod(RA, 2.*pi);

    if Args.OutUnitsDeg
        RA  = RA.*RAD;
        Dec = Dec.*RAD;
    end

end
