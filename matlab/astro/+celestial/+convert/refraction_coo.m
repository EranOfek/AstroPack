function [RefRA, RefDec] = refraction_coo(RA, Dec, JD, AtmospherePar, Args)
    % Convert airless RA, Dec to refracted RA, Dec in reference to a reference atmosphere.
    %   Including treatment of reference atmospheric values.
    %   See also: celestial.coo.refraction_coocor,
    %             celestial.convert.refractedCoo
    % Input  : - Airless RA
    %          - Airless Dec
    %          - JD. 
    %            If empty, then use curent UTC time. Default is [].
    %          - Atmospheric parameters.
    %            [Lambda, T, P, Pw]
    %            Default is 
    %          * ...,key,val,...
    %            'AtmosphereRef' - Reference atmospheric parameters.
    %                   If [], then the Atmospheric parameters are absolute.
    %                   If given, then the atmospheric parameters are relative to
    %                   this set.
    %                   Default is [].
    %            'Units' - Units of input and output RA, Dec. ['deg'|'rad'].
    %                   Default is 'deg'.
    %            'GeoPos' - Gedetc position [deg deg m].
    %                   Default is [35.05 30.05 415].
    %            'UnitsPress' - Pressure units: 'mmHg','mbar'.
    %                   Default is 'mbar'
    % Output : - Refracted RA (or HA).
    %          - Refrcated Dec.
    % Author : Eran Ofek (2025 Oct) 
    % Example: [RefRA, RefDec] = celestial.convert.refraction_coo(100,30, 2451545.5
    %          [RefRA, RefDec] = celestial.convert.refraction_coo(100,30, 2451545.5,[5000 20 1000 0],'AtmosphereRef',[5000 20 1000 0])


    arguments
        RA
        Dec
        JD    = [];
        AtmospherePar       = [5000 20 760 8];
        Args.AtmosphereRef  = [];
        Args.Units          = 'deg';
        Args.GeoPos         = [35.04 30.05 415];  % [deg deg m]
        Args.UnitsPress     = 'mbar';
    end
  
    RAD = 180./pi;

    Lam   = AtmospherePar(1);  % [ang]
    Temp  = AtmospherePar(2); % [C]
    P     = AtmospherePar(3); % 760 mm Hg
    Pw    = AtmospherePar(4); % 8 mm Hg

    % Convert pressure units
    ConvP = convert.pressure(Args.UnitsPress, 'mmhg');
    P = P .* ConvP;
    Pw= Pw.* ConvP;

    if ~isempty(Args.AtmosphereRef)
        Args.AtmosphereRef(3:4) = Args.AtmosphereRef(3:4) .* ConvP;
    end

    % convert to radians
    ConvFactor = convert.angular(Args.Units,'rad');
    RA         = RA .* ConvFactor;
    Dec        = Dec.* ConvFactor;

    Lon = Args.GeoPos(1)./RAD;
    Lat = Args.GeoPos(2)./RAD;

    % Calculate LST
    if isempty(JD)
        JD = celestial.time.julday();
    end
        
    % LST
    LST = 2.*pi.*celestial.time.lst(JD, Lon);  % [rad]
    % Hour angle
    HA  = LST - RA;

    % Parallactic angle
    ParAng = celestial.coo.parallactic_angle([RA Dec], LST, Lat);

    % Alt
    [~, Alt] = celestial.coo.hadec2azalt(HA, Dec, Lat, 'rad');

    % atmospheric refraction
    Ref = celestial.coo.refraction_wave(Alt,Lam,Temp,P,Pw);  % [rad]
    if ~isempty(Args.AtmosphereRef)
        % Calculate refraction relative to ref atmosphere
        RefRef = celestial.coo.refraction_wave(Alt,Args.AtmosphereRef(1), Args.AtmosphereRef(2), Args.AtmosphereRef(3), Args.AtmosphereRef(4));  % [rad]
        Ref    = Ref - RefRef;
    end

    % offsets to be added to the true position
    DelAlpha = Ref.*sec(Dec).*sin(ParAng);
    DelDelta = Ref.*cos(ParAng);

    % refracted coordinates
    RefRA  = RA + DelAlpha;
    RefDec = Dec + DelDelta;
    % convert to (0,2pi) range
    RefRA = mod(RefRA, 2.*pi);


    % back to units
    RefRA  = RefRA./ConvFactor;
    RefDec = RefDec./ConvFactor;


end
