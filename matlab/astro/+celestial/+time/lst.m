function LST=lst(JD,EastLong,STType)
    % Local Sidereal Time, mean and apparent
    % Package: celestial.time
    % Description: Local Sidereal Time, (mean or apparent), for vector of
    %              JDs and a given East Longitude.
    % Input  : - Vector of JD [days], in UT1 time scale.
    %            If empty, use current JD. Default is [].
    %          - East Longitude in radians. Default is 0.
    %          - Sidereal Time Type,
    %            'm' - IAU1976 Mean sidereal time (default).
    %            'a' - IAU1976 apparent sidereal time.
    %            'MeanIAU2000' - Mean IAU 2000 
    %            'AppIAU2000' - Apparent IAU 2000
    % Output : - vector of LST in fraction of day.
    % Tested : Matlab 5.3
    %     By : Eran O. Ofek                    Aug 1999
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: LST=celestial.time.lst(2451545+[0:1:5]',0);  % LST at Greenwhich 0 UT1
    % Reliable: 1
    %--------------------------------------------------------------------------

    arguments
        JD          = [];
        EastLong    = 0;
        STType      = 'm';
    end
    
    RAD = 180./pi;
    
    if isempty(JD)
        JD = celestial.time.julday();
    end

    % convert JD to integer day + fraction of day
    TJD = floor(JD - 0.5) + 0.5;
    DayFrac = JD - TJD;

    Tu = (TJD - 2451545.0);
    T  = Tu./36525.0;
    

    switch lower(STType)
        case {'m','a'}
            GMST0UT = 24110.54841 + 8640184.812866.*T + 0.093104.*T.*T - 6.2e-6.*T.*T.*T;
            % convert to fraction of day in range [0 1)
            GMST0UT = GMST0UT./86400.0;
            
        case {'meaniau2000','appiau2000'}
            % https://www.aanda.org/articles/aa/pdf/2003/30/aa3487.pdf   
            Theta = 2.*pi.*(0.7790572732640 + 1.00273781191135448.*Tu);
            dT0   = 0;
            GMST0UT = dT0 + Theta + (4612.15739966.*T + ...
                                    1.39667721.*T.^2 - ...
                                    0.00009344.*T.^3 + ...
                                    0.00001882.*T.^4)./(3600.*RAD);
                                
            GMST0UT = GMST0UT./(2.*pi);
        otherwise
            % do nothing
    end
    

    GMST0UT = GMST0UT - floor(GMST0UT);
    LST = GMST0UT + 1.0027379093.*DayFrac + EastLong./(2.*pi);
    LST = LST - floor(LST);

    switch lower(STType)
        case {'m'}
            % do nothing
        case {'a'}
            % calculate nutation
            NutMat = celestial.convert.nutation(JD);
            Obl    = celestial.coo.obliquity(JD);
            EquationOfEquinox = (RAD.*3600).*NutMat(:,1).*cos(Obl)./15;
            LST = LST + EquationOfEquinox./86400;    
        case 'meaniau2000'
            % do nothing
        case 'appiau2000'
            % https://www.aanda.org/articles/aa/pdf/2003/30/aa3487.pdf   
            error('AppIAU2000 is not implemented yet');
        otherwise
            error('Unknown sidereal time type');
    end

end
