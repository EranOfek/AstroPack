function [OutRA, OutDec, Alt, Refraction, Aux] = apparent_toJ2000(RA, Dec, JD, Args)
    % Apparent coordinates to J2000 coordinates (approximation!)
    %     Using one iteration of celestial.convert.j2000_toApparent
    %     Will be fixed in the future.
    % Input  : See celestial.convert.j2000_toApparent for arguments.
    % Output : See celestial.convert.j2000_toApparent for output.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [OutRA, OutDec, Alt, Refraction, Aux] = celestial.convert.apparent_toJ2000(180, 0, celestial.time.julday([1 1 2024]))
    %          [OutRA, OutDec,Alt,~,Aux] = celestial.convert.j2000_toApparent(180, 20, celestial.time.julday([1 1 2024]))
    %          [OutRA1, OutDec1, ~, ~, Aux1] = celestial.convert.apparent_toJ2000(OutRA, OutDec, celestial.time.julday([1 1 2024])

    arguments
        RA
        Dec
        JD                     = celestial.time.julday();
        Args.InUnits           = 'deg';
        Args.Epoch             = 2000;
        Args.EpochUnits        = 'J';
        Args.OutUnits          = 'deg';
        
        Args.OutEquinox        = [];
        Args.OutEquinoxUnits   = 'JD';
        Args.OutMean           = false;
        
        Args.PM_RA             = 0;
        Args.PM_Dec            = 0;
        Args.Plx               = 1e-2;
        Args.RV                = 0;
        
        Args.INPOP             = celestial.INPOP.init({'Ear'});
        
        Args.GeoPos            = [[35 30].*pi./180, 415];   % [rad rad m]
        Args.TypeLST           = 'm';
        
        Args.Server            = @VO.name.server_simbad;
        
        Args.TimeScale         = 'TDB';
        
        Args.ApplyAberration logical = true;
        Args.ApplyRefraction logical = true;
        Args.Wave              = 5000;  % [A]
        Args.Temp              = 15;
        Args.Pressure          = 760;
        Args.Pw                = 8;                           

        Args.ShiftRA                  = 0;
        Args.ShiftDec                 = 0;

        Args.ApplyDistortion logical  = false;
        Args.InterpHA                 = 0;  % numeric [deg] or interpolation function [deg]
        Args.InterpDec                = 0;
        
    end
    RAD       = 180./pi;

    if nargout>4
        Aux.RA_App = NaN;
        Aux.HA_App = NaN;
        Aux.Dec_App = NaN;
        Aux.RA_AppDist = NaN;
        Aux.HA_AppDist = NaN;
        Aux.Dec_AppDist = NaN;
        Aux.Alt_App     = NaN;
        Aux.Az_App      = NaN;
       
    end

    CellArgs = namedargs2cell(Args);
    [RA1, Dec1, Alt, Refraction, Aux1] = celestial.convert.j2000_toApparent(RA, Dec, JD, CellArgs{:});

    DRA  = RA1 - RA;
    DDec = Dec1 - Dec;

    OutRA  = RA - DRA;
    OutDec = Dec - DDec;

    FN = fieldnames(Aux1);
    Nfn = numel(FN);
    for Ifn=1:1:Nfn
        switch lower(FN{Ifn}(1:2))
            case 'de'
                DD = Aux1.(FN{Ifn}) - Dec;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
            case 'ra'
                DD = Aux1.(FN{Ifn}) - RA;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
                Aux.(FN{Ifn}) = mod(Aux.(FN{Ifn}), 360);
            case 'ha'
                TempFN = FN{Ifn};
                TempFN(1:2) = 'RA';
                II = find(strcmp(FN, TempFN));
                DD = Aux1.(FN{II}) - RA;
                Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) + 2.*DD;
            otherwise
                % skip
        end
    end

    % populate Az/Alt
    [Aux.Az_App, Aux.Alt_App] = celestial.coo.hadec2azalt(Aux.HA_App,Aux.Dec_App, Args.GeoPos(2).*RAD, 'deg');
    Aux.AirMass = celestial.coo.hardie((90-Aux.Alt_App)./RAD);


end
