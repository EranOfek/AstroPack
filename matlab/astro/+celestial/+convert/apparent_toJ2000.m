function [OutRA, OutDec, Alt, Refraction, Aux] = apparent_toJ2000(RA, Dec, JD, Args)
    % Apparent coordinates to J2000 coordinates (approximation!)
    %     Using one iteration of celestial.convert.j2000_toApparent (good
    %     to ~1" accuracy).
    %     Will be fixed in the future.
    % Input  : See celestial.convert.j2000_toApparent for arguments.
    % Output : See celestial.convert.j2000_toApparent for output.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [OutRA, OutDec, Alt, Refraction, Aux] = celestial.convert.apparent_toJ2000(180, 0, celestial.time.julday([1 1 2024]))
    %          [OutRA, OutDec,Alt,~,Aux] = celestial.convert.j2000_toApparent(180, 20, celestial.time.julday([1 1 2024]))
    %          [OutRA1, OutDec1, ~, ~, Aux1] = celestial.convert.apparent_toJ2000(OutRA, OutDec, celestial.time.julday([1 1 2024]))

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
        Args.TypeLST           = 'a';
        
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

    
    ConvFactor      = convert.angular(Args.InUnits, 'deg');
    Aux.RA_AppDist  = RA .*ConvFactor;
    Aux.Dec_AppDist = Dec .*ConvFactor;
    Aux.HA_AppDist  = celestial.convert.convert_ha(Aux.RA_AppDist, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                 'Long',Args.GeoPos(1),...
                                                 'LongUnits','rad',...
                                                 'TypeLST','a',...
                                                 'OutRange','pi');
    
    % apply Distortion
    if Args.ApplyDistortion
        % apply geometric distortion of mount - mount specific - not
        % astrophysical

        if isnumeric(Args.InterpHA)
            Aux.HA_App  = Aux.HA_AppDist - Args.InterpHA;
        elseif (isa(Args.InterpHA, 'struct') || isa(Args.InterpHA,'scatteredinterpolant')) && ~isempty(Args.InterpHA.Points)
            Aux.HA_App  = Aux.HA_AppDist  - Args.InterpHA(Aux.HA_AppDist, Aux.Dec_AppDist);
        else
            Aux.HA_App  = Aux.HA_AppDist;
        end
        if isnumeric(Args.InterpDec)
            Aux.Dec_App = Aux.Dec_AppDist - Args.InterpDec;
        elseif (isa(Args.InterpDec, 'struct') || isa(Args.InterpDec,'scatteredinterpolant')) && ~isempty(Args.InterpDec.Points)
            Aux.Dec_App = Aux.Dec_AppDist - Args.InterpDec(Aux.HA_AppDist, Aux.Dec_AppDist);
        else
            Aux.Dec_App  = Aux.Dec_AppDist;
        end

        % convert HA to RA
        Aux.RA_App = celestial.convert.convert_ha(Aux.HA_App, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                 'Long',Args.GeoPos(1),...
                                                 'LongUnits','rad',...
                                                 'TypeLST','a',...
                                                 'OutRange','2pi');


    else
        %
        Aux.HA_App  = Aux.HA_AppDist;
        Aux.RA_App  = Aux.RA_AppDist;
        Aux.Dec_App = Aux.Dec_AppDist;
        
    end
    
    % subtract shift
    Aux.RA_App  = Aux.RA_App  - Args.ShiftRA./cosd(Aux.Dec_App);
    Aux.Dec_App = Aux.Dec_App - Args.ShiftDec;
    
    % Az/Alt/Airmass
    [Aux.Az_App, Aux.Alt_App] = celestial.coo.hadec2azalt(Aux.HA_App, Aux.Dec_App, Args.GeoPos(2).*RAD, 'deg');
    Aux.AirMass = celestial.coo.hardie((90-Aux.Alt_App)./RAD);
    
    % apply inverse refraction
    % THIS SECTION RETURNS WEIRD RESULTS
    % THEREFORE running with apply refraction ...
    %
    % if Args.ApplyRefraction
    %     [Aux.Alt_App] = celestial.coo.invRefraction(Aux.Alt_App,'Wave',Args.Wave,...
    %                                                    'InUnits','deg',...
    %                                                    'OutUnits','deg',...
    %                                                    'T',Args.Temp,...
    %                                                    'P',Args.Pressure,...
    %                                                    'Pw',Args.Pw);
    % 
    %     % update: HA, Dec
    %     [Aux.HA_App,Aux.Dec_App] = celestial.coo.azalt2hadec(Aux.Az_App, Aux.Alt_App, Args.GeoPos(2).*RAD, 'deg');
    %     % and RA
    % 
    %     Aux.RA_App = celestial.convert.convert_ha(Aux.HA_App, JD, 'InUnits','deg', 'OutUnits','deg',...
    %                                              'Long',Args.GeoPos(1),...
    %                                              'LongUnits','rad',...
    %                                              'TypeLST','a',...
    %                                              'OutRange','2pi');
    % end

    % Apparent to J2000
    CellArgs = namedargs2cell(Args);
    [RA1, Dec1, Alt, Refraction, AuxJ] = celestial.convert.j2000_toApparent(Aux.RA_App, Aux.Dec_App, JD, CellArgs{:},...
                                                                            'ApplyRefraction',Args.ApplyRefraction,...
                                                                            'ApplyDistortion',false, 'ShiftRA',0, 'ShiftDec',0);

    
    D_RA  = RA1 - Aux.RA_App;
    D_Dec = Dec1 - Aux.Dec_App;
    
    Aux.RA_J2000  = Aux.RA_App - D_RA;
    Aux.Dec_J2000 = Aux.Dec_App - D_Dec;
    Aux.HA_J2000  = celestial.convert.convert_ha(Aux.RA_J2000, JD, 'InUnits','deg', 'OutUnits','deg',...
                                                                 'Long',Args.GeoPos(1),...
                                                                 'LongUnits','rad',...
                                                                 'TypeLST','a',...
                                                                 'OutRange','pi');
                                
    
    OutRA  = Aux.RA_J2000;
    OutDec = Aux.Dec_J2000;
    
%     % AppDist
%     Aux.RA_AppDist  = RA;
%     Aux.Dec_AppDist = Dec;
%     Aux.HA_AppDist  = celestial.convert.convert_ha(Aux1.RA_AppDist, JD, 'InUnits','deg', 'OutUnits','deg',...
%                                                                  'Long',Args.GeoPos(1),...
%                                                                  'LongUnits','rad',...
%                                                                  'TypeLST','a',...
%                                                                  'OutRange','pi');
%                                                              
%                                                              
%                                                              
%     
%     
%     DRA  = RA1 - RA;
%     DDec = Dec1 - Dec;
% 
%     OutRA  = RA - DRA;
%     OutDec = Dec - DDec;
% 
%     FN = fieldnames(Aux1);
%     Nfn = numel(FN);
%     for Ifn=1:1:Nfn
%         switch lower(FN{Ifn}(1:2))
%             case 'de'
%                 DD = Aux1.(FN{Ifn}) - Dec;
%                 Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
%             case 'ra'
%                 DD = Aux1.(FN{Ifn}) - RA;
%                 Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) - 2.*DD;
%                 Aux.(FN{Ifn}) = mod(Aux.(FN{Ifn}), 360);
%             case 'ha'
%                 TempFN = FN{Ifn};
%                 TempFN(1:2) = 'RA';
%                 II = find(strcmp(FN, TempFN));
%                 DD = Aux1.(FN{II}) - RA;
%                 Aux.(FN{Ifn}) = Aux1.(FN{Ifn}) + 2.*DD;
%             otherwise
%                 % skip
%         end
%     end
% 
%     % populate Az/Alt
%     %[Aux.Az_App, Aux.Alt_App] = celestial.coo.hadec2azalt(Aux.HA_App,Aux.Dec_App, Args.GeoPos(2).*RAD, 'deg');
%     %Aux.AirMass = celestial.coo.hardie((90-Aux.Alt_App)./RAD);


end
