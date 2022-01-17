


classdef Targets < Component
    properties
        Index
        TargetName cell
        IsSolarSystem logical      = false;
        IsTOO logical              = false;
        IsManual logical           = false;
        RA
        Dec
        
        LastJD
        GlobalCounter
        NightCounter
        
        GeoPos                     = [35.041201 30.053014 400];  %
       
        DecRange                   = [-90 90];
        AltLimit                   = 30;
        AzAltLimit                 = [0 30; 90 30; 180 30; 270 30; 360 30];
        SunAltLimit                = -11.5;    % deg
        MoonDistLimit              = [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30];
    end
    
    methods % constructor
        
    end
    
    
    
    
    methods % read/write        
        function Obj = generateTargetList(Obj, List)
            % Generate a Targets object with target list, including LAST default.
            % Input  : - A Targets object.
            %          - Either a table with [RA(deg), Dec(deg), [Name]]
            %            or a character array.
            %            'last' - Default LAST target list.
            % Output : - A populated Targets object.
            % Author : Eran Ofek (Jan 2022)
            % Example: T = celestial.scheduling.Targets
            %          T.generateTargetList('last')
            
            RAD = 180./pi;
            
            if ischar(List)
                % pre defined list
                
                switch lower(List)
                    case 'last'
                        Obj.DecRange        = [-90 90];
                        
                        [TileList,TileArea] = celestial.coo.tile_the_sky(56,42);
                        Obj.RA  = TileList(:,1).*RAD;
                        Obj.Dec = TileList(:,2).*RAD;
                        Ntarget = numel(Obj.RA);
                        
                        Obj.Index = (1:1:Ntarget).';
                        Obj.TargetName = celestial.scheduling.Targets.radec2name(Obj.RA, Obj.Dec);
                        
                        Obj.LastJD         = zeros(Ntarget,1);
                        Obj.GlobalCounter  = zeros(Ntarget,1);
                        Obj.NightCounter   = zeros(Ntarget,1);
                        
                    otherwise
                        error('Unknown List name');
                end
            else
                % List is table of [RA(deg), Dec(deg), [Name]]
               
                Obj.RA  = table2array(List(:,1));
                Obj.Dec  = table2array(List(:,2));
                Ntarget = numel(Obj.RA);
                        
                Obj.Index = (1:1:Ntarget).';
                
                if size(List,2)>2
                    % name is in 3rd column
                    Obj.TargetName = List(:,3);
                else
                    % default names
                    Obj.TargetName = celestial.scheduling.Targets.radec2name(Obj.RA, Obj.Dec);
                end
                
            end
        end
        

    end
    
    methods % visibility
        function [Sun] = sunCoo(Obj, JD)
            % Return Sun RA/Dec and geometric Az/Alt
            % Input  : - Targets object.
            %          - JD. Default is current time.
            % Output : - A structure with Sun:
            %            .RA [deg]
            %            .Dec [deg]
            %            .Az [deg]
            %            .Alt [deg]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [Sun] = T.sunCoo
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            
            RAD = 180./pi;
            
            [RA, Dec,R,SL,EquationTime] = celestial.SolarSys.suncoo(JD, 'a');
            Sun.RA  = RA.*RAD;
            Sun.Dec = Dec.*RAD;
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Sun.Dec./RAD, Obj.GeoPos(2)./RAD);
            Sun.Az  = Az.*RAD;
            Sun.Alt = Alt.*RAD;
        end
        
        function Moon = moonCoo(Obj, JD)
            % Return Moon phase/RA/Dec and geometric Az/Alt
            % Input  : - Targets Sunobject.
            %          - JD. Default is current time.
            % Output : - A structure with Moon:
            %            .RA [deg]
            %            .Dec [deg]
            %            .Az [deg]
            %            .Alt [deg]
            %            .Illum  - illumination fraction.
            %            .Phase [deg]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [Moon] = T.moonCoo
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            
            RAD = 180./pi;
                        
            [RA, Dec] = celestial.SolarSys.mooncool(JD, Obj.GeoPos(1:2), 'b');
            Moon.RA  = RA.*RAD;
            Moon.Dec = Dec.*RAD;
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Moon.Dec./RAD, Obj.GeoPos(2)./RAD);
            Moon.Az  = Az.*RAD;
            Moon.Alt = Alt.*RAD;
            
            % Moon phase/illumination
            [Moon.Illum, Moon.Phase] = celestial.SolarSys.moon_illum(JD);
            Moon.Phase = Moon.Phase.*RAD;
            
        end
        
        function [MoonDist, Moon] = moonDist(Obj, JD)
            % Calculate Moon distance for all targets 
            % Input  : - Targets Sunobject.
            %          - JD. Default is current time.
            % Output : - Vector of Moon distance [deg] per target.
            %          - A structure with Moon:
            %            .RA [deg]
            %            .Dec [deg]
            %            .Az [deg]
            %            .Alt [deg]
            %            .Illum  - illumination fraction.
            %            .Phase [deg]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [MD, Moon] = T.moonDist
            arguments
                Obj
                JD                 = celestial.time.julday;
            end
            RAD = 180./pi;
            
            Moon = moonCoo(Obj, JD);
            
            MoonDist = celestial.coo.sphere_dist_fast(Obj.RA./RAD, Obj.Dec./RAD, Moon.RA./RAD, Moon.Dec./RAD);
            MoonDist = MoonDist.*RAD;
            
        end
        
        function [Az, Alt] = azalt(Obj, JD)
            % get Az/Alt for target
            % Input  : - Target object.
            %          - JD. Default is current time.
            % Output : - Az [deg]
            %          - Alt [deg]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [Az, Alt] = T.azalt
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            
            RAD = 180./pi;
                   
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - Obj.RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Obj.Dec./RAD, Obj.GeoPos(2)./RAD);
            Az  = Az.*RAD;
            Alt = Alt.*RAD;
        end
            
        function [HA, LST] = ha(Obj, JD)
            % get HA and LST for target
            % Input  : - Target object.
            %          - JD. Default is current time.
            % Output : - HA [deg]
            %          - LST [deg]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [HA, LST] = T.ha
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            
            RAD = 180./pi;
                   
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - Obj.RA;
            
        end
            
        
        function isVisible(Obj, JD, Args)
            %
            
            arguments
                Obj
                JD     = celestial.time.julday;
                
                Args.CheckDec logical   = true;
                Args.CheckAlt logical   = true;
                Args.CheckAzAlt logical = true;
                Args.CheckSun logical   = true;
                Args.CheckMoon logical  = true;
            end
            
            RAD = 180./pi;
            
            if Args.CheckDec
                Flag.DecRange = Obj.Dec>=Obj.DecRange(1) & Obj.Dec<=Obj.DecRange(2);
            else
                Flag.DecRange = true;
            end
            
            [HA, LST] = Obj.ha(JD);
            [Az, Alt] = Obj.azalt(JD); 
            
            if Args.CheckAlt
                Flag.Alt     = Alt>Obj.AltLimit;
            else
                Flag.Alt     = true;
            end
            
            % got here...
            
            
        end
        
    end
    
    methods (Static)  % static utilities
        function TargetName = radec2name(RA,Dec, Fun)
            % given RA/Dec [deg] generate names in cell array %03d+%02d
            
            arguments
                RA
                Dec
                Fun = @round;
            end
            
            Ntarget      = numel(RA);
            TargetName  = cell(Ntarget,1);
            Sign = sign(Dec);
            for Itarget=1:1:Ntarget
                if Sign(Itarget)>0 
                    TargetName{Itarget} = sprintf('%03d+%02d', Fun(RA(Itarget)), Fun(abs(Dec(Itarget))));
                else
                    TargetName{Itarget} = sprintf('%03d-%02d', Fun(RA(Itarget)), Fun(abs(Dec(Itarget))));
                end
            end
        end
    end

end
    
