% celestial.scheduling.Targets class
%       Containers for astronomical targets.
%       Including visibility and scheduling
%       



classdef Targets < Component
    properties
        Index
        TargetName cell
        IsSolarSystem logical      = false;
        IsTOO logical              = false;
        IsManual logical           = false;
        RA
        Dec
        
        CadenceMethod                           % 'periodic' | 'continues' | 'west2east'
        Priority                                % baseline priority that multiplies the base priority
        PriorityArgs               = struct('MaxNobs',Inf,...
                                            'InterNightCadence',40./1440,...
                                            'CadenceFun',@celestial.scheduling.fermiexp,...  
                                            'CadeneFunArgs',[1.4, 1, 0.03, 1, 0.5]);  %t0,Decay,Soft,BaseW,ExtraW)
                                                    
        LastJD
        GlobalCounter
        NightCounter
        
        GeoPos                     = [35.041201 30.053014 400];  %
       
        VisibilityArgs             = struct('DecRange',[-90 90],...
                                            'EclipticRange',[-90 90],...
                                            'GalacticRange',[-90 90],...
                                            'AltLimit',30,...
                                            'AMLimit',2,...
                                            'AzAltLimit',[0 30; 90 30; 180 30; 270 30; 360 30],...
                                            'HALimit',120,...
                                            'MinNightlyVisibility',2./24,...
                                            'SunAltLimit',-11.5,...
                                            'MoonDistLimit',[0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30]);
                                        

        FileName                   = [];
    end
    
    methods % constructor
        function Obj = Targets(FileName)
            % Constructor for Targets
            % Input  : - If not given and the FileName property is empty
            %            then will return an empty Targets
            %            object. Otherwise, this, or the FileName property,
            %            is the file name to load.
            %            The file may contain a matlab Targets object.
            % Output : - A Targets object.
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets
            %           
            %          T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          T.write('try1.mat')
            %          T=celestial.scheduling.Targets('try1.mat');
            %          !rm try1.mat
            
            arguments
                FileName = [];
            end
            
            if ~isempty(FileName)
                Obj.FileName = FileName;
            end
            
            if isempty(FileName)
                % return empty object
                
            else
                Obj = io.files.load2(Obj.FileName);
            end
            
        end
    end
    
    methods % read/write        
        function Obj = generateTargetList(Obj, List, Args)
            % Generate a Targets object with target list, including LAST default.
            % Input  : - A Targets object.
            %          - Either a table with [RA(deg), Dec(deg), [Name]]
            %            or a character array.
            %            ['last'] - Default LAST target list.
            %          * ...,key,val,...
            %            'N_LonLat' - Number of fields along lon/lat.
            %                   Default is [56 42] (for LAST).
            %            'Priority' - Vector of priority.
            %                   Default is 1.
            % Output : - A populated Targets object.
            % Author : Eran Ofek (Jan 2022)
            % Example: T = celestial.scheduling.Targets
            %          T.generateTargetList('last')
            
            arguments
                Obj
                List            = 'last';
                Args.N_LonLat   = [56 42];
                Args.Priority   = 1;
            end
                
            
            RAD = 180./pi;
            
            if ischar(List)
                % pre defined list
                
                switch lower(List)
                    case 'last'
                        Obj.VisibilityArgs.DecRange        = [-90 90];
                        
                        [TileList,TileArea] = celestial.coo.tile_the_sky(Args.N_LonLat(1), Args.N_LonLat(2));
                        Obj.RA  = TileList(:,1).*RAD;
                        Obj.Dec = TileList(:,2).*RAD;
                        Ntarget = numel(Obj.RA);
                        
                        Obj.Index = (1:1:Ntarget).';
                        Obj.TargetName = celestial.scheduling.Targets.radec2name(Obj.RA, Obj.Dec);
                        
                        Obj.Priority       = Args.Priority(:).*ones(Ntarget,1);
                        Obj.LastJD         = zeros(Ntarget,1);
                        Obj.GlobalCounter  = zeros(Ntarget,1);
                        Obj.NightCounter   = zeros(Ntarget,1);
                        
                    otherwise
                        error('Unknown List name');
                end
            else
                % List is table of [RA(deg), Dec(deg), [Name]]
               
                Obj.RA  = table2array(List(:,1));
                Obj.Dec = table2array(List(:,2));
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
        
        function write(Obj, FileName)
            % save the Targets object as a MAT file.
            % Input  : - A Targets object.
            %          - File name.
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          T.write('try1.mat')
            
            save('-v7.3', FileName, 'Obj');
        end
        
    end
    
    methods
        function [Lon, Lat] = ecliptic(Obj)
            % Return ecliptic coordinates for targets.
            % Input  : - A Targets object.
            % Output : - Ecliptic longitude [deg].
            %          - Ecliptic latitude [deg].
            % AUthor : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          [Lon, Lat] = T.ecliptic;
            
            RAD = 180./pi;
            
            [Lon, Lat] = celestial.coo.convert_coo(Obj.RA./RAD, Obj.Dec./RAD, 'J2000.0', 'e');
            Lon        = Lon.*RAD;
            Lat        = Lat.*RAD;
            
        end
        
        function [Lon, Lat] = galactic(Obj)
            % Return galactic coordinates for targets.
            % Input  : - A Targets object.
            % Output : - Galactic longitude [deg].
            %          - Galactic latitude [deg].
            % AUthor : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          [Lon, Lat] = T.galactic;
            
            RAD = 180./pi;
            
            [Lon, Lat] = celestial.coo.convert_coo(Obj.RA./RAD, Obj.Dec./RAD, 'J2000.0', 'g');
            Lon        = Lon.*RAD;
            Lat        = Lat.*RAD;
            
        end
        
    end
    
    methods % visibility
        function [Sun] = sunCoo(Obj, JD)
            % Return Sun RA/Dec and geometric Az/Alt
            % Input  : - Targets object.
            %          - JD. Default is current UTC time.
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
            %          - JD. Default is current UTC time.
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
            %          - JD. Default is current UTC time.
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
        
        function [Az, Alt, dAz, dAlt] = azalt(Obj, JD)
            % get Az/Alt for target
            % Input  : - Target object.
            %          - JD. Default is current time.
            % Output : - Az [deg]
            %          - Alt [deg]
            %          - dAz/dt [deg/s]
            %          - dAlt/dt [deg/s]
            % Author : Eran Ofek (Jan 2022)
            % Example: T.generateTargetList('last');
            %          [Az, Alt] = T.azalt
            
            arguments
                Obj
                JD       = celestial.time.julday;
            end
            SEC_IN_DAY = 86400;
            RAD        = 180./pi;
                   
            LST     = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - Obj.RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Obj.Dec./RAD, Obj.GeoPos(2)./RAD);
            Az  = Az.*RAD;
            Alt = Alt.*RAD;
            
            if nargout>2
                JD1     = JD + 1./SEC_IN_DAY;
                LST     = celestial.time.lst(JD1, Obj.GeoPos(1)./RAD, 'a').*360;  % [deg]
                HA      = LST - Obj.RA;
                [Az1,Alt1]= celestial.coo.hadec2azalt(HA./RAD, Obj.Dec./RAD, Obj.GeoPos(2)./RAD);
                Az1  = Az1.*RAD;
                Alt1 = Alt1.*RAD;
                dAz  = Az1 - Az;
                dAlt = Alt1 - Alt;
            end
        end
            
        function [HA, LST] = ha(Obj, JD)
            % get HA and LST for target
            % Input  : - Target object.
            %          - JD. Default is current UTC time.
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
                
        function VisibilityTime = leftVisibilityTime(Obj, JD, Args)
            % Left visibility time for all targets
            % Input  : - Target object.
            %          - JD. Default is current UTC time.
            %          * see code
            % Output : - Vector of left time for target visibility [day].
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          [VisibilityTime] = leftVisibilityTime(T)
            
            arguments
                Obj
                JD              = celestial.time.julday;
                Args.TimeRes    = 2./1440;   % [day]
            end
            
            if isempty(JD)
                JD     = celestial.time.julday;
            end
                        
            [SunSetJD, IsRise] = celestial.scheduling.Targets.nextSunHorizon(JD, Obj.GeoPos, 'AltThreshold', Obj.VisibilityArgs.SunAltLimit);
            
            Ntarget = numel(Obj.RA);
            
            if IsRise
                VecJD = (JD:Args.TimeRes:SunSetJD).';
                
                Njd   = numel(VecJD);
                VisibilityCounter = zeros(Ntarget,1);
                VisibilityStatus  = true(Ntarget,1);  % become false after source is not visible for the first time
                for Ijd=1:1:Njd
                    [FlagAll] = isVisible(Obj, VecJD(Ijd), 'CheckVisibility',false);
                    VisibilityStatus  = VisibilityStatus & FlagAll;
                    VisibilityCounter = VisibilityCounter + FlagAll.*VisibilityStatus;
                end
                VisibilityTime = VisibilityCounter .* Args.TimeRes;
            else
                % night did not started yet
                % set viaibility time to zero
                VisibilityTime = zeros(Ntarget,1);
            end
        end
        
        function [FlagAll, Flag] = isVisible(Obj, JD, Args)
            % Check if Target is visible according to all selection criteria
            %       Selection criteria include:
            %       In Dec range
            %       Above Alt
            %       Below AM
            %       Above Alt(Az)
            %       Sun below ALt
            %       Moon Dist.
            %       HA limits
            % Input  : - A Targets object.
            %          - JD. Default is current UTC time.
            %          * see code
            % Output : - A vector of logical (element per target)
            %            indicating if the target is visible.
            %          - Structure of specific flags.
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          [FlagAll, Flag] = isVisible(T)
            
            arguments
                Obj
                JD     = celestial.time.julday;
                
                Args.MinVisibilityTime       = 1./24;  % [day]
                
                Args.CheckDec logical        = true;
                Args.CheckAlt logical        = true;
                Args.CheckAM logical         = true;
                Args.CheckAzAlt logical      = true;
                Args.CheckSun logical        = true;
                Args.CheckMoon logical       = true;
                Args.CheckHA logical         = true;
                Args.CheckEcl logical        = true;
                Args.CheckGal logical        = true;
                Args.CheckVisibility logical = true;
            end
            
            if isempty(JD)
                JD     = celestial.time.julday;
            end
            
            RAD = 180./pi;
            
            Ntarget = numel(Obj.RA);
            
            if Args.CheckDec
                Flag.DecRange = Obj.Dec>=Obj.VisibilityArgs.DecRange(1) & Obj.Dec<=Obj.VisibilityArgs.DecRange(2);
            else
                Flag.DecRange = true;
            end
            
            [HA, LST] = Obj.ha(JD);
            [Az, Alt] = Obj.azalt(JD); 
            
            if Args.CheckHA
                Flag.HA      = abs(HA) < Obj.VisibilityArgs.HALimit;
            else
                Flag.HA      = true;
            end
            
            if Args.CheckAlt
                Flag.Alt     = Alt>Obj.VisibilityArgs.AltLimit;
            else
                Flag.Alt     = true;
            end
            
            if Args.CheckAM
                AM           = celestial.coo.hardie((90-Alt)./RAD);
                Flag.AM      = AM < Obj.VisibilityArgs.AMLimit;
            else
                Flag.AM      = true;
            end
            
            if Args.CheckAzAlt
                AltLimitOfAz = interp1(Obj.VisibilityArgs.AzAltLimit(:,1),Obj.VisibilityArgs.AzAltLimit(:,2), Az);
                Flag.AzAlt   = Alt>AltLimitOfAz;
            else
                Flag.AzAlt   = true;
            end
            
            if Args.CheckSun
                Sun          = sunCoo(Obj, JD);
                Flag.Sun     = Sun.Alt<Obj.VisibilityArgs.SunAltLimit;
            else
                Flag.Sun     = true;
            end
            
            if Args.CheckMoon
                [MoonDist, Moon] = moonDist(Obj, JD);
                
                if numel(Obj.VisibilityArgs.MoonDistLimit)==1
                    Flag.Moon = MoonDist > Obj.VisibilityArgs.MoonDistLimit;
                else
                    DistLimitOfIllum = interp1(Obj.VisibilityArgs.MoonDistLimit(:,1), Obj.VisibilityArgs.MoonDistLimit(:,2), abs(Moon.Illum));
                    Flag.Moon = MoonDist > DistLimitOfIllum;
                end
            else
                Flag.Moon    = true;
            end
            
            if Args.CheckEcl
                [~, EclLat]   = Obj.ecliptic;
                Nr = size(Obj.VisibilityArgs.EclipticRange,1);
                Flag.Ecliptic = true(Ntarget,1);
                for Ir=1:1:Nr
                    Flag.Ecliptic = Flag.Ecliptic & (EclLat>Obj.VisibilityArgs.EclipticRange(Ir,1) & EclLat<Obj.VisibilityArgs.EclipticRange(Ir,2));
                end
            else
                Flag.Ecliptic = true;
            end
                
            if Args.CheckGal 
                [~, GalLat]   = Obj.galactic;
                Nr = size(Obj.VisibilityArgs.GalacticRange,1);
                Flag.Galactic = true(Ntarget,1);
                for Ir=1:1:Nr
                    Flag.Galactic = Flag.Galactic & (GalLat>Obj.VisibilityArgs.GalacticRange(Ir,1) & GalLat<Obj.VisibilityArgs.GalacticRange(Ir,2));
                end
            else
                Flag.Galactic = true;
            end
            
            if Args.CheckVisibility
                VisibilityTime  = leftVisibilityTime(Obj, JD);
                Flag.Visibility = VisibilityTime > Args.MinVisibilityTime;
            else
                Flag.Visibility = true;
            end
                
            
            FlagFN = fieldnames(Flag);
            FlagAll = true(Ntarget,1);
            for Ifn=1:1:numel(FlagFN)
                FlagAll = FlagAll & Flag.(FlagFN{Ifn});
            end
            
            
        end
        
    end
    
    methods % weights and priority
        function [Obj, P] = calcPriority(Obj, JD, CadenceMethod)
            %
            % Example: T=celestial.scheduling.Targets;
            %          T.generateTargetList('last');
            %          [T, P] = calcPriority(T, 2451545.5, 'west2east')
            
            
            arguments
                Obj
                JD                   = celestial.time.julday;
                CadenceMethod        = [];
                %Args
            end
            
            if ~isempty(CadenceMethod)
                Obj.CadenceMethod = CadenceMethod;
            end
            
            if isempty(Obj.CadenceMethod)
                error('CadenceMethod must be provided either as an argument or as Targets property');
            end
            
            Ntarget = numel(Obj.RA);
            
            switch lower(Obj.CadenceMethod)
                case 'periodic'
                    
                case 'continues'
                    
                case 'predefined'
                    
                case 'west2east'
                    % priortize targets by the left visibility time,
                    % where the highest priority target is the one with the
                    % shortest visibility time above the Obj.MinNightlyVisibility
                    %
                    VisibilityTime = leftVisibilityTime(Obj, JD);
                    % for all above min visibility time, sort by lowest to
                    % highest
                    Npr = 200;
                    Obj.Priority = zeros(Ntarget,1);
                    Obj.Priority(VisibilityTime > Obj.VisibilityArgs.MinNightlyVisibility) = 1;
                                        
                    VisibilityTime(Obj.GlobalCounter > Obj.PriorityArgs.MaxNobs) = 0;
                    
                    [~,SI] = sort(VisibilityTime);
                    Iv     = find(VisibilityTime > Obj.VisibilityArgs.MinNightlyVisibility, Npr, 'first');
                    Nv     = numel(Iv);
                    Obj.Priority(SI(Iv)) = 2 - ((1:1:Nv)' - 1)./(Npr+1);
                    
                    Obj.Priority(Obj.GlobalCounter > Obj.PriorityArgs.MaxNobs) = 0;
                    
                otherwise
                    error('Unknown CadenceMethod option');
            end
            P = Obj.Priority;
            
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
        
        function [Alt, Az, dAlt, dAz] = sunAlt(JD, GeoPos)
            % Return Sun geometric Alt and Az [no refraction] (Static)
            % Input  : - Vector of JD
            %          - Geo pos [Lon, Lat] in deg.
            % Output : - Sun Alt [deg].
            %          - Sun Az [deg]
            %          - Sun dAlt/dt [deg/sec]
            %          - Sun dAz/dt [deg/sec]
            % Author : Eran Ofek (Jan 2022)
            % Example: [Alt, Az] = celestial.scheduling.Targets.sunAlt(2451545, [1 1])
            
            RAD = 180./pi;
            
            [RA, Dec] = celestial.SolarSys.suncoo(JD, 'a');
            RA  = RA.*RAD;
            Dec = Dec.*RAD;
            LST     = celestial.time.lst(JD, GeoPos(1)./RAD, 'a').*360;  % [deg]
            HA      = LST - RA;
            [Az,Alt]= celestial.coo.hadec2azalt(HA./RAD, Dec./RAD, GeoPos(2)./RAD);
            Az  = Az.*RAD;
            Alt = Alt.*RAD;
            
            if nargout>2
                JD1 = JD + 1./86400;
                [RA, Dec] = celestial.SolarSys.suncoo(JD1, 'a');
                RA  = RA.*RAD;
                Dec = Dec.*RAD;
                LST     = celestial.time.lst(JD1, GeoPos(1)./RAD, 'a').*360;  % [deg]
                HA      = LST - RA;
                [Az1,Alt1] = celestial.coo.hadec2azalt(HA./RAD, Dec./RAD, GeoPos(2)./RAD);
                Az1  = Az1.*RAD;
                Alt1 = Alt1.*RAD;
                dAlt = Alt1 - Alt;
                dAz  = Az1 - Az;
            end
            
        end
        
        function [Time, IsRise] = nextSunHorizon(JD, GeoPos, Args)
            % look for next Snn horizon crossing (including refraction)
            % Input  : - JD
            %          - GeoPos [Lon Lat] in deg.
            %            Default is [35 32].
            %          * ...,key,val,...
            %            'AltThreshold' - Default is -0.8333 deg (for
            %                       rise/set).
            %            'Step' - Initial step size [min]. Default is 10.
            % Output : - UTC JD of next sun crossing the horizon.
            %          - A logical flag indicating if rise (true), or set
            %            (false).
            % Author : Eran Ofek (Jan 2022)
            % Example: [Time, IsRise] = celestial.scheduling.Targets.nextSunHorizon
            
            arguments
                JD           = celestial.time.julday;
                GeoPos       = [35 32];
                
                Args.AltThreshold  = -0.83333;
                Args.Step          = 10;    % [min]
            end
            
            MIN_IN_DAY    = 1440;
            
            VecJD = JD + (0:Args.Step:MIN_IN_DAY).'./MIN_IN_DAY;
            
            SunAlt = celestial.scheduling.Targets.sunAlt(VecJD, GeoPos);
            
            DiffSign  = [0; diff(sign(SunAlt))];
            Iapprox   = find(abs(DiffSign) == 2, 1, 'first');
            JDapprox  = VecJD(Iapprox);
            Range     = [JDapprox - 2.*(Args.Step+2)./MIN_IN_DAY, JDapprox + 2.*(Args.Step+2)./MIN_IN_DAY];
            
            if sign(DiffSign(Iapprox))==-1
                % set
                IsRise = false;
            else
                % rise
                IsRise = true;
            end
                       
            Time = tools.find.fun_binsearch(@celestial.scheduling.Targets.sunAlt, Args.AltThreshold, Range, 0.1./1440, GeoPos);
            
        end
        
        function [RA, Dec] = earthShadow(JD, Dist, Args)
            % Calculate the J2000.0 equatorial coordinates of the Earth shadow at a given height
            % Input  : - JD (UT1 time scale).
            %          - Topocentric distance to point in shadow for which to
            %            calculate the position. Default is 42164 km.
            %            If empty, use default.
            %          * ...,key,val,...
            %            'DistUnits' - Default is 'km'.
            %            'GeoPos' - [Lon, Lat, Height] must be in [rad, rad, m].
            %                   Default is [35 32 0]./(180./pi);  % [rad rad m]
            %            'RefEllipsoid' - Default is 'WGS84'.
            %            'OutUnitsDeg' - Output is in degrees. Default is true.
            % Output : - J2000.0 RA of shadow point.
            %          - J2000.0 Dec of shadow point.
            %          - J2000.0 RA of anti Sun direction.
            %          - J2000.0 Dec of anti Sun direction.
            % Author : Eran Ofek (Jan 2022)
            % Example:
            % [RA,Dec]=celestial.scheduling.Targets.earthShadow(2451545 +(0:0.1:365)');

            arguments
                JD
                Dist                      = 42164;
                Args.DistUnits            = 'km';
                Args.GeoPos               = [35 32 0]./(180./pi);  % [rad rad m]
                Args.RefEllipsoid         = 'WGS84';
                Args.OutUnitsDeg logical  = true;
            end
            Cell = namedargs2cell(Args);
            [RA, Dec] = celestial.SolarSys.earthShadowCoo(JD, Dist, Cell{:});
        end
        
    end
    
    methods (Static)  % in other files / unitTest
        Result = unitTest
    end 

end
    
