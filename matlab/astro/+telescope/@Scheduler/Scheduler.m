% celestial.Targets class
%       Containers for astronomical targets.
%       Including visibility and scheduling
%       


% Examples:
%
%   S = telescope.Scheduler;
%   S.generateRegularGrid;
%   S.insertColList('Priority',1)
%   
%   % some getters
%   S.RA
%   S.Dec
%   S.TotalExpTime
%   S.LST
%   S.Alt
%   S.EclLat
%   S.GalLat
%   S.GalExt
%   ...
%   S.moonDist
%   S.sun
%   S.sphere_dist(1,1)
%   ...
%
%   S.isVisible
%   S.leftVisibilityTime
%
%   % example for target selection and observations
%   JD = 2451545.5
%   S.initNightCounter;  % init NightCounter (set to 0 at begining of night)
%   W = S.weight(JD)
%   IsV = S.isVisible(JD);
%   Priority = W.*IsV
%   % select target for observation
%   [~,TargetInd] = Priority;
%   if IsV(TargetInd)==0
%       error('No target to observe');
%   end
%
%   % update counters and LastJD
%   SincreaseCounter(TargetInd)
%
%   




classdef Scheduler < Component
    
    properties 
        ListName 
        JD
        List AstroCatalog
        % units deg/days
        Defaults       = struct('MinAlt',15, 'MaxAlt',90, 'MaxHA',120,...
                                'Nexp',20, 'ExpTime',20,...
                                'BasePriority', 0.1,...
                                'Priority', 0.1,...
                                'NightCounter',0, 'GlobalCounter',0, 'MaxCounter',Inf,...
                                'LastJD',0,...
                                'CadenceMethod', 1,...
                                'StartJD',0, 'StopJD',Inf,...
                                'Cadence',1, 'WeightHigh',1.1, 'WeightLow',1.0, 'CadenceRiseTime',0.5, 'WeightDecayTime',10,...
                                'NightCadence',1./24, 'NightWeightHigh',1.1, 'NightWeightLow',1.0, 'NightCadenceRiseTime',0.005, 'NightWeightDecayTime',-100,...
                                'MinMoonDist',-1,...
                                'MinVisibility',2./24);

        MaxSunAlt         = -11.5;
        MinSunDist        = 30;
        CadenceMethodMap  = {"periodic", "continous", "cycleAllNonZero"}
        %CadenceArgs      = 
        AltConstraints  = [0 15; 90 15; 180 15; 270 15; 360 15];
        MoonConstraints = [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 30; 1.0 30];

        % boost priority to ecliptic/galactic latitude

        UseRealTime logical = true;
    end
    
    properties
        
        GeoPos                     = [35.041201 30.053014 415];  %
       
        FileName                   = [];
    end
    
    properties (Hidden)
        RA
        Dec
        TotalExpTime
    
    end
    
    properties (Dependent, Hidden)
        FieldName
        LST
        HA
        EclLon
        EclLat
        GalLon
        GalLat
        Az
        Alt
        AirMass
        ParAng
        GalExt
        SunAz
        SunAlt
        MoonAz
        MoonAlt
        MoonPhase
    end
        
        
    
    
    properties (Hidden)
        ColRA  = 'RA';
        ColDec = 'Dec';
        ColFieldName = 'FieldName';
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
            % Example: T=celestial.Targets
            %           
            %          T=celestial.Targets;
            %          T.generateTargetList('last');
            %          T.write('try1.mat')
            %          T=celestial.Targets('try1.mat');
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
    
    methods % setters/getters
        function set.List(Obj,Tbl)
            % Setter for List
           
            Obj.List = Tbl;
            
            if Obj.List.isColumn(Obj.ColRA)
                Obj.RA   = Obj.List.getCol(Obj.ColRA);
            end
            if Obj.List.isColumn(Obj.ColDec)
                Obj.Dec  = Obj.List.getCol(Obj.ColDec);
            end
            if Obj.List.isColumn('ExpTime') && Obj.List.isColumn('Nexp')
                Val = Obj.List.getCol('ExpTime').*Obj.List.getCol('Nexp');
                
                if numel(unique(Val))==1
                    Obj.TotalExpTime = Val(1);
                else
                    Obj.TotalExpTime = Val;
                end
            end
        end
        
        function Val=get.FieldName(Obj)
            % Getter for FieldName
            
            Val = Obj.List.Catalog.(Obj.ColFieldName);
        end
        
        function Val=get.JD(Obj)
            % Getter for JD property
            
            if Obj.UseRealTime
                Val = celestial.time.julday;
            else
                Val = Obj.JD;
            end
        end
        
        function Val=get.RA(Obj)
            % getter for RA Dependent property
            
            if isempty(Obj.RA)
                if isempty(Obj.List) || Obj.List.sizeCatalog==0
                    error('Catalog is empty');
                end
                ColInd = Obj.List.colname2ind(Obj.ColRA);
                if isnan(ColInd)
                    error('can not find %s column in List',Obj.ColRA);
                end
                Obj.RA = Obj.List.Catalog(:,ColInd);
            end
            Val = Obj.RA;
        end
        
        function Val=get.Dec(Obj)
            % getter for Dec Dependent property
            
            if isempty(Obj.Dec)
                if isempty(Obj.List) || Obj.List.sizeCatalog==0
                    error('Catalog is empty');
                end
                ColInd = Obj.List.colname2ind(Obj.ColDec);
                if isnan(ColInd)
                    error('can not find %s column in List',Obj.ColDec);
                end
                Obj.Dec = Obj.List.Catalog(:,ColInd);
            end
            Val = Obj.Dec;
        end
        
        function Val=get.LST(Obj)
            % Getter for LST - Return LST [deg]
            
            RAD = 180./pi;
            Val     = celestial.time.lst(Obj.JD, Obj.GeoPos(1)./RAD, 'm').*360;  % [deg]
        end
        
        function Val=get.HA(Obj)
            % Getter for HA [deg] (-180 to 180 deg range)
        
            Val       = Obj.LST - Obj.RA;
           
            Val       = mod(Val,360);
            I180      = find(Val>180);
            Val(I180) = Val(I180) - 360;
           
        end
            
            
        function Val=get.EclLon(Obj)
            % getter for EclLon Dependent property
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','e','d','d');
            Val    = OutCoo(:,1);
        end
        
        function Val=get.EclLat(Obj)
            % getter for EclLat Dependent property
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','e','d','d');
            Val    = OutCoo(:,2);
        end
        
        function Val=get.GalLon(Obj)
            % getter for GalLon Dependent property
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','g','d','d');
            Val    = OutCoo(:,1);
        end
        
        function Val=get.GalLat(Obj)
            % getter for GalLat Dependent property
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','g','d','d');
            Val    = OutCoo(:,2);
        end
       
        function Val=get.Az(Obj)
            % getter for Az Dependent property
           
            [Az, ~, ~, ~] = celestial.coo.radec2azalt(Obj.JD, Obj.RA,Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            Val = Az;
        end
        
        function Val=get.Alt(Obj)
            % getter for Alt Dependent property
           
            [~, Alt, ~, ~] = celestial.coo.radec2azalt(Obj.JD, Obj.RA,Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            Val = Alt;
        end
        
        function Val=get.AirMass(Obj)
            % getter for AirMass Dependent property
           
            [~, ~, AM, ~] = celestial.coo.radec2azalt(Obj.JD, Obj.RA,Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            Val = AM;
        end
        
        function Val=get.ParAng(Obj)
            % getter for ParAng Dependent property
           
            [~, ~, ~, ParAng] = celestial.coo.radec2azalt(Obj.JD, Obj.RA,Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            Val = ParAng;
        end
        
        function Val=get.GalExt(Obj)
            % Getter for dependent property GalExt (return E(B-V) [mag]
            
            RAD = 180./pi;
            Val = astro.extinction.sky_ebv(Obj.RA./RAD, Obj.Dec./RAD,'eq');
        end
        
        
    end

    methods (Static)   % utilities
        function [RA,Dec]=radec2deg(RA, Dec)
            % Convert scalar coordinates (hms, sexagesimal, ddeg)  to deg.
            % Input  : - J2000 RA [H M S] or [deg] or sexagesimal.
            %          - J2000 Dec [Sign D M S] or [deg] or sexagesimal.
            % Output : - J2000 RA [deg].
            %          - J2000 Dec [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: telescope.Scheduler.radec2deg(1,1)
            
            RAD = 180./pi;
            
            if ~isnumeric(RA)
                RA = celestial.coo.convertdms(RA, 'SH', 'r');
            else
                if numel(RA)>1
                    RA = celestial.coo.convertdms(RA, 'H', 'r');
                else
                    RA = RA./RAD;
                end
            end
            if ~isnumeric(Dec)
                Dec = celestial.coo.convertdms(Dec, 'SD', 'r');
            else
                if numel(Dec)>1
                    Dec = celestial.coo.convertdms(Dec, 'D', 'r');
                else
                    Dec = Dec./RAD;
                end
            end
        end
    end
    
    
    methods % getter for coordinates and positions
        function varargout = azalt(Obj, JD)
            % Return targets Az/Alt/AirMass/ParAng
            % Input  : - Self.
            %          - JD. If empty, use object JD. Default is [].
            % Output : - Az [deg].
            %          - Alt [deg].
            %          - AirMass.
            %          - Paralactic Angle [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: S.azalt
            
            arguments
                Obj
                JD   = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            
            [varargout{1:nargout}] = celestial.coo.radec2azalt(JD, Obj.RA, Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            %[Az, Alt, AirMass, ParAng] = celestial.coo.radec2azalt(JD, Obj.RA, Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            
        end
        
        function [HA, LST] = halst(Obj, JD)
            % Return targets HA, LST
            % Input  : - Self.
            %          - JD. If empty, use object JD. Default is [].
            % Output : - HA [deg].
            %          - LST [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: S.halst
            
            arguments
                Obj
                JD   = [];
            end
            RAD = 180./pi;
            if isempty(JD)
                JD = Obj.JD;
            end
            
            LST = celestial.time.lst(JD, Obj.GeoPos(1)./RAD, 'm').*360;
            HA       = LST - Obj.RA;
            HA       = mod(HA,360);
            I180     = find(HA>180);
            HA(I180) = HA(I180) - 360;
           
        end
            
        function [SunAz, SunAlt, DSunAz, DSunAlt, SunRA, SunDec, EqOfTime]=sun(Obj, JD)
            % Return Sun position and Az/Alt derivatives
            % Input  : - Self.
            %          - Optional JD, if not given or empty, then use
            %            object JD.
            % Output : - Sun Az (equinox of date) [deg].
            %          - Sun Alt [deg].
            %          - dSunAz/dt [deg/s]
            %          - dSunAlt/dt [deg/s]
            %          - Sun RA (equinox of date) [deg].
            %          - Sun Dec [deg].
            %          - Equation of time [days].
            % Author : Eran Ofek (Jul 2024)
            % Example: [SunAz, SunAlt, DSunAz, DSunAlt, SunRA, SunDec, EqOfTime]=S.sun;
            
            arguments
                Obj
                JD    = [];
            end
            RAD     = 180./pi;
            SEC_DAY = 86400;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            JD1 = JD + 1./SEC_DAY;
            
            
            [SunRA,SunDec,~,~,EqOfTime]=celestial.SolarSys.suncoo(JD, 'a');
            [SunAz, SunAlt] = celestial.coo.radec2azalt(JD, SunRA, SunDec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','rad', 'OutUnits','deg','LSTType','m');
            
            if nargout>2
                [SunRA1,SunDec1,~,~,~]=celestial.SolarSys.suncoo(JD1, 'a');

                EqOfTime = EqOfTime./1440;

                [SunAz1, SunAlt1] = celestial.coo.radec2azalt(JD1, SunRA, SunDec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','rad', 'OutUnits','deg','LSTType','m');

                DSunAz  = SunAz1 - SunAz;
                DSunAlt = SunAlt1 - SunAlt;

                SunRA  = SunRA.*RAD;
                SunDec = SunDec.*RAD;
            end

        end
        
        function Dist=sphere_dist(Obj, RA, Dec)
            % Angular distance to a given position
            % Input  : - Self.
            %          - J2000 RA [H M S] or [deg] or sexagesimal.
            %          - J2000 Dec [Sign D M S] or [deg] or sexagesimal.
            % Output : Ang. distance to all targets [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: S.sphere_dist(100,20)
            %          S.sphere_dist([12 0 0],[-1 20 0 0])
            %          S.sphere_dist('06:00:00','+57:00:00');
            
            
            RAD = 180./pi;
            
            [RA, Dec] = telescope.Scheduler.radec2deg(RA, Dec);
            
            Dist = celestial.coo.sphere_dist_fast(RA, Dec, Obj.RA./RAD, Obj.Dec./RAD);
            Dist = Dist.*RAD;
            
        end
        
        function [MoonRA, MoonDec]=moonEqCoo(Obj, JD)
            % Return Moon RA/Dec
            % Input  : - Self.
            %          - JD. If empty, or not given, then use object JD.
            %            Default is [].
            % Output : - Moon RA (J2000) [deg].
            %          - Moon Dec (J2000) [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: S.moonEqCoo
            
            arguments
                Obj
                JD       = [];
            end
            RAD     = 180./pi;
            SEC_DAY = 86400;
            
            if isempty(JD)
                JD = Obj.JD;
            end
                        
            [MoonRA, MoonDec] = celestial.SolarSys.mooncool(JD, Obj.GeoPos(1:2), 'b');
            MoonRA  = MoonRA.*RAD;
            MoonDec = MoonDec.*RAD;
        end
        
        function [MoonAz, MoonAlt]=moonAzAlt(Obj, JD)
            % Return Moon Az/Alt
            % Input  : - Self.
            %          - JD. If empty use object JD.
            %            Default is empty.
            % Output : - Approximate Moon Az [deg]. (not precessed to eq.
            %            of date).
            %          - Approximate Moon Alt [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: [MoonRA, MoonDec] = S.moonAzAlt;
            
            arguments
                Obj
                JD   = [];
            end
            RAD     = 180./pi;
            SEC_DAY = 86400;
            
            if isempty(JD)
                JD = Obj.JD;
            end
        
            [MoonRA, MoonDec] = moonEqCoo(Obj, JD);
            [MoonAz, MoonAlt] = celestial.coo.radec2azalt(JD, MoonRA, MoonDec, 'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            
        end
        
        function [MoonIllum, MoonPhase]=moonIllum(Obj, JD)
            % Return Moon phase and illumination fraction
            % Input  : - Self.
            %          - JD. If empty use object JD.
            %            Default is empty.
            % Output : - Moon illuminated fraction.
            %          - Moon pahse angle [deg].
            % Author : Eran Ofek (Jul 2024)
            % Example: [MoonRA, MoonDec] = S.moonIllum;
            
            arguments
                Obj
                JD   = [];
            end
            RAD     = 180./pi;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            
            % Moon phase/illumination
            [MoonIllum, MoonPhase] = celestial.SolarSys.moon_illum(JD);
            MoonPhase = MoonPhase.*RAD;
            
        end
            
        function MoonDist=moonDist(Obj, JD)
            % Calculate Moon distance for all targets 
            % Input  : - Self.
            %          - JD. If empty use object JD.
            %            Default is [].
            % Output : - Vector angular distance between Moon and all
            %            targets.
            % Author : Eran Ofek (Jul 2024)
            % Example: DistMoon = S.moonDist;
            
            arguments
                Obj
                JD   = [];
            end
            RAD     = 180./pi;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            
            [MoonRA, MoonDec] = moonEqCoo(Obj, JD);
            
            MoonDist = celestial.coo.sphere_dist_fast(MoonRA./RAD, MoonDec./RAD, Obj.RA./RAD, Obj.Dec./RAD);
            MoonDist = MoonDist.*RAD;
                        
        end
        
        function SunDist=sunDist(Obj, JD)
            % Calculate Sun distance for all targets 
            % Input  : - Self.
            %          - JD. If empty use object JD.
            %            Default is [].
            % Output : - Vector angular distance between Moon and all
            %            targets.
            % Author : Eran Ofek (Jul 2024)
            % Example: SunMoon = S.sunDist;
            
            arguments
                Obj
                JD   = [];
            end
            RAD     = 180./pi;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            
            [~,~,~,~,SunRA, SunDec] = sun(Obj, JD);
                       
            SunDist = celestial.coo.sphere_dist_fast(SunRA./RAD, SunDec./RAD, Obj.RA./RAD, Obj.Dec./RAD);
            SunDist = SunDist.*RAD;
                        
        end
         
        function [FlagCoo] = cooInField(Obj, RA, Dec, Args)
            % Search for fields that contains a list of coordinates
            % Input  : - A celestial.targets object.
            %          - J2000 RA [H M S] or [deg] or sexagesimal.
            %          - J2000 Dec [Sign D M S] or [deg] or sexagesimal.
            %          * ...,key,val,...
            %            'HalfSize' - Half size of box to search around
            %                   each field. Default is [2.1 3.2] (deg).
            % Output : - A vector of logical indicating if the targets in
            %            the celestial.targets object contains one or the RA/Dec.
            % Author : Eran Ofek (Mar 2023)
            % Example: S.cooInField(100,10);
           
            arguments
                Obj
                RA     
                Dec    
                Args.HalfSize   = [2.1 3.2];  % deg
            end
            
            RAD = 180./pi;
            
            [RA, Dec] = telescope.Scheduler.radec2deg(RA, Dec);
            
            RA   = RA./RAD;
            Dec  = Dec./RAD;
            
            HalfSize  = Args.HalfSize./RAD;
            
            Nsrc = Obj.List.sizeCatalog;
            FieldRA  = Obj.RA./RAD;
            FieldDec = Obj.Dec./RAD;
            FlagCoo  = false(Nsrc,1);
            for Isrc=1:1:Nsrc
                FlagCoo(Isrc) = celestial.coo.in_box(RA, Dec, [FieldRA(Isrc), FieldDec(Isrc)], HalfSize);
            end
            
        end
    end
    
    methods % load lists and tables
        function Obj=injectDefaultColumns(Obj)
            % Inject or replace the column in List with the default values
            % in the Defaults property.
            % Input  : - self.
            % Output : - Object with updated List property.
            % Author : Eran Ofek (Jul 2024)
            % Example: injectDefaultColumns(S);

            arguments
                Obj
               
            end

            Nsrc = Obj.List.sizeCatalog;
            DefFN = fieldnames(Obj.Defaults);
            Ndef  = numel(DefFN);
            for Idef=1:1:Ndef
               
                NewData = repmat(Obj.Defaults.(DefFN{Idef}), Nsrc, 1);
                Obj.List = replaceCol(Obj.List, NewData, DefFN{Idef}, Inf, '');
            end


        end

        function Obj=generateRegularGrid(Obj, Args)
            % Generate a regular grid of targets using tile_the_sky
            % Input  : - Self.
            %            'ListName' - List Name to insert to object
            %                   property ListName.
            %                   Default is 'LAST'.
            %            'N_LonLat' - Number of fields along lon/lat.
            %                   Default is [56 42] (for LAST).
            %            'InjectDefaults' - Logical indicating if to
            %                   insert/replace all the columns indicated in the
            %                   Defaults property with their default values.
            %                   Default is true.
            % Output : - Updated object (with updated List).
            % Author : Eran Ofek (Jul 2024)
            % Example: S = celestial.Scheduler;
            %          S.generateRegularGrid;

            arguments
                Obj
                Args.ListName            = 'LAST';
                Args.N_LonLat   = [88 30] %[85 28];  %[56 42];

                %Args.DefaultArgs cell = {};
                
                Args.InjectDefaults logical = true;
            end


            RAD = 180./pi;

            [TileList,TileArea] = celestial.grid.tile_the_sky(Args.N_LonLat(1), Args.N_LonLat(2));
            Nsrc = size(TileList,1);
            
            RA  = TileList(:,1).*RAD;
            Dec = TileList(:,2).*RAD;

            Tbl = [RA, Dec];
            Obj.List = AstroCatalog({[RA, Dec]}, 'ColNames',{Obj.ColRA,Obj.ColDec});
            Obj = Obj.injectDefaultColumns;
            Obj.ListName = Args.ListName;
            
            FieldName = string(num2cell((1:1:Nsrc).'));
            Obj.List.Catalog = [table(FieldName), array2table(Obj.List.Catalog)];
            ColNames         = ['FieldName', Obj.List.ColNames];
            Obj.List.ColNames = ColNames;            

        end
        
        function Obj=loadAndConcat(Obj, AddFileName, Args)
            % Load a mat file containing a telescope.Scheduler object and concat to current object.
            % Input  : - Self.
            %          - FileName containing the object to concat.
            %          * ...,key,val,...
            %            'ErrorIfNotExist' - Default is false.
            % Output : - An updated object.
            % Author : Eran Ofek (Jul 2024)
            
            arguments
                Obj
                AddFileName = [];
                Args.ErrorIfNotExist logical   = false;
            end
            
            if isfile(AddFileName)
                Tmp = io.files.load2(AddFileName);
                if isa(Tmp, 'telescope.Scheduler')
                    Obj.List.Catalog = [Obj.List.Catalog; Tmp.List.Catalog];
                else
                    error('FileName have unsupported type');
                end
            else
                if Args.ErrorIfNotExist
                    error('AddFileName %s does not exist',AddFileName);
                end
                    
            end            
            
        end
    end

    methods % write lists and tables
        function save(Obj, FileName)
            % save the Targets object as a MAT file.
            % Input  : - A Targets object.
            %          - File name.
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.Targets;
            %          T.generateTargetList('last');
            %          T.write('try1.mat')
            
            arguments
                Obj
                FileName       = [];
            end
            
            if isempty(FileName)
               FileName = Obj.FileName;
               if isempty(FileName)
                   error('FileName must be provided');
               end
            end
            
            save('-v7.3', FileName, 'Obj');
        end
        
    end

    
    methods  % setters to Data table
        function Obj = insertColList(Obj, ColName, Val, Index)
            % set Data table column for specific entries
            % Input  : - celestial.Targets object.
            %          - Table column name to set (e.g., 'LastJD').
            %          - Vector of values.
            %          - Vector of indices in which to insert the values.
            %            If empty, then assume insert scalar value for entire
            %            column, or insert vector to entire column.
            %            Default is [].
            % Output : - celestial.Targets in which the Data table is
            %            updated.
            % Author : Eran Ofek (Jul 2024)
            % Example: S = celestial.Scheduler;
            %          S.generateRegularGrid;
            %          S.insertColList('Priority',1)
           
            arguments
                Obj
                ColName
                Val
                Index    = [];
            end
            
            if nargin<3
                error('ColName and Val must be provided');
            end

            Nsrc = Obj.List.sizeCatalog;

            
            ColInd = colname2ind(Obj.List, ColName);
            if isnan(ColInd) && isempty(Index)
                if isempty(Index)
                    if numel(Val)==1
                        Val = repmat(Val, Nsrc, 1);
                    end
                    Obj.List.insertCol(Val, Inf, ColName, '');
                else
                    error('For column insertion index must be empty');
                end
            else
                if isempty(Index)
                    Index = (1:1:Nsrc).';
                end

                Obj.List.Catalog(Index, ColInd) = Val;
            end
            
        end
        
    end
    
    
    methods % visibility
        function [Flag, Alt, AltLimit]=checkAltConstraints(Obj, JD)
            % Check Alt of targets agains the AltConstraints property.
            % Input  : - Self..
            %          - JD. If empty, use object JD. Default is [].
            % Output : - A vector of logical flags (per target) indicating
            %            if each target is above the Alt limit in the
            %            AltConstraints property.
            %          - A vector of the Alt of each source.
            %          - A vector of the AltLimit for each source, ad
            %            interpolated from the AltConstraints property.
            % Author : Eran Ofek (Jul 2024)
            % Example: S.checkAltConstraints
            
            arguments
                Obj
                JD    = [];
            end
            
            [Az, Alt] = Obj.azalt(JD);
            
            AltLimit = interp1(Obj.AltConstraints(:,1), Obj.AltConstraints(:,2), Az);
            Flag     = Alt>AltLimit;
            
        end
        
        function [Flag,MoonIllum,MoonAlt,MinMoonDist]=checkMoonConstraints(Obj, JD)
            % Check Moon distance of targets against the MoonConstraints property (distance/illumination).
            % Input  : - Self.
            %          - JD. If empty, use object JD. Default is [].
            % Output : - A vector of logical flags (per target) indicating
            %            if each target distance from the Moon is larger then
            %            the minium distance interpolated from the
            %            MoonConstraints property (distance as a function
            %            of illumination).
            %          - Moon illuminated fraction.
            %          - Moon Alt [deg].
            %          - Vector of min Moon distance per target, as
            %            interpolated from the MoonConstraints property.
            % Author : Eran Ofek (Jul 2024)
            % Example: S.checkMoonConstraints
           
            arguments
                Obj
                JD   = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            
            MoonDist    = Obj.moonDist(JD);
            [~,MoonAlt] = Obj.moonAzAlt(JD);
            MoonIllum   = Obj.moonIllum(JD);
            
            MinMoonDist = interp1(Obj.MoonConstraints(:,1), Obj.MoonConstraints(:,2), abs(MoonIllum));
            
            Flag        = MoonDist>MinMoonDist | MoonAlt<0;
            
        end
        
        function [Flag]=applyColumnConstraints(Obj, ColName, ColVal, JD)
            % Apply for each column in List, its appropriate constraints
            % Input  : - self.
            %          - Column name.
            %          - Column vector of values. If empty, then extract
            %            from List using ColName. Default is [].
            % Output : - A vector of logicals indicatins (per target)
            %            indicating if the target passes the criterion specified in
            %            the column.
            % Author : Eran Ofek (Jul 2024)
            % Example: S.applyColumnConstraints('MaxHA')
            
            arguments
                Obj
                ColName
                ColVal    = [];
                JD        = [];
            end
            SEC_DAY = 86400;
            
            if isempty(ColVal)
                ColVal = Obj.List.getCol(ColName);
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            
            JD1 = JD + Obj.TotalExpTime./SEC_DAY;  % predicted end of visit
            
            switch ColName
                case 'MinAlt'
                    [~,Alt] = Obj.azalt(JD);
                    [~,Alt1] = Obj.azalt(JD1);
                    Flag = Alt>ColVal & Alt1>ColVal;
                case 'MaxAlt'
                    [~,Alt] = Obj.azalt(JD);
                    Flag = Alt<=ColVal;
                case 'MaxHA'
                    [HA] = Obj.halst(JD);
                    [HA1] = Obj.halst(JD1);
                    
                    Flag = abs(HA)<ColVal & abs(HA1)<ColVal;
                case 'MaxCounter'
                    GlobalCounter = Obj.List.getCol('GlobalCounter');
                    if isnan(GlobalCounter)
                        error('GlobalCounter is missing');
                    end
                    Flag = ColVal>GlobalCounter;
                case 'MinMoonDist'
                    Flag = Obj.moonDist(JD)>ColVal;
                case 'StartJD'
                    Flag = JD>ColVal;
                case 'StopJD'
                    Flag = JD<ColVal;
                case 'MinVisibility'
                    LeftTime = Obj.leftVisibilityTime(JD);
                    NightCounter = Obj.List.getCol('NightCounter');
                    if isnan(NightCounter)
                        error('NightCounter is not available');
                    end
                    Flag     = LeftTime>ColVal | NightCounter>0;
                    
                otherwise
                    % skip
                    Flag = true(Obj.List.sizeCatalog, 1);
            end
                    
            
        end
        
        function [Flag, FlagsIn, Summary]=isVisible(Obj, JD, Args)
            % Check if targets are visible given all their specified criteria
            % Input  : - Self.
            % Output : - A vector of flags (one per target) indicating if
            %            the targets are visible according to all criteria.
            %          - Structure containing vector of flags for each used
            %            constraint.
            %          - A structure with some summary of information.
            % Author : Eran Ofek (Jul 2024)
            % Example: S.isVisible
            
            arguments
                Obj
                JD    = [];
                Args.SkipMinVisibility logical  = false;
            end
            %RAD     = 180./pi;
            SEC_DAY  = 86400;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            
            JD1 = JD + Obj.TotalExpTime./SEC_DAY;
            
            % check Sun altitude
            [~,Summary.SunAlt]= Obj.sun(JD);
            [~,SunAlt1]= Obj.sun(JD1);
            FlagsIn.SunAlt    = Summary.SunAlt<Obj.MaxSunAlt & SunAlt1<Obj.MaxSunAlt;
            
            % check Sun distance
            Summary.SunDist   = Obj.sunDist(JD);
            SunDist1          = Obj.sunDist(JD1);
            FlagsIn.SunDist   = Summary.SunDist>Obj.MinSunDist & SunDist1>Obj.MinSunDist;
            
            % AltConstraints
            [FlagsIn.Alt, Summary.Alt] = Obj.checkAltConstraints(JD);
            [FAlt1]                    = Obj.checkAltConstraints(JD1);
            FlagsIn.Alt                = FlagsIn.Alt & FAlt1;
            
            % MoonConstraints
            [FlagsIn.Moon, Summary.MoonIllum]= Obj.checkMoonConstraints(JD);
            
            % go over all columns in List
            [~,Ncol] = Obj.List.sizeCatalog;
            for Icol=1:1:Ncol
                ColName = Obj.List.ColNames{Icol};
                if ~(Args.SkipMinVisibility && strcmp(ColName, 'MinVisibility'))
                    FlagsIn.(ColName) = Obj.applyColumnConstraints(ColName, [], JD);
                end
            end
            
            % Merge all flags
            FN   = fieldnames(FlagsIn);
            Nfn  = numel(FN);
            Nsrc = Obj.List.sizeCatalog; 
            Flag = true(Nsrc,1);
            for Ifn=1:1:Nfn
                Flag = Flag & FlagsIn.(FN{Ifn});
            end
            
            
        end
        
        
        function VisibilityTime = leftVisibilityTime(Obj, JD, Args)
            % Left visibility time for all targets
            % Input  : - Target object.
            %          - JD. If empty use object JD. Default is [].
            %          * see code
            % Output : - Vector of left time for target visibility [day].
            % Author : Eran Ofek (Jul 2024)
            % Example: S=telescope.Scheduler;
            %          S.generateRegularGrid;
            %          [VisibilityTime] = S.leftVisibilityTime;
            
            arguments
                Obj
                JD             = [];
                Args.TimeRes   = 5./1440;   % time resolution [day]
            end
            RAD     = 180./pi;
            
            if isempty(JD)
                JD = Obj.JD;
            end
            
            [SunCrossingTime, NextIsRise] = telescope.Scheduler.nextSunHorizon(JD, Obj.GeoPos, 'AltThreshold', Obj.MaxSunAlt);
            
            Ntarget = Obj.List.sizeCatalog;
            
            if NextIsRise
                VecJD = (JD:Args.TimeRes:SunCrossingTime).';
                
                Njd   = numel(VecJD);
                VisibilityCounter = zeros(Ntarget,1);
                VisibilityStatus  = true(Ntarget,1);  % become false after source is not visible for the first time
                for Ijd=1:1:Njd
                    [FlagAll] = isVisible(Obj, VecJD(Ijd), 'SkipMinVisibility',true);
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
    end


    methods % counters
        function Obj=increaseCounter(Obj, Ind, LastJD, CountersName, ColLastJD)
            % Increase Global/Night counters for target
            % Input  : - Self.
            %          - Target index, or FieldName.
            %          - Value for LastJD to update in table.
            %            If empty, use current time.
            %            If NaN, do not update.
            %            Default is [].
            %          - Cell array of colun names containing counters to
            %            increase.
            %            Default is {'GlobalCounter', 'NightCounter'}.
            %          - ColLastJD. Default is 'LastJD'.
            % Output : - Updated object.
            % Author : Eran Ofek (Jul 2024)
            % Example: S.increaseCounter("1")
            
            arguments
                Obj
                Ind
                LastJD       = [];
                CountersName = {'GlobalCounter', 'NightCounter'};
                ColLastJD    = 'LastJD';
            end
                        
            if ~isnumeric(Ind)
                Ind = strcmp(Obj.List.Catalog.(Obj.ColFieldName), Ind);
            end
            
            Nc = numel(CountersName);
            for Ic=1:1:Nc
                Obj.List.Catalog.(CountersName{Ic})(Ind) = Obj.List.Catalog.(CountersName{Ic})(Ind) + 1;
            end
            
            if isempty(LastJD)
                LastJD = celestial.time.julday();
            end
            if ~isnan(LastJD)
                Obj.List.Catalog.(ColLastJD)(Ind) = LastJD;
            end
            
        end
        
        function Obj=initNightCounter(Obj)
            % Set NightCounter to 0 for all targets
           
            Nsrc = Obj.List.sizeCatalog;
            Obj.List.Catalog.NightCounter = zeros(Nsrc,1);
        end
    end

    
    methods % targets selection
        
        function [Result,Struct]=getTarget(Obj, ObjectIndex)
            % Get properties of selected targets by index or target name
            % Input  : - A celestial.Targets object.
            %          - Vector of indices, or cell array of target names.
            % Output : - A table with the selected targets.
            %          - A structure array with the selected targets.
            % Author : Eran Ofek (Apr 2022)
            % Example: T=celestial.Targets.generateTargetList('last');
            %          [ResT, ResS] = T.getTarget(1:2)

            arguments
                Obj
                ObjectIndex
            end

            if ~isnumeric(ObjectIndex)
                % assume TargetName is provided
                ObjectIndex = find(strcmp(Obj.FieldName,ObjectIndex));
            end

            Result = Obj.List.Catalog(ObjectIndex,:);
            if nargout>1
                Struct = table2struct(Result);
            end
        end
    end
    
    methods % priority
        function W=weight(Obj, JD, Args)
            % Calculate priority weight (without visibility) for periodic cadence
            % Input  : - Self.
            %          - JD. If empty, use object JD.
            %            Default is [].
            %          * ...,key,val,...
            %            See code.
            % Output : - Cadene Weight (without visibility information)
            % Author : Eran Ofek (Jul 2024)
            % Example: S.weight
           
            arguments
                Obj
                JD      = [];
                Args.ColLastJD  = 'LastJD';
            end
           
            if isempty(JD)
                JD = Obj.JD;
            end
            
            LastJD = Obj.List.Catalog.(Args.ColLastJD);
            
            Nsrc         = Obj.List.sizeCatalog;
            NightCounter = Obj.List.Catalog.NightCounter;
            
            W            = zeros(Nsrc,1);
            Fnc0         = NightCounter==0;
            Inc0         = find(Fnc0);
            In0          = find(~Fnc0);
            
            W(Inc0) = telescope.Scheduler.fermiExpWeight(JD-LastJD(Inc0), 'Cadence', Obj.List.Catalog.Cadence(Inc0),...
                                                            'WeightHigh',Obj.List.Catalog.WeightHigh(Inc0),...
                                                            'WeightLow',Obj.List.Catalog.WeightLow(Inc0),...
                                                            'CadenceRiseTime',Obj.List.Catalog.CadenceRiseTime(Inc0),...
                                                            'WeightDecayTime',Obj.List.Catalog.WeightDecayTime(Inc0));
            
            W(In0)  = telescope.Scheduler.fermiExpWeight(JD-LastJD(In0), 'Cadence', Obj.List.Catalog.NightCadence(In0),...
                                                            'WeightHigh',Obj.List.Catalog.NightWeightHigh(In0),...
                                                            'WeightLow',Obj.List.Catalog.NightWeightLow(In0),...
                                                            'CadenceRiseTime',Obj.List.Catalog.NightCadenceRiseTime(In0),...
                                                            'WeightDecayTime',Obj.List.Catalog.NightWeightDecayTime(In0));
            
            % Add BasePriority
            W = W + Obj.List.Catalog.BasePriority;
            
        end
    end
    
    
    
    
    
    






    




    
    methods % visibility
      
        function [FlagAll, Flag] = isVisible111(Obj, JD, Args)
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
            % Example: T=celestial.Targets.generateTargetList('last');
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
        
            
                            
        
        function [Obj, P, Ind] = calcPriority(Obj, JD, CadenceMethod)
            % Calculate priority for targets in celestial.Targets object.
            %
            % Example: T=celestial.Targets;
            %          T.generateTargetList('last');
            %          [T, Prio] = calcPriority(T, 2451545.5, 'west2east')
            %
            %          T=celestial.Targets.generateTargetList('last');
            %          [lon,lat]=T.ecliptic; F=abs(lat)<5 & T.RA>100 & T.RA<110; T.MaxNobs(~F)=0; T.MaxNobs(F)=Inf;
            %          [~,PP,Ind]=T.calcPriority(2451545.5,'cycle');
            %          T.GlobalCounter(Ind(1))=T.GlobalCounter(Ind(1))+1;
            %          [~,PP,Ind]=T.calcPriority(2451545.5,'cycle');
            %          [~,PP,Ind]=T.calcPriority(2451545.5,'fields_cont');
            
            
            arguments
                Obj
                JD                   = [];
                CadenceMethod        = [];
                %Args
            end
            SEC_DAY = 86400;

            if isempty(JD)
                JD                   = celestial.time.julday;
            end
            
            if ~isempty(CadenceMethod)
                Obj.CadenceMethod = CadenceMethod;
            end
            
            if isempty(Obj.CadenceMethod)
                error('CadenceMethod must be provided either as an argument or as Targets property');
            end
            
            Ntarget = numel(Obj.RA);
            
            switch lower(Obj.CadenceMethod)
                case 'fields_cont'
                    % Given a list of selected fields - observe each field
                    % continously for X hours during Y night
                    
                    [Obj, P, Ind] = Obj.cadence_fields_cont(JD);

                case 'predefined'
                    % observed according to predefined priority (order in
                    % list if no priority given). Switch to next target
                    % when MaxNobs reached.
                    % implemented by Nora
                    
                    [Obj, P, Ind] = Obj.cadence_predefined(JD);
                    
                case 'highestsetting'
                    % observe the highest field that has crossed the Meridian 
                    % (= is setting), fields near pole don't have to be 
                    % setting 
                    % implemented by Nora in May 2023
                    [Obj, P, Ind] = Obj.cadence_highest_setting(JD);
                    
                    
                 case 'highest'
                    % observe the highest field 
                    % implemented by Nora in January 2023
                    [Obj, P, Ind] = Obj.cadence_highest(JD);
                   
                    
                case 'cycle'
                    % observe according to predefined priority (order in
                    % list if no priority given). Move to next field when
                    % NperVisit reached.
                    [Obj, P, Ind] = Obj.cadence_cycle(JD);
                    
                    
                case 'survey'
                    % prioritize target for survey with pre defined
                    % cadence.
                    % The survey have two cadences:
                    % Main cadence time scale (e.g., 1 day)
                    % Repitition time scale (e.g., 30 min)

%                     W = zeros(size(t));
%                     W(t<t0)  = (BaseW + ExtraW)./(1 + exp(-(t(t<t0)-t0)./Soft));
%                     W(t>=t0) = BaseW + ExtraW.*exp(-(t(t>=t0)-t0)./Decay);
% 
%                     W = Obj.PriorityArgs.CadenceFun(t, Obj.PriorityArgs.CadeneFunArgs{:});
                        
                    
                case 'west2east'
                    % priortize targets by the left visibility time,
                    % where the highest priority target is the one with the
                    % shortest visibility time above the Obj.MinNightlyVisibility
                    
                    [Obj, P, Ind]=cadence_west2east(Obj, JD);
                    
                otherwise
                    error('Unknown CadenceMethod option');
            end
            
%             if nargout>1
%                 P = Obj.Priority;
%                 if nargout>2
%                     % return also the indices of targets with priority>0 listed by priority
%                     [SortedP,SI] = sort(P, 'descend');
%                     Flag = SortedP>0;
%                     Ind  = SI(Flag);
%                 end
%             end
            
        end
                
    end
    
    
    
    
    
    
    
    
    
    methods (Static)  % static utilities
        function TargetName = radec2name(RA,Dec, Fun)
            % given RA/Dec [deg] generate names in cell array %03d+%02d
            % Input  : - RA [deg].
            %          - Dec [deg].
            %          - Function for calculating the RA/Dec number.
            %            Default is @round.
            % Output : - Cell array of strings of the format %03d+%02d.
            % Author : Eran Ofek (Dec 2022)
            % Example: celestial.Targets.radec2name(20,10)

            arguments
                RA
                Dec
                Fun = @round;
            end
            
            RA  = mod(RA, 360);

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
                            
        function [Time, IsRise] = nextSunHorizon(JD, GeoPos, Args)
            % look for next Sun horizon crossing (including refraction)
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
            % Example: [Time, IsRise] = telescope.Scheduler.nextSunHorizon
            
            arguments
                JD           = celestial.time.julday;
                GeoPos       = [35 32];
                
                Args.AltThreshold  = -0.83333;  % [deg]
                Args.Step          = 10;    % [min]
            end
            
            MIN_IN_DAY    = 1440;
            
            VecJD = JD + (0:Args.Step:MIN_IN_DAY).'./MIN_IN_DAY;
            
            SunAlt = celestial.Targets.sunAlt(VecJD, GeoPos);
            
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
                       
            Time = tools.find.fun_binsearch(@celestial.Targets.sunAlt, Args.AltThreshold, Range, 0.1./1440, GeoPos);
            
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
            % [RA,Dec]=celestial.Targets.earthShadow(2451545 +(0:0.1:365)');

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
        
        function W=fermiExpWeight(T, Args)
            % fermi-rise exp-decay weight function for cadence priority
            % Input  : - Time now - Time of last observations.
            %          * ...,key,val,...
            %            'Cadence' - Cadence. Default is 0.7.
            %            'WeightHigh' - Max Weight following Fermi
            %                   rise. Default is 1.1
            %            'WeightLow' - Min Weight following the exp.
            %                   decay. Default is 1.
            %            'CadenceRiseTime' - Fermi rise time scale.
            %                   Default is 0.5.
            %            'WeightDecayTime' - Exp. decay time scale.
            %                   Default is 2.
            % Output : - Vector of weights.
            % Author : Eran Ofek (Dec 2022)
            % Example: t=(0:0.01:10)';
            %          W=telescope.Scheduler.fermiExpWeight(t);
           
            arguments
                T
                Args.Cadence          = 0.7;  % Cadence
                Args.WeightHigh       = 1.1;  % WeightHigh
                Args.WeightLow        = 1.0;  % WeightLow
                Args.CadenceRiseTime  = 0.5;  % CadenceRise
                Args.WeightDecayTime  = 2; %-20;    % Use - for rising
            end
            
            Ones = ones(size(T));
            Args.Cadence    = Args.Cadence.*Ones;
            Args.WeightHigh = Args.WeightHigh.*Ones;
            Args.WeightLow  = Args.WeightLow.*Ones;
            Args.CadenceRiseTime = Args.CadenceRiseTime.*Ones;
            Args.WeightDecayTime = Args.WeightDecayTime.*Ones;
                        
            W = Ones;
            FlagR = T<Args.Cadence;
            
            W(FlagR)  = Args.WeightHigh(FlagR)./(1 + exp(-(T(FlagR)-Args.Cadence(FlagR))./Args.CadenceRiseTime(FlagR)));
            W(~FlagR) = Args.WeightLow(~FlagR) + (Args.WeightHigh(~FlagR)-Args.WeightLow(~FlagR)).*exp(-(T(~FlagR)-Args.Cadence(~FlagR))./Args.WeightDecayTime(~FlagR));        
            
        end
        
    end
    
    methods (Static)  % in other files / unitTest
        Result = unitTest
    end 

end
    