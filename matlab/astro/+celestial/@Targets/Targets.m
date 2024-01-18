% celestial.Targets class
%       Containers for astronomical targets.
%       Including visibility and scheduling
%       


% Examples:
% This command will generate a list of 3 targets at some RA/Dec
% the rest of the parameters will be taken from a default (see function
% help)
% T = celestial.Targets.createList('RA',[112 21 321].','Dec',[-21,22,3].');
% 
% You can overide the default using - e.g.,
% T = celestial.Targets.createList('RA',[112 21 321].','Dec',[-21,22,3].','ExpTime',10);
% or
% T = celestial.Targets.createList('RA',[112 21 321].','Dec',[-21,22,3].','MaxNobs',[1 2 3].');
% If scalar values are given then assume the same value for all targets
%
% To generate a list based on some all-sky fields:
% T = celestial.Targets.generateTargetList('last');
%
% write a celestial.Targets as mat file
% T.write('FileName.mat');
%
% Coordinate conversiona and information
% (in all cases default time is now):
% [Lon, Lat] = T.ecliptic;
% [Lon, Lat] = T.galactic;
% [MD, Moon] = T.moonDist(2451545);
% [MD, Moon] = T.moonDist; % for current time
% [Sun] = T.sunCoo;
% [Moon] = T.moonCoo;
% [Az, Alt, dAz, dAlt] = T.azalt;          
% [HA, LST] = T.ha;
% [Time, IsRise] = celestial.Targets.nextSunHorizon;      
% [VisibilityTime] = leftVisibilityTime(T);
% [FlagAll, Flag] = isVisible(T); 
%
% Search for fields that contains some coordinates:
% Flag = T.cooInField(100,10, 'HalfSize',[2.1 3.2]);
%
% Calculate priority and select objects:
% [~,~,Ind] = T.calcPriority(2451545.5,'fields_cont');
%
% Get the details RA/Dec, etc for selected targets, in Table, and structure
% format:
% [ResT, ResS] = T.getTarget(Ind);
%
% after an observation is done, to increase the global/night counters for
% target: Ind.
% T.updateCounter(Ind);


classdef Targets < Component
    properties (Dependent)
        RA
        Dec
        Index
        TargetName
        MaxNobs
        LastJD
        GlobalCounter
        NightCounter
        Priority              % baseline priority
        NperVisit
        ExpTime
    end
    
    properties
        Data table   % with columns: Index, TargetName, RA, Dec, DeltaRA, DeltaDec, ExpTime, NperVisit, MaxNobs, LastJD, GlobalCounter, NightCounter
        
        %Index
        %TargetName cell
        %IsSolarSystem logical      = false;
        %IsTOO logical              = false;
        %IsManual logical           = false;
        
        %DeltaRA                   
        %DeltaDec
        %ExpTime
        %NperVisit
        %Filter
        
        CadenceMethod                           % 'periodic' | 'continues' | 'west2east'
        
        PriorityArgs               = struct('InterNightCadence',40./1440,...
                                            'CadenceFun',@celestial.scheduling.fermiexp,...  
                                            'CadeneFunArgs',{1.4, 1, 0.03, 1, 0.5},...
                                            'DeadTime',30);  %t0,Decay,Soft,BaseW,ExtraW)
                                                    
        %LastJD
        %GlobalCounter              = 0;
        %NightCounter               = 0;
        
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
        function Result = get.RA(Obj)
            % getter for RA
            Result = Obj.Data.RA;
        end
        function Result = get.Dec(Obj)
            % getter for Dec
            Result = Obj.Data.Dec;
        end
        function Result = get.Index(Obj)
            % getter for Index
            Result = Obj.Data.Index;
        end
        function Result = get.TargetName(Obj)
            % getter for TargetName
            Result = Obj.Data.TargetName;
        end
        function Result = get.MaxNobs(Obj)
            % getter for MaxNobs
            Result = Obj.Data.MaxNobs;
        end
        function Result = get.LastJD(Obj)
            % getter for LastJD
            Result = Obj.Data.LastJD;
        end
        function Result = get.GlobalCounter(Obj)
            % getter for GlobalCounter
            Result = Obj.Data.GlobalCounter;
        end
        function Result = get.NightCounter(Obj)
            % getter for GlobalCounter
            Result = Obj.Data.NightCounter;
        end
        function Result = get.Priority(Obj)
            % getter for Priority
            Result = Obj.Data.Priority;
        end 
        function Result = get.ExpTime(Obj)
            % getter for ExpTime
            Result = Obj.Data.ExpTime;
        end 
        function Result = get.NperVisit(Obj)
            % getter for NperVisit
            Result = Obj.Data.NperVisit;
        end 
    end
    
    methods  % setters to Data table
        function Obj = setTableProp(Obj, Prop, Val, Index)
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
            % Author : Eran Ofek (Mar 2023)
           
            arguments
                Obj
                Prop
                Val
                Index = [];
            end
            
            if isempty(Index)
                Nline = size(Obj.Data,1);
                if numel(Val)==1
                    Val = Val.*ones(Nline,1);
                    Index = true(Nline,1);
                else
                    Index = true(Nline,1);
                end
            end
            
            Obj.Data.(Prop)(Index) = Val;
            
        end
        
    end
    
    
    methods (Static) % generate lists
        function Obj = createList(Args)
            % Create a Targets object with data specified by user
            % Input  : * ...,key,val,...
            %            'RA' - Mandatory vector of RA [deg].
            %            'Dec' - Mandaotory vector of Dec [deg].
            %            'Index' - If empty, use serial numbers.
            %                   Default is [].
            %            'TargetName' - If empty, use celestial.Targets.radec2name
            %                   to generate names. Default is [].
            %            'DeltaRA' - Default is 0.
            %            'DeltaDec' - Default is 0.
            %            'ExpTime' - Default is 20.
            %            'NperVisit' - Default is 20.
            %            'MaxNobs' - Default is Inf.
            %            'LastJD' - Default is 0.
            %            'GlobalCounter' - Default is 0.
            %            'NightCounter' - Default is 0.
            %            'Priority' - Default is 1.
            % Output : - A populated celestial.Targets object.
            % Author : Eran Ofek (Mar 2023)
            % Example: T=celestial.Targets.createList('RA',[1;2],'Dec',[1;2])

            arguments
                Args.RA            = [];
                Args.Dec           = [];
                Args.Index         = [];
                Args.TargetName    = [];
                Args.DeltaRA       = 0;
                Args.DeltaDec      = 0;
                Args.ExpTime       = 20;
                Args.NperVisit     = 20;
                Args.MaxNobs       = Inf;
                Args.LastJD        = 0;
                Args.GlobalCounter = 0;
                Args.NightCounter  = 0;
                Args.Priority      = 1;
            end
                
            % Index, TargetName, RA, Dec, DeltaRA, DeltaDec, ExpTime, NperVisit, MaxNobs, LastJD, GlobalCounter, NightCounter

            if isempty(Args.RA) || isempty(Args.Dec)
                error('RA and Dec must be supplied');
            end
            
            Ntarget = numel(Args.RA);
            FN      = fieldnames(Args);
            Nf      = numel(FN);

            CellData = cell(1,Nf);
            for If=1:1:Nf
                switch FN{If}
                    case 'Index'
                        if isempty(Args.(FN{If}))
                            Args.(FN{If}) = (1:1:Ntarget).';
                        end
                    case 'TargetName'
                        if isempty(Args.(FN{If}))
                            Args.(FN{If}) = celestial.Targets.radec2name(Args.RA, Args.Dec);
                        end
                    otherwise
                        Args.(FN{If}) = Args.(FN{If}).*ones(Ntarget,1);
                end
                CellData{If} = Args.(FN{If});
               
            end
            Obj      = celestial.Targets;
            Obj.Data = table(CellData{:},'VariableNames',FN);

        end

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
            % Example: T = celestial.Targets.generateTargetList('last')
            
            arguments
                Obj
                List            = 'last';
                Args.N_LonLat   = [88 30] %[85 28];  %[56 42];
                Args.Priority   = 1;
                Args.ExpTime    = 20;
                Args.NperVisit  = 20;
                Args.MaxNobs    = Inf;
            end
                
            RAD = 180./pi;
            
            Obj = celestial.Targets;
            if ischar(List)
                % pre defined list
                
                switch lower(List)
                    case 'last'
                        Obj.VisibilityArgs.DecRange        = [-90 90];
                        
                        [TileList,TileArea] = celestial.coo.tile_the_sky(Args.N_LonLat(1), Args.N_LonLat(2));
                        RA  = TileList(:,1).*RAD;
                        Dec = TileList(:,2).*RAD;

                        Ntarget = numel(RA);
                        
                        % Index, Name, RA(deg), Dec(deg), DeltaRA, DeltaDec, ExpTime, NperVisit, MaxNobs, LastJD, GlobalCounter, NightCounter
                        Obj = celestial.Targets.createList('RA',TileList(:,1).*RAD, 'Dec',TileList(:,2).*RAD, 'ExpTime',Args.ExpTime,'NperVisit',Args.NperVisit,'MaxNobs',Args.MaxNobs,'Priority',Args.Priority);
                        
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
                    Obj.TargetName = celestial.Targets.radec2name(Obj.RA, Obj.Dec);
                end
                
            end
        end
        
    end

    methods % read/write
        function write(Obj, FileName)
            % save the Targets object as a MAT file.
            % Input  : - A Targets object.
            %          - File name.
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.Targets;
            %          T.generateTargetList('last');
            %          T.write('try1.mat')
            
            save('-v7.3', FileName, 'Obj');
        end
        
        function writeFile(Obj, FileName)
            % save the Targets object in a txt file.
            % Input  : - A Targets object.
            %          - File name.
            % Author : Eran Ofek (Feb 2022)
            % Example: T=celestial.Targets;
            %          T.generateTargetList('last');
            %          T.writeFile('LAST_Fields.txt')
           
            arguments
                Obj
                FileName
            end
            
            N = numel(Obj.RA);
            VecN = (1:1:N).';
            FID = fopen(FileName,'w');
            %fprintf(FID, 'RA    Dec    Index   TargetName DeltaRA    DeltaDec   ExpTime    NperVisit  MaxNobs    LastJD GlobalCounter  NightCounter   Priority\n');
            fprintf(FID, '%9.5f %9.5f    %6d    \n', [Obj.RA(:), Obj.Dec(:), VecN(:)].');
            fclose(FID);
            
        end
        
        
            
    end

    methods (Static)
        function Result=readTargetList(Table)
            % Generate an Targets object from a table/file.
            % Input  : - A table object, or a file name to upload into a
            %            table object using the readtable command.
            %            The table must contains the Targets properties you
            %            want to load (e.g., 'RA','Dec',...).
            % Output : - A celestial.Targets object with the populated
            %            fields.
            % Author : Eran Ofek (Mar 2023)

            arguments
                Table
                
            end

            if istable(Table)
                % already Table
            elseif isa(Table,'AstroCatalog')
                error('AstroCatalog not yet supported');
            else
                % assume file input
                % attemp to read using readtable
                Table = io.files.readtable1(Table);
            end

            Cols  = Table.Properties.VariableNames;
            Ncols = numel(Cols);

            Result = celestial.Targets;
            for Icol=1:1:Ncols
                if isprop(Result, Cols{Icol})
                    Result.Data.(Cols{Icol}) = Table.(Cols{Icol});
                end
            end


        end
    end


    methods % coordinates
        function [Lon, Lat] = ecliptic(Obj)
            % Return ecliptic coordinates for targets.
            % Input  : - A Targets object.
            % Output : - Ecliptic longitude [deg].
            %          - Ecliptic latitude [deg].
            % AUthor : Eran Ofek (Jan 2022)
            % Example: T=celestial.Targets;
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
            % Example: T=celestial.Targets;
            %          T.generateTargetList('last');
            %          [Lon, Lat] = T.galactic;
            
            RAD = 180./pi;
            
            [Lon, Lat] = celestial.coo.convert_coo(Obj.RA./RAD, Obj.Dec./RAD, 'J2000.0', 'g');
            Lon        = Lon.*RAD;
            Lat        = Lat.*RAD;
            
        end
        
        function [Flag] = cooInField(Obj, RA, Dec, Args)
            % Search for fields that contains a list of coordinates
            % Input  : - A celestial.targets object.
            %          - A vector of RA [deg].
            %          - A vectot of Dec [deg].
            %          * ...,key,val,...
            %            'HalfSize' - Half size of box to search around
            %                   each field. Default is [2.1 3.2] (deg).
            % Output : - A vector of logical indicating if the targets in
            %            the celestial.targets object contains one or the RA/Dec.
            % Author : Eran Ofek (Mar 2023)
            % Example: T=celestial.Targets.generateTargetList('last');
            %          Flag = T.cooInField(100,10, 'HalfSize',[2.1 3.2])
           
            arguments
                Obj
                RA     % [deg]
                Dec    % [deg]
                Args.HalfSize   = [2.1 3.2];  % deg
            end
            
            RAD = 180./pi;
            
            RA   = RA./RAD;
            Dec  = Dec./RAD;
            
            HalfSize  = Args.HalfSize./RAD;
            
            FieldsRA  = Obj.RA./RAD;
            FieldsDec = Obj.Dec./RAD;
            
            Ntarget = numel(FieldsRA);
            Flag    = false(Ntarget,1);
            for Itarget=1:1:Ntarget
                FlagCoo = celestial.coo.in_box(RA, Dec, [FieldsRA(Itarget), FieldsDec(Itarget)], HalfSize);
                if any(FlagCoo)
                    Flag(Itarget) = true;
                end
            end
            
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
            HA      = LST - Sun.RA;
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
            % Output : - HA [deg] in the ranfe -180 to 180 deg.
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
            
            % convert to -180 to 180 deg range:
            HA = mod(HA, 360);
            FlagL = HA>180;
            HA(FlagL) = HA(FlagL) - 360;
            
        end
                
        function VisibilityTime = leftVisibilityTime(Obj, JD, Args)
            % Left visibility time for all targets
            % Input  : - Target object.
            %          - JD. Default is current UTC time.
            %          * see code
            % Output : - Vector of left time for target visibility [day].
            % Author : Eran Ofek (Jan 2022)
            % Example: T=celestial.Targets;
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
                        
            [SunSetJD, IsRise] = celestial.Targets.nextSunHorizon(JD, Obj.GeoPos, 'AltThreshold', Obj.VisibilityArgs.SunAltLimit);
            
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
            % Example: T=celestial.Targets;
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

        function [Obj, P, Ind]=cadence_highest_setting(Obj, JD)
            % observe the highest field that has crossed the Meridian (= is
            % setting), fields near pole don't have to be setting
            % implemented by Nora in May 2023
                    
            SEC_DAY = 86400;
            
            TimeOnTarget = (Obj.NperVisit+1).*Obj.ExpTime/SEC_DAY; % days
            [FlagAllVisible, ~] = isVisible(Obj, JD,'MinVisibilityTime',TimeOnTarget);
            FlagObserve = (Obj.GlobalCounter<Obj.MaxNobs) & FlagAllVisible;
            
            [~,Alt] = Obj.azalt(JD);
            P = Alt/90+1;
            [HA, LST]=Obj.ha(JD);
            FlagSetting = (HA>0) | (Obj.Dec>75); % fields with Dec>75 don't have to be setting
            
            P = P.*FlagObserve.*FlagSetting;
            [~,Ind] = max(P);
            
        end
            
        
          function [Obj, P, Ind]=cadence_highest(Obj, JD)
            % observe the highest field
            % Author: Nora Strotjohann (January 2023)
                    
            SEC_DAY = 86400;
            
            TimeOnTarget = (Obj.NperVisit+1).*Obj.ExpTime/SEC_DAY; % days
            [FlagAllVisible, ~] = isVisible(Obj, JD,'MinVisibilityTime',TimeOnTarget);
            FlagObserve = (Obj.GlobalCounter<Obj.MaxNobs) & FlagAllVisible;
            
            [~,Alt] = Obj.azalt(JD);
            P = Alt/90+1;
            [HA, ~]=Obj.ha(JD);
            
            P = P.*FlagObserve;
            [~,Ind] = max(P);
            
          end
        
      
        function [Obj, P, Ind]=cadence_predefined(Obj, JD)
            % observed according to predefined priority (order in
            % list if no priority given). Switch to next target
            % when MaxNobs reached.
            % implemented by Nora in May 2023
                    
            SEC_DAY = 86400;
            
            TimeOnTarget = (Obj.NperVisit+1).*Obj.ExpTime/SEC_DAY; % days
            [FlagAllVisible, ~] = isVisible(Obj, JD,'MinVisibilityTime',TimeOnTarget);
            FlagObserve = (Obj.GlobalCounter<Obj.MaxNobs) & FlagAllVisible;
                    
            P = Obj.Priority.*FlagObserve;
            [~,Ind] = max(P);
            
        end
        
            
        function [Obj, P, Ind]=cadence_cycle(Obj, JD)
            % observed according to predefined priority (order in
            % list if no priority given). Switch to next target
            % when NperVisit reached.
            % implemented by Nora in July 2023
                    
            SEC_DAY = 86400;
            
            TimeOnTarget = (Obj.NperVisit+1).*Obj.ExpTime/SEC_DAY; % days
            [FlagAllVisible, ~] = isVisible(Obj, JD,'MinVisibilityTime',TimeOnTarget);
            FlagObserve = (Obj.GlobalCounter<Obj.MaxNobs) & FlagAllVisible;
            %NVisits = Obj.GlobalCounter./Obj.NperVisit;
            %MinVisits = min(NVisits(FlagObserve));
            [MaxLastJD,IndPrevious] = max(Obj.LastJD);
            if MaxLastJD==0
                IndPrevious=length(Obj.Index);
                %fprintf('\nFirst obs.\n')
            %else
            %    fprintf('\nLast observed', IndPrevious)
            end
            newInd = linspace(length(Obj.Index)-1,0,length(Obj.Index)).';
            
            P = (mod(newInd+IndPrevious,length(Obj.Index))+1).*FlagObserve;
            [~,Ind] = max(P);
            
        end
            
            
        function [Obj, P, Ind]=cadence_fields_cont(Obj, JD)
            % Implement "fields_cont" cadence
            %   Given a list of selected fields - observe each field
            %   continously for X hours during Y night
            % Input  : - A celestial.Targets object.
            %          - JD.
            % Output : - The updated celestial.Targets object.
            %          - Priority for all targets.
            %          - Index of target with highest priority.
            % Author : Eran Ofek (Jan 2023)
            
            SEC_DAY = 86400;

            
            MaxNnight = 1;
            Nfind = 1;
            DeadTime  = Obj.PriorityArgs.DeadTime;  % [s]
            UsePriority = true;

            TimeOnTarget = Obj.ExpTime.*Obj.NperVisit + DeadTime;


            Priority         = Obj.Priority;


            GlobalCounter    = Obj.GlobalCounter;
            NightCounter     = Obj.NightCounter;
            [VisibilityTime] = leftVisibilityTime(Obj, JD);
            FlagVisible      = VisibilityTime.*SEC_DAY > TimeOnTarget;

            FlagObserve      = GlobalCounter<Obj.MaxNobs & ...
                               NightCounter<MaxNnight & ...
                               Priority>0 & ...
                               FlagVisible;

            UniquePriority   = sort(unique(Priority),'descend');
            Nup              = numel(UniquePriority);
            MinI             = NaN;
            for Iup=1:1:Nup
                %
                Flag = FlagObserve & Priority==UniquePriority(Iup);

                if sum(Flag)>0
                    VisibilityTimeP = VisibilityTime;

                    VisibilityTimeP(VisibilityTimeP==0) = NaN;
                    VisibilityTimeP(~Flag)      = NaN;

                    [~,MinI] = min(VisibilityTimeP);
                    if ~isnan(MinI)
                        break;
                    end
                end
            end

            % MinI contains the found target index
            Ind = MinI;
            P   = Priority;
            P(Ind) = P(Ind) + 1;

            
            
        end
        
        function [Obj, P, Ind]=cadence_west2east(Obj, JD)
            % Implement the "west2east" cadence.
            %   priortize targets by the left visibility time,
            %   where the highest priority target is the one with the
            %   shortest visibility time above the Obj.MinNightlyVisibility
            % Input  : - A celestial.Targets object.
            %          - JD.
            % Output : - The updated celestial.Targets object.
            %          - Priority for all targets.
            %          - Index of target with highest priority.
            % Author : Eran Ofek (Jan 2023)
            
            VisibilityTime = leftVisibilityTime(Obj, JD);
            % for all above min visibility time, sort by lowest to
            % highest
            Npr = 200;
            Obj.Priority = zeros(Ntarget,1);
            Obj.Priority(VisibilityTime > Obj.VisibilityArgs.MinNightlyVisibility) = 1;

            VisibilityTime(Obj.GlobalCounter > Obj.MaxNobs) = 0;

            [~,SI] = sort(VisibilityTime);
            Iv     = find(VisibilityTime > Obj.VisibilityArgs.MinNightlyVisibility, Npr, 'first');
            Nv     = numel(Iv);
            Obj.Priority(SI(Iv)) = 2 - ((1:1:Nv)' - 1)./(Npr+1);

            Obj.Priority(Obj.GlobalCounter > Obj.MaxNobs) = 0;

            P = Obj.Priority;
            [~,Ind] = max(P);
        end
        
        function [Obj, P, Ind]=cadence_transients_survey(Obj, JD)
            %
            
            arguments
                Obj
                JD    = celestial.time.julday;
            end
            
            % Calculate visibility time left for all targets
            VisibilityTime = leftVisibilityTime(Obj, JD);
            
            % for NightCounter==0: calc priority using daily cadence
            % for NightCounter>0: calc priority using nightly cadence
            
            
            
            % Select all sources that are curretly visible
            % and are visible
            
        end
                            
        
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
    
    methods % targets selection
        
        % OBSOLETE?
        function Ind = selectTarget(Obj, JD, Args)
            % Return the indices of visible targets sorted by priority highest to lowest.
            % Input  : - A Target object.
            %          - JD. Default is current UTC time.
            %          * ...,key,val,...
            %            'isVisibleArgs' - A cell array of additional
            %                   arguments to pass to isVisible.
            %                   Default is {}.
            % Output : - Indices of visible targets, sorted by priority,
            %            highest to lowest.
            % Author : Eran Ofek (Mar 2022)
            % Example: Ind = selectTarget(T);
            
            arguments
                Obj
                JD                        = [];
                Args.isVisibleArgs cell   = {};
            end
            
            if isempty(JD)
                JD     = celestial.time.julday;
            end
            
            Ind = find(Obj.isVisible(JD,Args.isVisibleArgs{:}));
            [~,SI] = sort(Obj.Priority(Ind), 'descend');
            Ind = Ind(SI);
            
        end
        
        
        
        function Obj = updateCounter(Obj, ObjectIndex)
            % Increase celestial.Targets counter by 1 for selected targets
            % Input  : - A celestial.Targets object.
            %          - A scalar or vector of targets indices, or target
            %            names (as appear in the TargetName property).
            % Output : - A celestial.Targets object, in which the
            %            GlobalCounter and NightCounter for the selected
            %            indices are increased by 1.
            % Author : Eran Ofek (Mar 2023)
            % Example: T=celestial.Targets.generateTargetList('last');
            %          T.updateCounter(2);

            if iscell(ObjectIndex) || ischar(ObjectIndex)
                % Object index is a TargetName
                ObjectIndex = find(strcmp(Obj.TargetName, ObjectIndex));
            end
            
            Obj.Data.GlobalCounter(ObjectIndex) = Obj.Data.GlobalCounter(ObjectIndex) + 1;
            Obj.Data.NightCounter(ObjectIndex) = Obj.Data.NightCounter(ObjectIndex) + 1;
            
        end
        
        function [Result,Struct]=getTarget(Obj, ObjectIndex)
            % Get properties of selected targets by index or target name
            % Input  : - A celestial.Targets object.
            %          - Vector of indices, or cell array of target names.
            % Output : - A table with the selected targets.
            %          - A structure array with the selected targets.
            % Example: T=celestial.Targets.generateTargetList('last');
            %          [ResT, ResS] = T.getTarget(1:2)

            arguments
                Obj
                ObjectIndex
            end

            if ischar(ObjectIndex)
                % assume TargetName is provided
                ObjectIndex = find(strcmp(Obj.TargetName,ObjectIndex));
            end

            Result = Obj.Data(ObjectIndex,:);
            if nargout>1
                Struct = table2struct(Result);
            end
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
        
        function [Alt, Az, dAlt, dAz] = sunAlt(JD, GeoPos)
            % Return Sun geometric Alt and Az [no refraction] (Static)
            % Input  : - Vector of JD
            %          - Geo pos [Lon, Lat] in deg.
            % Output : - Sun Alt [deg].
            %          - Sun Az [deg]
            %          - Sun dAlt/dt [deg/sec]
            %          - Sun dAz/dt [deg/sec]
            % Author : Eran Ofek (Jan 2022)
            % Example: [Alt, Az] = celestial.Targets.sunAlt(2451545, [1 1])
            
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
            % Example: [Time, IsRise] = celestial.Targets.nextSunHorizon
            
            arguments
                JD           = celestial.time.julday;
                GeoPos       = [35 32];
                
                Args.AltThreshold  = -0.83333;
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
        
        function W=fermiExpWeight(Tnow,Tlast, Args)
            % fermi-rise exp-decay weight function for cadence priority
            % Input  : - Time now.
            %          - Time of last observation. Default is 0.
            %          * ...,key,val,...
            %            'TimeCadence' - Cadence. Default is 0.7.
            %            'WeightLevelHigh' - Max Weight following Fermi
            %                   rise. Default is 1.1
            %            'WeightLevelLow' - Min Weight following the exp.
            %                   decay. Default is 1.
            %            'FermiRiseTime' - Fermi rise time scale.
            %                   Default is 0.5.
            %            'ExpDecayTime' - Exp. decay time scale.
            %                   Default is 2.
            % Output : - Vector of weights.
            % Author : Eran Ofek (Dec 2022)
            % Example: t=(0:0.01:10)';
            %          W=celestial.Targets.fermiExpWeight(t,0);
           
            arguments
                Tnow
                Tlast                 = 0;
                Args.FermiCadence     = 0.7;
                Args.WeightLevelHigh  = 1.1;
                Args.WeightLevelLow   = 1.0;
                Args.FermiRiseTime    = 0.5;
                Args.ExpDecayTime     = 2;
            end
            
            T = Tnow - Tlast;
            
            W = zeros(size(T));
            FlagR = T<Args.FermiCadence;
            
            W(FlagR)  = Args.WeightLevelHigh./(1 + exp(-(T(FlagR)-Args.FermiCadence)./Args.FermiRiseTime));
            W(~FlagR) = Args.WeightLevelLow + (Args.WeightLevelHigh-Args.WeightLevelLow).*exp(-(T(~FlagR)-Args.FermiCadence)./Args.ExpDecayTime);        
            
        end
        
        function W=stepWeight(Tnow, Tlast, Args)
            % step weight function for cadence priority
            % Input  : - Time now.
            %          - Time of last observation. Default is 0.
            %          * ...,key,val,...
            %            'StepCadence' - Cadence. Default is 1/48.
            %            'WeightLevelStep' - Weight following step
            %                   rise. Default is 1
            % Output : - Vector of weights.
            % Author : Eran Ofek (Dec 2022)
            % Example: t=(0:0.01:10)';
            %          W=celestial.Targets.stepWeight(t,0);
           
            arguments
                Tnow
                Tlast
                Args.StepCadence     = 1./48;
                Args.WeightLevelStep = 1;
            end
            
            T = Tnow - Tlast;
            
            W = zeros(size(T));
            FlagS = T>Args.StepCadence;
            W(FlagS) = Args.WeightLevelStep;
            
        end
        
    end
    
    methods (Static)  % in other files / unitTest
        Result = unitTest
    end 

end
    