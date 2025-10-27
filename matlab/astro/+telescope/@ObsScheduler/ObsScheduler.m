%
% 
% Example:
%{
   S = telescope.ObsScheduler;
%}



classdef ObsScheduler < Base
    
    properties 
        ListName  = '';
        Table table  % with RA, Dec [deg]
        JD
        IsRealTime logical  = true;

        GeoPos = [35.05 30.04 415];  % [deg deg m]

        MountAltLimit    = [];
        SunAltLimit      = -11.5;
        MoonConstraints  = [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5;0.7 10;0.8 15; 0.9 40; 1.0 40];
    end

    properties (Hidden)
        ColRA    = 'RA';
        ColDec   = 'Dec'

        Defaults       = struct('FieldName',"",...
                                'BasePriority',0.1,...
                                'MountNum',NaN,...
                                'Nexp',20, 'ExpTime',20,...
                                'NightCounter', 0, 'MaxNightCounter',3,...
                                'GlobalCounter',0, 'MaxGlobalCounter',Inf,...
                                'LastJD',0,...
                                'StartJD',0, 'StopJD',Inf,...
                                'MinAlt',25, 'MaxAlt',90+eps, 'MinHA',-12./24, 'MaxHA',+12./24,...
                                'MinVisibility',1./24,...
                                'MinMoonDist', -1,...
                                'CadenceMethod',1,...
                                'Cadence',0.7, 'WeightHigh',1.1, 'WeightLow',1.0, 'CadenceRiseTime',0.2, 'WeightDecayTime',10,...
                                'NightCadence',1./24, 'NightWeightHigh',1.5, 'NightWeightLow',1.4, 'NightCadenceRiseTime',0.005, 'NightWeightDecayTime',-100,...
                                'ExtraPriorityHA',0.1, 'MinHA1',-2./24, 'MaxHA1', -1./24);
    end

    properties (Dependent)
        RA
        Dec

        LST  % [deg] scalar
        HA   % [deg]

        EcLon
        EcLat
        GalLon
        GalLat
        Az
        Alt
        AirMass
        ParAng
        Ebv
        TotalExpTime % [s]

        SunAlt
        MoonAlt
        MoonIllum

    end

    methods % constructor
        function Obj = Scheduler()
            Obj.JD = celestial.time.julday();
            Obj.Table = table();
        end
    end

    methods % getters & setters
        function Val=get.JD(Obj)
            % getter for JD - behavior depands on IsRealTime property
            % If IsRealTime=true, then return current JD
            % Else return the content of the property.

            if Obj.IsRealTime
                Val = celestial.time.julday();
            else
                Val = Obj.JD;
            end
        end

        function Val=get.LST(Obj)
            % Getter for LST - Return LST [deg]
            
            RAD = 180./pi;
            Val     = celestial.time.lst(Obj.JD, Obj.GeoPos(1)./RAD, 'm').*350;  % [deg]
        end

        function Val=get.HA(Obj)
            % Getter for HA [deg]
            
            Val = Obj.LST - Obj.RA;
            Val = mod(Val, 360);
            Fpi = Val>180;
            Val(Fpi) = Val(Fpi) - 360;
        end
        

        function Val=get.RA(Obj)
            % getter for RA
            Val = Obj.Table.(Obj.ColRA);
        end

        function Val=get.Dec(Obj)
            % getter for Dec
            Val = Obj.Table.(Obj.ColDec);
        end


        function Val=get.EcLon(Obj)
            % Getter for EcLon (ecliptic longitude) [deg]

            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','e','d','d');
            Val    = OutCoo(:,1);
        end

        function Val=get.EcLat(Obj)
            % Getter for EcLat (ecliptic latitude) [deg]

            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','e','d','d');
            Val    = OutCoo(:,2);
        end


        function Val=get.GalLon(Obj)
            % getter for GalLon Dependent property [deg]
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','g','d','d');
            Val    = OutCoo(:,1);
        end
        
        function Val=get.GalLat(Obj)
            % getter for GalLat Dependent property [deg]
           
            OutCoo = celestial.coo.coco([Obj.RA, Obj.Dec],'j2000.0','g','d','d');
            Val    = OutCoo(:,2);
        end
       
        function Val=get.Az(Obj)
            % getter for Az Dependent property [deg]
           
            [Az, ~, ~, ~] = celestial.coo.radec2azalt(Obj.JD, Obj.RA,Obj.Dec,'GeoCoo',Obj.GeoPos(1:2), 'InUnits','deg','OutUnits','deg','LSTType','m');
            Val = Az;
        end
        
        function Val=get.Alt(Obj)
            % getter for Alt Dependent property [deg]
           
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
        
        function Val=get.Ebv(Obj)
            % Getter for dependent property GalExt (return E(B-V) [mag]
            
            RAD = 180./pi;
            Val = astro.extinction.sky_ebv(Obj.RA./RAD, Obj.Dec./RAD,'eq');
        end
        
        function Val=get.TotalExpTime(Obj)
            % Getter for TotalExpTime
            
            if tools.table.isColumn(Obj.Table, 'ExpTime') && tools.table.isColumn(Obj.Table, 'Nexp')
                Val = Obj.Table.('ExpTime').*Obj.Table.('Nexp');
            else
                Val = nan(Obj.Ntarget,1);
            end
        end

        function Val=get.SunAlt(Obj)
            % getter for SunAlt

            Val = Obj.getSun;
        end

        function Val=get.MoonAlt(Obj)
            % getter for Moon Alt

            Val = Obj.getMoon;
        end

        function Val=get.MoonIllum(Obj)
            % getter for Moon illumination

            [~,~,~,~,Val] = Obj.getMoon;
        end

    end

    methods (Static)  % static utilities


    end


    methods % default lists
        function Ntarget = nTarget(Obj)
            % Return numbre of targets in list
            Ntarget = size(Obj.Table,1);
        end

        function Obj=setTableVal(Obj, ColName, Val, Ind)
            % Set value for table column (using scalar or vector)
            % optionally specified Ind/Flags in which to populate the
            % value (default is all).

            arguments
                Obj
                ColName
                Val
                Ind      = [];
            end

            Ntarget = Obj.nTarget;
            if isempty(Ind)
                % populate all rows
                Obj.Table.(ColName) = Val.*ones(Ntarget,1);
            else
                % populate selected rows
                if numel(Val)==Ntarget && numel(Val)>1
                    Val = Val(Ind);
                end
                NN = numel(Obj.Table.(ColName)(Ind));
                Obj.Table.(ColName) = Val.*ones(NN,1);
            end
        end
            
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

            Nsrc = Obj.nTarget;
            DefFN = fieldnames(Obj.Defaults);
            Ndef  = numel(DefFN);
            for Idef=1:1:Ndef
                NewData = repmat(Obj.Defaults.(DefFN{Idef}), Nsrc, 1);
                Obj.Table.(DefFN{Idef}) = NewData;
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
            % Example: S = telescope.Scheduler;
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
            Obj.Table = array2table(Tbl, 'VariableNames',{Obj.ColRA, Obj.ColDec});

            Obj = Obj.injectDefaultColumns;
            Obj.ListName = Args.ListName;
            
            FieldName = string(num2cell((1:1:Nsrc).'));
            Obj.Table.FieldName = FieldName;

        end
    end


    methods % limits & constraints
        function Obj=populateMountAltLimit(Obj, Data, Path)
            % populate MountAltConstraints
            % Input  : - self.
            %          - Data - options are:
            %            A structure array with Con field and elemnt per
            %            mount. Each element contains a two column matrix
            %            of [Az, Alt].
            %            A mat file name containing the structure.
            %            A cell array of string array of file names (one
            %            per mount).
            %          - Path of files. If empty use current dir.
            %            Default is [].
            % Output : - self. Populated with MountAltLimit.
            % Author : Eran Ofek (Nov 2024)
            % Example: CMC = tools.cell.sprintf2cell('MountConst%d.txt',(1:1:12)');
            %          S.populateMountAltLimit(CMC);
           
            arguments
                Obj
                Data
                Path = [];
            end
            
            if ~isempty(Path)
                PWD = pwd;
                cd(Path);
            end
                
            if isstruct(Data)
                Obj.MountAltLimit = Data;
            elseif ischar(Data)
                % assume mat file with struct inside
                Data = io.files.load2(Data);
            else
                % assume string array or cell array
                % each element contains file to load (per mount)
                Nf = numel(Data);
                Obj.MountAltLimit = struct('Con',cell(Nf,1));
                for If=1:1:Nf
                    Tmp    = load(Data{If});
                    [~,SI] = sort(Tmp(:,1));
                    Tmp    = Tmp(SI,:);
                    Tmp    = [[-1; Tmp(:,1); 361], [Tmp(1,2); Tmp(:,2); Tmp(end,2)]];
                    Nt     = size(Tmp,1);
                    Tmp(:,1) = Tmp(:,1) + (1:1:Nt).'.*100.*eps;
                    Obj.MountAltLimit(If).Con = Tmp;
                end
            end
            
            if ~isempty(Path)
                cd(PWD);
            end
            
        end

        function AltLimit=altLimit(Obj)
            % Get Alt limit for all targets
            %   The AltLimit is calculated per target given its Az.
            %   The sources of AltLimit are: 'table AltLimit' column.
            %   'Az alt limit' from the MountAltLimit property (either for
            %   all targets, or per mount).
            %   If the MountAltLimit property is a [Az, Alt] matrix
            %   then the AltLimit is max of the table AltLimit and Az alt limit.
            %   otherwise it is taken from the table.
            % Input  : - self.
            % Output : - AltLimit vector for all targets.

            AltLimitCol = 'MinAlt';

            TableAltLimit = Obj.Table.(AltLimitCol);
            if isempty(Obj.MountAltLimit)
                % use only target AltLimit
                AltLimit = TableAltLimit;
            else
                % get targets Az
                TargetAz  = Obj.Az;
                Ntarget   = size(TargetAz,1);

                if isstruct(Obj.MountAltLimit)
                    % MountAltLimit is a structure with limits per mount
                    Nmnt  = numel(Obj.MountAltLimit);
                    Ntime = numel(Obj.JD); 
                    AltLimit = nan(Ntarget,Ntime);
                    for Imnt=1:1:Nmnt
                        TheAltLimit = Obj.MountAltLimit(Imnt).Con;

                        IndMount = find(Obj.Table.MountNum==Imnt);
                        if ~isempty(IndMount)
                            AltLimit(IndMount,:) = interp1(TheAltLimit(:,1), TheAltLimit(:,2), TargetAz(IndMount,:));
                        end
                    end
                    AltLimit = max(AltLimit, TableAltLimit);
                else
                    % single MontAltLimit
                    TheAltLimit = Obj.MountAltLimit;

                    AltLimit = interp1(TheAltLimit(:,1), TheAltLimit(:,2), TargetAz);
                    AltLimit = max(AltLimit, TableAltLimit);
                end
            end
        end

        function Dist = sphere_dist(Obj, RA, Dec)
            % Spherical distance between all targets and some list of coordinates [deg].
            % Output is a matrix (column per coordinate), row per target.

            RAD = 180./pi;

            % convert to raw vector [rad]:
            RA  = RA(:).'./RAD;
            Dec = Dec(:).'./RAD;

            Dist = celestial.coo.sphere_dist_fast(Obj.Table.(Obj.ColRA)./RAD, Obj.Table.(Obj.ColDec)./RAD, RA, Dec).*RAD;

        end
    
    end

    methods % Moon, Sun
        function [SunAlt, SunAz, SunRA, SunDec, DAltDt, DAzDt, Dist] = getSun(Obj, UseJD)
            % Get Sun [Alt, Az, RA, Dec, DAltDt (deg/day), DAzDt, Dist(target)]

            arguments
                Obj
                UseJD = [];
            end
            RAD = 180./pi;

            if isempty(UseJD)
                UseJD = Obj.JD;
            end
                
            [SunRA,SunDec]=celestial.SolarSys.suncoo(UseJD, 'j');  % [rad]
            LST = celestial.time.lst(UseJD, Obj.GeoPos(1)./RAD, 'm');  % [frac day]
            
            SunHA = 2.*pi.*LST - SunRA;  % [rad]
            [SunAz, SunAlt] = celestial.coo.hadec2azalt(SunHA, SunDec, Obj.GeoPos(2)./RAD, 'rad');
            SunRA  = SunRA.*RAD;
            SunDec = SunDec.*RAD;
            SunAz  = SunAz.*RAD;
            SunAlt = SunAlt.*RAD;

            if nargout>4
                SEC_DAY = 86400;
                [SunAlt1, SunAz1] = Obj.getSun(UseJD + 10./SEC_DAY);
                DAltDt = (SunAlt1 - SunAlt).*SEC_DAY./10;  % [deg/day]
                DAzDt  = (SunAz1 - SunAz).*SEC_DAY./10;    % [deg/day]
            end

            if nargin>6
                % angular distance between targets and Sun
                Dist = celestial.coo.sphere_dist_fast(SunRA./RAD, SunDec./RAD, Obj.Table.(Obj.ColRA)./RAD, Obj.Table.(Obj.ColDec)./RAD).*RAD; % [deg]
            end

        end

        function [MoonAlt, MoonAz, MoonRA, MoonDec, Illum, DAltDt, DAzDt, Dist] = getMoon(Obj, UseJD)
            % Get Moon [Alt, Az, RA, Dec, IllumFrac, Dist(target)] [deg]

            arguments
                Obj
                UseJD  = [];
            end
            RAD = 180./pi;

            if isempty(UseJD)
                UseJD = Obj.JD;
            end

            [MoonRA,MoonDec] = celestial.SolarSys.mooncool(UseJD(:), Obj.GeoPos(1:2)./RAD, 'b');
            LST = celestial.time.lst(UseJD, Obj.GeoPos(1)./RAD, 'm');  % [frac day]
            [Illum,Ph]       = celestial.SolarSys.moon_illum(UseJD(:));
            MoonRA  = MoonRA(:).';
            MoonDec = MoonDec(:).';
            Illum   = Illum(:).';

            MoonHA = 2.*pi.*LST - MoonRA;
            [MoonAz, MoonAlt] = celestial.coo.hadec2azalt(MoonHA./RAD, MoonDec./RAD, Obj.GeoPos(2)./RAD, 'rad');
            MoonRA  = MoonRA.*RAD;
            MoonDec = MoonDec.*RAD;
            MoonAz  = MoonAz.*RAD;
            MoonAlt = MoonAlt.*RAD;

            if nargout>5
                SEC_DAY = 86400;
                [MoonAlt1, MoonAz1] = Obj.getMoon(UseJD + 10./SEC_DAY);
                DAltDt = (MoonAlt1 - MoonAlt).*SEC_DAY./10;  % [deg/day]
                DAzDt  = (MoonAz1 - MoonAz).*SEC_DAY./10;    % [deg/day]
            end

            if nargout>7
                % angular distance between targets and Moon
                Dist = celestial.coo.sphere_dist_fast(MoonRA./RAD, MoonDec./RAD, Obj.Table.(Obj.ColRA)./RAD, Obj.Table.(Obj.ColDec)./RAD).*RAD; % [deg]
            end

        end

        function [Rise, Set] = sunRiseSet(Obj, AltLimit, ReturnAll, ScanDay)
            % Calculate Sun Rise/Set time (in JD) above some AltLimit
            % If ReturnAll=true, then may retutn more then one Rise/Set
           

            arguments
                Obj
                AltLimit   = [];
                ReturnAll  = false;
                ScanDay    = 0.55;
            end


            if isempty(AltLimit)
                AltLimit = Obj.SunAltLimit;
            end

            UseJD = Obj.JD;

            VecJD = UseJD + (-ScanDay:1./1440:ScanDay).'; 

            [SunAlt] = Obj.getSun(VecJD);

            ZC = tools.find.find_local_zeros(VecJD, SunAlt-AltLimit); 
            % [X, 1st derivative, 2nd derivative d^2Y/dX^2]
            Irise = find(ZC(:,2)>0);
            Iset  = find(ZC(:,2)<0);
            Rise  = ZC(Irise,1);
            Set   = ZC(Iset, 1);
            if ~ReturnAll
                if numel(Rise)>1
                    if (UseJD - Set)>0
                        Rise = Rise(2);
                    else
                        Rise = Rise(1);
                    end
                end
                if numel(Set)>1
                    if (UseJD - Rise)>0
                        Set = Set(2);
                    else
                        Set = Set(1);
                    end
                end
            end
            
        end

        function [TimeToSunSet, NextSet] = timeToSunSet(Obj, AltLimit)
            % Calculate time [days] to next sun set, given some AlLimit.

            arguments
                Obj
                AltLimit   = [];
            end

            UseJD = Obj.JD;
            [Rise, Set]  = Obj.sunRiseSet(AltLimit, true, 1.0);
            NextSet      = Set(find(Set>UseJD, 1));
            TimeToSunSet = NextSet - UseJD;

        end

        function [TimeToSunRise, NextRise] = timeToSunRise(Obj, AltLimit)
            % Calculate time [days] to next sun rise, given some AlLimit.

            arguments
                Obj
                AltLimit   = [];
            end

            UseJD = Obj.JD;
            [Rise, Set]   = Obj.sunRiseSet(AltLimit, true, 1.0);
            NextRise      = Rise(find(Rise>UseJD, 1));
            TimeToSunRise = NextRise - UseJD;

        end

    end

    methods % visibility
        function Result=isDark(Obj)
            % Return true if Sun below SunAltLimit

            SunAlt = Obj.getSun;
            Result = SunAlt<Obj.SunAltLimit;
        end

        function [Result, IsEvening]=isTwighlight(Obj)
            % Return true if Sun below 0 and above SunAltLimit
            %   Also return IsEvening (if evenining twilght)

            [SunAlt,~,~,~,SunDaltDt] = Obj.getSun;
            Result = SunAlt>=Obj.SunAltLimit & SunAlt<0;
            IsEvening = SunDaltDt<0;
        end
    
        function Result=isMoonOk(Obj)
            % Return for each target if it is far enough from the Moon, given the Moon illumination

            Ntarget = Obj.nTarget;
            [MoonAlt,~,~,~,MoonIllumFrac, ~, ~, MoonDist] = Obj.getMoon;
            MoonAlt       = MoonAlt(:).';
            MoonIllumFrac = MoonIllumFrac(:).';
            %MoonDist      = MoonDist(:).';  % MoonDist may be a matrix!

            if all(MoonAlt<0)
                Result = true(Ntarget,1);
            else
                % Min dist for Moon observations
                MinDist = interp1(Obj.MoonConstraints(:,1), Obj.MoonConstraints(:,2), MoonIllumFrac);                
                Result = MoonDist>MinDist;
            end

        end
    
        function VisTime=visibilityTime(Obj, Args)
            % Return for each target its visibility time (till AltConstraints or SunRise or HA limits)
            %   By default the visibility constraints include altLimit, and
            %   may also include isDark and moonDist.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Apply_isDark' - The visibility constraints will
            %                   include isDark. Default is true.
            %            'Apply_moonDist' - The visibility constraints will
            %                   include moon distance. Default is true.
            %            'TimeStep'  - Time step to check visibility [day]
            %                   Default is 5./1440.
            %            'TimeRange' - Time range to check visibility [day]
            %                   Default is [0 0.6].
            % Output : - A vector with element per target of the visibility
            %            time left for each target [day].
            % Author : Eran Ofek (Oct 2025)
            % Example: S.visibilityTime;

            arguments
                Obj
                Args.Apply_isDark   = true;
                Args.Apply_moonDist = true;
                Args.TimeStep   = 5./1440;
                Args.TimeRange  = [0 0.6];
            end

            Ntarget = Obj.nTarget;
            VecTime = (Args.TimeRange(1):Args.TimeStep:Args.TimeRange(2));


            UseJD = Obj.JD;

            StateIsRealTime = Obj.IsRealTime;
            Obj.IsRealTime  = false;
            Obj.JD          = UseJD; % + VecTime;
            AltLimitTarget  = Obj.altLimit();
            Obj.JD          = UseJD + VecTime;
            AltTarget       = Obj.Alt;

            FlagVis         = AltTarget>AltLimitTarget;
            
            % multiply by the Sun state
            if Args.Apply_isDark
                FlagVis         = FlagVis .* Obj.isDark;
            end
            if Args.Apply_moonDist
                FlagMoon = Obj.isMoonOk;
                FlagVis  = FlagVis & FlagMoon;
            end
            % is visible now
            IsVisNow        = FlagVis(:,1);

            % count number of time steps of visibility
            % multiply by the visibility now to account only for visible
            % targets
            VisCount = nan(Ntarget,1);
            for Itarget=1:1:Ntarget
                Diff = diff(FlagVis(Itarget,:));
                Ivis = find(Diff==-1, 1);
                if isempty(Ivis)
                    VisCount(Itarget) = 0;
                else
                    VisCount(Itarget) = Ivis;
                end
            end
            VisTime        = (VisCount+1) .* double(IsVisNow) .* Args.TimeStep;
                    

            % return IsRealTime to original state
            Obj.IsRealTime = StateIsRealTime;
            if ~Obj.IsRealTime
                Obj.JD = UseJD;
            end
            
        end
    
        function Result=isVisible(Obj)
            % Return a vector of logicals per target indicatinf if target is visible
            % Input  : - self
            % Output : - A vector logicals per target indicatinf if target is visible
            % Author : Eran Ofek (Oct 2025)
            % Example: S.isVisible;

            UseJD = Obj.JD;

            % check list:
            %NightCounter
            %GlobalCounter
            %StartJD
            %StopJD
            %MinAlt (part of MinVisibility)
            %MaxAlt
            %MinHA
            %MaxHA
            %MinVisibility
            %MinMonnDist (part of MinVisibility)
            
            TimeVis  = Obj.visibilityTime;
            Alt      = Obj.Alt;
            AltLimit = Obj.altLimit;
            HA       = Obj.HA ./360;

            Result = ((TimeVis>Obj.Table.MinVisibility & Obj.Table.NightCounter==0) | (Obj.Table.NightCounter>0 & Obj.isMoonOk & Alt>AltLimit)) & ...
                     Obj.Table.GlobalCounter<Obj.Table.MaxGlobalCounter & ...
                     Obj.Table.NightCounter<Obj.Table.MaxNightCounter & ...
                     UseJD>Obj.Table.StartJD & UseJD<Obj.Table.StopJD & ...
                     HA>Obj.Table.MinHA & HA<Obj.Table.MaxHA;


        end
    end

    methods % read and write
        function save(Obj, FileName, SaveType)
            % Save ObsScheduler object
            % Input  : - self.
            %          - FileName to save.
            %          - SaveType - one of the following options:
            %            'object' - Save object as mat file.
            %            'table' - Save table as mat file.
            %            'csv' - Sae table as csv file.
            %            Default is 'object'.
            % Output : null
            % Author : Eran Ofek (Oct 2025)
            % Example: S.save('targets.csv','csv')

            arguments
                Obj
                FileName
                SaveType = 'object';  % 'object' | 'table' | 'csv'
            end

            switch lower(SaveType)
                case 'object'
                    % save object as mat file
                    save(FileName, 'Obj', '-v7.3');
                case 'table'
                    % save table as mat file
                    Tbl = Obj.Table;
                    save(FileName, 'Tbl', '-v7.3');
                case 'csv'
                    writetable(Obj.Table, FileName);
                otherwise
                    error('Unknown SaveType option');
            end


        end
    
        function Result=read(Obj, FileName, Args)
            % Read mat file/csv file into telescope.ObsSceduler object.
            %   Read a mat file containing a table or object, into a
            %   telescope.ObsScheduler object.
            %   Alternatively, read a csv file into the object table.
            % Input  : - self.
            %          - FileName to read. Default is 'TargetList.mat'.
            %          * ...,key,val,...
            %            'CreateNewObj' - Create a new deep copy of the
            %                   object. Default is false.
            % Output : - Updated object.
            % Author : Eran Ofek (Oct 2025)
            % Example: S.read;

            arguments
                Obj
                FileName = 'TargetList.mat';
                Args.CreateNewObj   = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            try
                Tbl = readtable(FileName);
                Result.Table = Tbl;
            catch
                Data = io.files.load2(FileName);
                if isa(Data, 'table')
                    Result.Table = Data;
                else
                    Result = Data;
                end
            end

        end
    end

    methods % plots
        
        function plotMountAltLimit(Obj, Mounts)
            % Plot Az/Alt mount constraints
            % Input  : - self.
            %          - Mounts to plot. Default is (1:1:12)
            % Output : plot the Az vs. Alt mount constraints per mount.
            % Author : Eran Ofek (Feb 2025)
            % Example: CMC = tools.cell.sprintf2cell('MountConst%d.txt',(1:1:12)');
            %          S.populateMountAltLimit(CMC);
            %          S.plotMountAltLimit([1 3])
        
            arguments
                Obj(1,1)
                Mounts = (1:1:12);
            end
                   
            %Nmnt = numel(Obj.MountAltConstraints);
            Nmnt = numel(Mounts);
            [Colors,Markers] = plot.colorMarkerOrder;
            for Im=1:1:Nmnt
                Imnt = Mounts(Im);
                H=plot(Obj.MountAltLimit(Imnt).Con(:,1), Obj.MountAltLimit(Imnt).Con(:,2));
                H.Color = Colors(Imnt,:);
                H.LineStyle = Markers{Imnt};
                hold on;
            end
            axis([0 360 0 90]);
            
            CellMnt = num2cell(Mounts);
            Str  = {'M%d'};
            Cell = tools.cell.cell_sprintf(repmat(Str,Nmnt,1), CellMnt(:));
            legend(Cell{:}, 'Location','NorthEast')
        end
    end

end