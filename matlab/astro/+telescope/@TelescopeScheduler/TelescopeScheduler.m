% TelescopeScheduler - General-purpose telescope scheduler (matlab/astro/+telescope)
% Description:
%   A general-purpose, telescope-agnostic observation scheduler. The target
%   list is a MATLAB table whose schema follows AstroPack discussion #949
%   (see TelescopeScheduler.defaultTargetTable for the full column list).
%   All celestial geometry (Az/Alt, Sun/Moon, visibility, coordinate
%   transforms) is delegated to the CelCoo class via an embedded CelCoo
%   object (Obj.Coo) built from the RA/Dec columns of the target table.
%
%   The scheduler is advisory and stateless between calls except for the
%   "Dynamic" target columns (LastJD, Counter, NightCounter, ObserveNow):
%   a unit/manager asks the scheduler to propose a target, the scheduler
%   returns one, and the caller updates the counters.
%
%   This file currently implements the per-target weight and constraint
%   calculators that form the building blocks of the selection logic:
%     calcCadencePriority      - Fermi-rise/plateau/exp-decay cadence weight.
%     calcTimeWindowPriority   - StartJD/StopJD window (+ time-critical urgency).
%     calcSunConstraints       - Sun altitude + Sun distance.
%     calcAzAltConstraints     - MinAlt/MaxAlt + system Az/Alt horizon profile.
%     calcAirMassConstraints   - Max airmass limit (+ airmass values).
%     calcMoonConstraints      - Per-target Moon distance (NaN -> illum. profile).
%     calcVisibilityConstraints- MinVisibility for first nightly visit +
%                                VisibilityTimeExcess priority-boost report.
%
% Conventions:
%   - calc*Priority   methods return a numeric weight per target (Ntarget x 1).
%   - calc*Constraints methods return a logical feasibility mask per target
%     (Ntarget x 1; true = target passes the constraint), plus optional
%     secondary outputs (Az/Alt, airmass, Sun/Moon structs, visibility time).
%   - Angles in the target table and configuration are in degrees; time in JD
%     and time intervals in days, unless noted.
%
% Author : Eran Ofek (Jun 2026)
% Example:
%   S = telescope.TelescopeScheduler;
%   S.TargetList = telescope.TelescopeScheduler.defaultTargetTable(3);
%   S.TargetList.RA  = [10; 120; 250];
%   S.TargetList.Dec = [20; -10;  40];
%   JD = 2451545.5;
%   P  = S.calcCadencePriority(JD);
%   F  = S.calcAzAltConstraints(JD);

classdef TelescopeScheduler < Base

    properties
        TargetList table = table()    % Target list (schema: defaultTargetTable)
        JD                            % Current JD (used when IsRealTime=false)
        IsRealTime logical = true     % If true, JD getter returns current UTC JD
        GeoPos             = [35.041201 30.053014 415];  % [Lon(deg) Lat(deg) Height(m)]
    end

    properties  % system-level defaults / constraints (used when per-target is NaN)
        SunAltLimit      = -11.5;     % [deg] Sun must be below this for night
        MinSunDist       = 30;        % [deg] minimum target-Sun distance
        MinMoonAlt       = 0;         % [deg] ignore Moon when its Alt is below this
        MoonIllumThresh  = 0.1;       % ignore Moon when |illum| below this
        % [|illum|, MinDist(deg)] - default illumination-dependent Moon distance
        MoonConstraints  = [0 0; 0.1 1; 0.2 1; 0.3 1; 0.4 2; 0.5 3; 0.6 5; 0.7 10; 0.8 15; 0.9 40; 1.0 40];
        % [Az(deg), MinAlt(deg)] - system horizon profile (interpolated in Az)
        AltConstraints   = [0 15; 90 15; 180 15; 270 15; 360 15];
        MaxAirMass       = Inf;       % system maximum airmass
        DefMinAlt        = 45;        % [deg] default MinAlt when target MinAlt is NaN
        DefMaxAlt        = 90+eps;    % [deg] default MaxAlt when target MaxAlt is NaN
        TimeCriticalBoost= 1;         % max extra priority for time-critical targets near StopJD
        ObserveNowWeight = 1000;      % multiplier for the ObserveNow override (must dominate)
        LSTType          = 'm';       % sidereal time type for Az/Alt ('m'|'a')
    end

    properties  % visibility-integration settings (forwarded to CelCoo.leftVisibility)
        VisIncludeMoon logical = true;  % include Moon in remaining-visibility calc
        VisTimeStep            = 5./1440; % [day] night sampling step
    end

    properties (Hidden)
        Coo CelCoo = CelCoo.empty;    % embedded CelCoo built from TargetList RA/Dec
    end

    methods % constructor
        function Obj = TelescopeScheduler(Args)
            % Construct a TelescopeScheduler.
            % Input  : * ...,key,val,...
            %            'TargetList' - Target table (see defaultTargetTable).
            %                   Default is an empty default-schema table.
            %            'GeoPos' - [Lon(deg) Lat(deg) Height(m)].
            %                   Default is [35.041201 30.053014 415].
            %            'IsRealTime' - Default is true.
            %            'JD' - Initial JD. Default is current JD.
            % Output : - A TelescopeScheduler object.
            % Author : Eran Ofek (Jun 2026)
            % Example: S = telescope.TelescopeScheduler;

            arguments
                Args.TargetList = [];
                Args.GeoPos     = [];
                Args.IsRealTime = [];
                Args.JD         = [];
            end

            if isempty(Args.TargetList)
                Obj.TargetList = telescope.TelescopeScheduler.defaultTargetTable(0);
            else
                Obj.TargetList = Args.TargetList;
            end
            if ~isempty(Args.GeoPos)
                Obj.GeoPos = Args.GeoPos;
            end
            if ~isempty(Args.IsRealTime)
                Obj.IsRealTime = Args.IsRealTime;
            end
            if isempty(Args.JD)
                Obj.JD = celestial.time.julday();
            else
                Obj.JD = Args.JD;
            end
        end
    end

    methods % getters
        function Val = get.JD(Obj)
            % Getter for JD: returns current UTC JD in real-time mode,
            % otherwise the stored JD value.
            if Obj.IsRealTime
                Val = celestial.time.julday();
            else
                Val = Obj.JD;
            end
        end
    end

    methods % coordinate (CelCoo) management
        function populateCoo(Obj)
            % (Re)build the embedded CelCoo object from the target table.
            % Input  : - self.
            % Output : - null (updates Obj.Coo in place).
            % Author : Eran Ofek (Jun 2026)
            % Example: S.populateCoo;

            T = Obj.TargetList;
            C = CelCoo(0, 0, 'InUnits','deg', 'Units','deg');
            if istable(T) && any(strcmp('RA', T.Properties.VariableNames)) && height(T)>0
                C.RA  = T.RA(:);
                C.Dec = T.Dec(:);
            else
                C.RA  = [];
                C.Dec = [];
            end
            C.System = 'eq';
            C.GeoCoo = Obj.GeoPos;
            Obj.Coo  = C;
        end

        function ensureCoo(Obj)
            % Ensure the embedded CelCoo matches the current target table.
            %   Rebuilds Obj.Coo if empty or if its size differs from the
            %   number of targets, and keeps GeoCoo in sync with GeoPos.
            % Author : Eran Ofek (Jun 2026)

            Nt = height(Obj.TargetList);
            if isempty(Obj.Coo) || numel(Obj.Coo.RA) ~= Nt
                Obj.populateCoo();
            else
                Obj.Coo.GeoCoo = Obj.GeoPos;
            end
        end

        function Result = getTotalExpTime(Obj)
            % Total exposure time per target [s] = ExpTime .* Nexp.
            % Output : - Column vector (Ntarget x 1) of total exposure time [s].
            % Author : Eran Ofek (Jun 2026)
            % Example: T = S.getTotalExpTime;

            if height(Obj.TargetList)==0
                Result = [];
            else
                Result = Obj.TargetList.ExpTime(:) .* Obj.TargetList.Nexp(:);
            end
        end
    end

    methods % priorities
        function P = calcCadencePriority(Obj, JD)
            % Cadence priority weight per target.
            %   Uses the Fermi-rise / plateau / exp-decay kernel
            %   (see TelescopeScheduler.cadenceWeight). For targets already
            %   observed during the night (NightCounter>0) the intra-night
            %   parameters (NightCadence / NightMaxPriority / NightAsymPriority)
            %   are used; otherwise the long-term parameters (Cadence /
            %   MaxPriority / AsymPriority). MinPriority is the common floor.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Cadence priority per target (Ntarget x 1).
            % Author : Eran Ofek (Jun 2026)
            % Example: P = S.calcCadencePriority(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end

            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                P = [];
                return;
            end

            dT = JD - T.LastJD(:);    % [day] time since last visit

            % Select per-target cadence parameters (night vs long-term).
            IsNight = T.NightCounter(:) > 0;
            CadVec  = T.Cadence;            % Nt x 4 [Trise, Srise, Tdecay, Sdecay]
            MaxP    = T.MaxPriority(:);
            AsymP   = T.AsymPriority(:);
            CadVec(IsNight,:) = T.NightCadence(IsNight,:);
            MaxP(IsNight)     = T.NightMaxPriority(IsNight);
            AsymP(IsNight)    = T.NightAsymPriority(IsNight);
            MinP    = T.MinPriority(:);

            P = telescope.TelescopeScheduler.cadenceWeight(dT, CadVec, MinP, MaxP, AsymP);

            % Floor the cadence priority at MinPriority.
            P = max(MinP, P);
        end

        function W = calcTimeWindowPriority(Obj, JD)
            % Absolute observing-window priority (StartJD/StopJD).
            %   Targets outside their [StartJD, StopJD] window get weight 0
            %   (excluded). Inside the window the baseline weight is 1.
            %   Time-critical targets (IsTimeCritical=true) with a finite
            %   StopJD receive an extra urgency boost that grows linearly with
            %   the elapsed fraction of their window, up to TimeCriticalBoost.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Time-window priority per target (Ntarget x 1).
            % Notes  : - This enforces the hard observing window AND encodes
            %            deadline urgency for time-critical targets.
            % Author : Eran Ofek (Jun 2026)
            % Example: W = S.calcTimeWindowPriority(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end

            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                W = [];
                return;
            end

            StartJD = T.StartJD(:);
            StopJD  = T.StopJD(:);

            InWindow = JD>=StartJD & JD<=StopJD;
            W = double(InWindow);

            % Deadline urgency for time-critical targets with finite windows.
            IsTC    = T.IsTimeCritical(:) & InWindow & isfinite(StopJD) & (StopJD>StartJD);
            if any(IsTC)
                Frac = (JD - StartJD(IsTC)) ./ (StopJD(IsTC) - StartJD(IsTC));
                Frac = min(1, max(0, Frac));
                W(IsTC) = W(IsTC) + Obj.TimeCriticalBoost .* Frac;
            end
        end

        function W = calcObserveNow(Obj)
            % Observe-now override factor per target.
            %   Returns the ObserveNow column as-is (no additional tests),
            %   to be combined multiplicatively with the other priorities.
            %   ObserveNow>0 marks a target that should be observed
            %   immediately (the value is the number of observations to
            %   conduct now); 0 means no override.
            % Input  : - self.
            % Output : - ObserveNow per target (Ntarget x 1).
            % Author : Eran Ofek (Jun 2026)
            % Example: W = S.calcObserveNow;

            T  = Obj.TargetList;
            if height(T)==0
                W = [];
                return;
            end
            W = T.ObserveNow(:);
        end
    end

    methods % constraints
        function [Flag, Sun] = calcSunConstraints(Obj, JD)
            % Sun-related feasibility: night-time and Sun distance.
            %   The target passes if the Sun altitude is below SunAltLimit
            %   (it is night) AND the target-Sun angular distance exceeds
            %   MinSunDist.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Logical mask per target (Ntarget x 1; true=passes).
            %          - Sun structure from CelCoo.sunDist (RA/Dec/Az/Alt/...).
            % Author : Eran Ofek (Jun 2026)
            % Example: [F,Sun] = S.calcSunConstraints(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            Obj.ensureCoo();
            Nt = height(Obj.TargetList);
            if Nt==0
                Flag = []; Sun = struct();
                return;
            end

            [Sun, SunDistDeg] = Obj.Coo.sunDist(JD, 'OutUnits','deg', 'GeoCoo',Obj.GeoPos);

            IsNight  = Sun.Alt < Obj.SunAltLimit;          % scalar
            FlagDist = SunDistDeg(:) > Obj.MinSunDist;     % per target
            Flag     = IsNight & FlagDist;                 % scalar broadcasts
            Flag     = Flag(:) & true(Nt,1);
        end

        function [Flag, Az, Alt] = calcAzAltConstraints(Obj, JD)
            % Azimuth/Altitude feasibility (MinAlt/MaxAlt + horizon profile).
            %   Computes Az/Alt of every target and accepts those with
            %   altitude above the effective lower limit and below MaxAlt.
            %   The effective lower limit per target is the maximum of the
            %   target MinAlt (NaN -> DefMinAlt) and the system Az/Alt horizon
            %   profile (AltConstraints), interpolated at the target azimuth.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Logical mask per target (Ntarget x 1; true=passes).
            %          - Azimuth [deg] per target.
            %          - Altitude [deg] per target.
            % Author : Eran Ofek (Jun 2026)
            % Example: [F,Az,Alt] = S.calcAzAltConstraints(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            Obj.ensureCoo();
            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                Flag = []; Az = []; Alt = [];
                return;
            end

            [Az, Alt] = Obj.Coo.azAlt(JD, 'GeoCoo',Obj.GeoPos, 'OutUnits','deg', 'LSTType',Obj.LSTType);
            Az  = Az(:);
            Alt = Alt(:);

            MinAlt = T.MinAlt(:);
            MaxAlt = T.MaxAlt(:);
            MinAlt(isnan(MinAlt)) = Obj.DefMinAlt;
            MaxAlt(isnan(MaxAlt)) = Obj.DefMaxAlt;

            % System horizon profile interpolated at target azimuth.
            HorizonMinAlt = interp1(Obj.AltConstraints(:,1), Obj.AltConstraints(:,2), ...
                                    mod(Az,360), 'linear', 'extrap');
            EffMinAlt = max(MinAlt, HorizonMinAlt(:));

            Flag = Alt > EffMinAlt & Alt < MaxAlt;
        end

        function [Flag, AM] = calcAirMassConstraints(Obj, JD)
            % Airmass feasibility and airmass values.
            %   Accepts targets whose airmass is finite, >= 1 (above horizon)
            %   and <= MaxAirMass. The airmass values are returned for use by
            %   the airmass-minimization selection rule.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Logical mask per target (Ntarget x 1; true=passes).
            %          - Airmass per target.
            % Author : Eran Ofek (Jun 2026)
            % Example: [F,AM] = S.calcAirMassConstraints(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            Obj.ensureCoo();
            Nt = height(Obj.TargetList);
            if Nt==0
                Flag = []; AM = [];
                return;
            end

            [~, ~, AM] = Obj.Coo.azAlt(JD, 'GeoCoo',Obj.GeoPos, 'OutUnits','deg', 'LSTType',Obj.LSTType);
            AM   = AM(:);
            Flag = isfinite(AM) & AM>=1 & AM<=Obj.MaxAirMass;
        end

        function [Flag, Moon] = calcMoonConstraints(Obj, JD)
            % Moon-distance feasibility (per-target, illumination-aware).
            %   The Moon is ignored (all targets pass) when its altitude is
            %   below MinMoonAlt or its absolute illuminated fraction is below
            %   MoonIllumThresh. Otherwise the minimum required target-Moon
            %   distance is the per-target MoonDist column; where that is NaN,
            %   the system illumination-dependent profile (MoonConstraints) is
            %   used, interpolated at |Moon illumination|.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Logical mask per target (Ntarget x 1; true=passes).
            %          - Moon structure from CelCoo.moonDist.
            % Author : Eran Ofek (Jun 2026)
            % Example: [F,Moon] = S.calcMoonConstraints(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            Obj.ensureCoo();
            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                Flag = []; Moon = struct();
                return;
            end

            [Moon, MoonDistDeg] = Obj.Coo.moonDist(JD, 'OutUnits','deg', 'GeoCoo',Obj.GeoPos);

            % Moon negligible -> all targets pass.
            if Moon.Alt < Obj.MinMoonAlt || abs(Moon.Illum) < Obj.MoonIllumThresh
                Flag = true(Nt,1);
                return;
            end

            % Minimum required distance per target.
            MinReq = T.MoonDist(:);
            IsDef  = isnan(MinReq);
            if any(IsDef)
                DefDist = interp1(Obj.MoonConstraints(:,1), Obj.MoonConstraints(:,2), ...
                                  abs(Moon.Illum), 'linear', 'extrap');
                MinReq(IsDef) = DefDist;
            end

            Flag = MoonDistDeg(:) > MinReq;
        end

        function [Flag, BoostFlag, LeftVis] = calcVisibilityConstraints(Obj, JD)
            % Visibility feasibility for the first nightly visit + excess boost.
            %   Computes the remaining visibility time of every target
            %   (CelCoo.leftVisibility, using the system Az/Alt horizon
            %   profile and Sun/Moon settings).
            %   Constraint: for targets whose first visit of the night is yet
            %   to be conducted (NightCounter==0), the remaining visibility
            %   must be at least MinVisibility. Targets already started during
            %   the night (NightCounter>0) are not subjected to this minimum.
            %   Boost report: BoostFlag marks targets whose remaining
            %   visibility falls within the [lo,hi] VisibilityTimeExcess band;
            %   the caller adds VisibilityPriorityExcess to their priority
            %   (to prefer targets within a limited visibility window).
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Logical constraint mask per target (true=passes).
            %          - Logical boost report per target (true=in excess band).
            %          - Remaining visibility time per target [day].
            % Notes  : - Remaining visibility uses the system horizon profile
            %            (AltConstraints), not per-target MinAlt; per-target
            %            refinement can be added later.
            % Author : Eran Ofek (Jun 2026)
            % Example: [F,B,L] = S.calcVisibilityConstraints(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            Obj.ensureCoo();
            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                Flag = []; BoostFlag = []; LeftVis = [];
                return;
            end

            LeftVis = Obj.Coo.leftVisibility(JD, Obj.AltConstraints, ...
                                'AltUnits','deg', ...
                                'SunAlt',Obj.SunAltLimit, ...
                                'GeoCoo',Obj.GeoPos, ...
                                'LSTType',Obj.LSTType, ...
                                'IncludeMoon',Obj.VisIncludeMoon, ...
                                'MoonDistProfile',Obj.MoonConstraints, ...
                                'TimeStep',Obj.VisTimeStep);
            LeftVis = LeftVis(:);

            % MinVisibility applies only to the first visit of the night.
            First = T.NightCounter(:)==0;
            Flag  = true(Nt,1);
            Flag(First) = LeftVis(First) >= T.MinVisibility(First);

            % VisibilityTimeExcess priority-boost report.
            VisExcess = T.VisibilityTimeExcess;   % Nt x 2 [lo hi]
            BoostFlag = LeftVis >= VisExcess(:,1) & LeftVis <= VisExcess(:,2);
        end

        function Flag = calcCounterConstraints(Obj)
            % Visit-counter feasibility (global and nightly caps).
            %   A target fails (Flag=false) when it has reached its maximum
            %   number of visits, i.e. Counter>MaxCounter or
            %   NightCounter>MaxNightCounter; otherwise it passes (Flag=true).
            % Input  : - self.
            % Output : - Logical mask per target (Ntarget x 1; true=passes).
            % Author : Eran Ofek (Jun 2026)
            % Example: F = S.calcCounterConstraints;

            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                Flag = [];
                return;
            end

            Flag = ~(T.Counter(:) > T.MaxCounter(:) | T.NightCounter(:) > T.MaxNightCounter(:));
        end
    end

    methods % priority aggregation
        function [Priority, Info] = calcPriority(Obj, JD)
            % Combine all weights and constraints into a per-target priority.
            % Description:
            %   Feasibility (hard constraints, AND-combined into a 0/1 mask):
            %     Sun, Az/Alt, airmass, Moon, visibility, counters, IsActive,
            %     and being inside the [StartJD,StopJD] time window.
            %   Soft priority (only meaningful where feasible):
            %     P = CadencePriority .* TimeWindowPriority
            %         + VisibilityPriorityExcess .* (visibility-excess boost)
            %   Observe-now override (dominant, gated by feasibility):
            %     P = P + ObserveNow .* ObserveNowWeight
            %   Final per-target priority = Feasible .* P.
            % Input  : - self.
            %          - JD. If empty, use Obj.JD. Default is [].
            % Output : - Priority per target (Ntarget x 1). Infeasible targets
            %            get 0.
            %          - (Optional) Info structure with the individual
            %            component weights and constraint flags, for debugging
            %            and inspection.
            % Author : Eran Ofek (Jun 2026)
            % Example: [P, Info] = S.calcPriority(2451545.5);

            arguments
                Obj
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end

            T  = Obj.TargetList;
            Nt = height(T);
            if Nt==0
                Priority = [];
                Info     = struct();
                return;
            end
            Obj.ensureCoo();

            % --- Soft priorities ---
            CadenceP    = Obj.calcCadencePriority(JD);
            TimeWindowP = Obj.calcTimeWindowPriority(JD);
            ObsNow      = Obj.calcObserveNow();

            % --- Hard constraints ---
            FlagSun                 = Obj.calcSunConstraints(JD);
            FlagAzAlt               = Obj.calcAzAltConstraints(JD);
            FlagAM                  = Obj.calcAirMassConstraints(JD);
            FlagMoon                = Obj.calcMoonConstraints(JD);
            [FlagVis, BoostVis]     = Obj.calcVisibilityConstraints(JD);
            FlagCounter             = Obj.calcCounterConstraints();
            FlagActive              = T.IsActive(:);
            FlagWindow              = TimeWindowP > 0;   % inside [StartJD,StopJD]

            Feasible = FlagSun & FlagAzAlt & FlagAM & FlagMoon & ...
                       FlagVis & FlagCounter & FlagActive & FlagWindow;
            FeasD    = double(Feasible);

            % --- Soft priority: multiplicative weights + additive visibility boost ---
            BaseP = CadenceP .* TimeWindowP + T.VisibilityPriorityExcess(:) .* BoostVis;

            Priority = FeasD .* BaseP;

            % --- Observe-now override (dominant, gated by feasibility) ---
            Priority = Priority + FeasD .* ObsNow(:) .* Obj.ObserveNowWeight;

            % --- Optional breakdown ---
            if nargout>1
                Info.JD             = JD;
                Info.CadenceP       = CadenceP;
                Info.TimeWindowP    = TimeWindowP;
                Info.ObserveNow     = ObsNow(:);
                Info.BoostVis       = BoostVis;
                Info.FlagSun        = FlagSun;
                Info.FlagAzAlt      = FlagAzAlt;
                Info.FlagAirMass    = FlagAM;
                Info.FlagMoon       = FlagMoon;
                Info.FlagVisibility = FlagVis;
                Info.FlagCounter    = FlagCounter;
                Info.FlagActive     = FlagActive;
                Info.FlagWindow     = FlagWindow;
                Info.Feasible       = Feasible;
            end
        end
    end

    methods % counters
        function resetNightCounter(Obj)
            % Reset the nightly visit counter of all targets to zero.
            %   Call this when the night changes (e.g. at the start of a new
            %   night) so that the intra-night cadence/limits restart.
            % Input  : - self.
            % Output : - null (updates Obj.TargetList.NightCounter in place).
            % Author : Eran Ofek (Jun 2026)
            % Example: S.resetNightCounter;

            if height(Obj.TargetList)>0
                Obj.TargetList.NightCounter(:) = 0;
            end
        end

        function increaseCounter(Obj, Ind, JD)
            % Register an observation: bump counters and update LastJD.
            %   Increments both Counter (global) and NightCounter by 1 for the
            %   given target(s), and sets their LastJD to JD.
            % Input  : - self.
            %          - Target index/indices into Obj.TargetList.
            %          - JD of the observation. If empty, use Obj.JD.
            %            Default is [].
            % Output : - null (updates Obj.TargetList in place).
            % Author : Eran Ofek (Jun 2026)
            % Example: S.increaseCounter(12, 2451545.6);

            arguments
                Obj
                Ind
                JD = [];
            end
            if isempty(JD)
                JD = Obj.JD;
            end
            if isempty(Ind)
                return;
            end
            Obj.TargetList.Counter(Ind)      = Obj.TargetList.Counter(Ind) + 1;
            Obj.TargetList.NightCounter(Ind) = Obj.TargetList.NightCounter(Ind) + 1;
            Obj.TargetList.LastJD(Ind)       = JD;
        end
    end

    methods % simulations
        function Report = simulate(Obj, Args)
            % Simulate scheduled observations over a time range.
            % Description:
            %   Steps through the night-time portion of [StartJD, EndJD] in
            %   fixed steps. At each step, each of Ntelescope telescopes (all
            %   sharing this scheduler) selects the highest-priority feasible
            %   target via calcPriority, with ties broken toward the lowest
            %   airmass. Selecting a target increments its Counter and
            %   NightCounter and updates LastJD (via increaseCounter), so the
            %   next telescope at the same step sees the updated state and
            %   normally picks a different target. The NightCounter is reset to
            %   zero whenever the night changes (a time gap larger than
            %   NightGap between consecutive night samples).
            % Input  : - self.
            %          * ...,key,val,...
            %            'StartJD' - Simulation start JD.
            %                   Default is 2451545.0.
            %            'EndJD' - Simulation end JD. If empty, StartJD+3.
            %                   Default is [].
            %            'TimeStep' - Time step [day]. Default is 15/1440.
            %            'Ntelescope' - Number of telescopes sharing the
            %                   scheduler. Default is 1.
            %            'NightGap' - Min. time gap [day] between night-time
            %                   samples that marks a new night. Default is 0.3.
            %            'ResetCounters' - If true, zero Counter and
            %                   NightCounter before the simulation.
            %                   Default is true.
            %            'Verbose' - Print a line per observation and display
            %                   the report at the end. Default is true.
            %            'Plot' - Plot observed fields in Aitoff projection
            %                   (requires Mapping Toolbox). Default is true.
            % Output : - A table report with one row per observation:
            %            JD, Telescope, TargetInd, Name, RA, Dec, Alt, AirMass,
            %            Priority, NightCounter, Counter.
            % Notes  : - calcPriority evaluates the (expensive) visibility
            %            integral per call; long, fine-stepped multi-telescope
            %            runs can therefore be slow.
            % Author : Eran Ofek (Jun 2026)
            % Example: S = telescope.TelescopeScheduler;
            %          S.TargetList = telescope.TelescopeScheduler.defaultTargetTable(50);
            %          S.TargetList.RA  = rand(50,1).*360;
            %          S.TargetList.Dec = rand(50,1).*120 - 30;
            %          Rep = S.simulate('EndJD',2451546.0,'Ntelescope',2);

            arguments
                Obj
                Args.StartJD       = 2451545.0;
                Args.EndJD         = [];
                Args.TimeStep      = 15./1440;
                Args.Ntelescope    = 1;
                Args.NightGap      = 0.3;
                Args.ResetCounters logical = true;
                Args.Verbose logical = true;
                Args.Plot    logical = true;
            end

            if isempty(Args.EndJD)
                Args.EndJD = Args.StartJD + 3;
            end

            if height(Obj.TargetList)==0
                error('TelescopeScheduler:simulate:EmptyList', 'TargetList is empty');
            end
            Obj.ensureCoo();

            if Args.ResetCounters
                Obj.TargetList.Counter(:)      = 0;
                Obj.TargetList.NightCounter(:) = 0;
            end

            % Build night-time JD grid (skip daytime).
            VecJD  = (Args.StartJD:Args.TimeStep:Args.EndJD).';
            Sun    = Obj.Coo.sunDist(VecJD, 'OutUnits','deg', 'GeoCoo',Obj.GeoPos);
            VecJD  = VecJD(Sun.Alt(:) < Obj.SunAltLimit);
            Njd    = numel(VecJD);

            % Plot setup.
            DoPlot = Args.Plot;
            if DoPlot
                if exist('axesm','file')~=2 || exist('plotm','file')~=2
                    warning('TelescopeScheduler:simulate:NoMappingToolbox', ...
                            'Mapping Toolbox (axesm/plotm) not found; disabling plot');
                    DoPlot = false;
                else
                    axesm('aitoff', 'Frame','on', 'Grid','on', ...
                          'MeridianLabel','on', 'ParallelLabel','on');
                    hold on;
                    ColorOrder = colororder;
                    Ncolor     = size(ColorOrder,1);
                end
            end

            % Pre-allocate report columns (grow then trim).
            MaxRows = Njd .* Args.Ntelescope;
            RepJD   = nan(MaxRows,1);
            RepTel  = nan(MaxRows,1);
            RepInd  = nan(MaxRows,1);
            RepName = strings(MaxRows,1);
            RepRA   = nan(MaxRows,1);
            RepDec  = nan(MaxRows,1);
            RepAlt  = nan(MaxRows,1);
            RepAM   = nan(MaxRows,1);
            RepP    = nan(MaxRows,1);
            RepNC   = nan(MaxRows,1);
            RepC    = nan(MaxRows,1);
            Nrow    = 0;

            if Args.Verbose
                fprintf('%-20s  Tel  Target  %-12s  %7s %7s %5s %5s %7s\n', ...
                        'Time', 'Name', 'RA', 'Dec', 'Alt', 'AM', 'Prio');
            end

            PrevJD = NaN;
            for Ijd=1:1:Njd
                JD = VecJD(Ijd);

                % New night -> reset nightly counters.
                if isnan(PrevJD) || (JD - PrevJD) > Args.NightGap
                    Obj.resetNightCounter();
                end
                PrevJD = JD;

                % Az/Alt/airmass for this instant (for tie-break and report).
                [~, Alt, AM] = Obj.Coo.azAlt(JD, 'GeoCoo',Obj.GeoPos, ...
                                             'OutUnits','deg', 'LSTType',Obj.LSTType);
                Alt = Alt(:);
                AM  = AM(:);

                for Itel=1:1:Args.Ntelescope
                    P = Obj.calcPriority(JD);
                    if isempty(P) || all(P<=0)
                        continue;   % no feasible target for this telescope
                    end

                    % Highest priority; ties -> lowest airmass.
                    Pmax = max(P);
                    Cand = find(P >= Pmax-1e-9 & P>0);
                    if isscalar(Cand)
                        Ind = Cand;
                    else
                        [~,Kmin] = min(AM(Cand));
                        Ind = Cand(Kmin);
                    end

                    % Record observation.
                    Nrow          = Nrow + 1;
                    RepJD(Nrow)   = JD;
                    RepTel(Nrow)  = Itel;
                    RepInd(Nrow)  = Ind;
                    RepName(Nrow) = string(Obj.TargetList.Name(Ind));
                    RepRA(Nrow)   = Obj.TargetList.RA(Ind);
                    RepDec(Nrow)  = Obj.TargetList.Dec(Ind);
                    RepAlt(Nrow)  = Alt(Ind);
                    RepAM(Nrow)   = AM(Ind);
                    RepP(Nrow)    = P(Ind);

                    % Update counters/LastJD.
                    Obj.increaseCounter(Ind, JD);
                    RepNC(Nrow)   = Obj.TargetList.NightCounter(Ind);
                    RepC(Nrow)    = Obj.TargetList.Counter(Ind);

                    if Args.Verbose
                        TimeStr = convert.time(JD, 'JD', 'StrDate');
                        fprintf('%-20s  %3d  %6d  %-12s  %7.3f %7.3f %5.1f %5.2f %7.3f\n', ...
                                TimeStr{1}, Itel, Ind, RepName(Nrow), ...
                                RepRA(Nrow), RepDec(Nrow), RepAlt(Nrow), RepAM(Nrow), RepP(Nrow));
                    end

                    if DoPlot
                        RAplot = mod(Obj.TargetList.RA(Ind)+180, 360) - 180;
                        Hp = plotm(Obj.TargetList.Dec(Ind), RAplot, '.', 'MarkerSize',12);
                        Hp.Color = ColorOrder(mod(Itel-1, Ncolor)+1, :);
                        drawnow limitrate;
                    end
                end
            end

            % Trim and assemble report.
            Idx = 1:Nrow;
            Report = table(RepJD(Idx), RepTel(Idx), RepInd(Idx), RepName(Idx), ...
                           RepRA(Idx), RepDec(Idx), RepAlt(Idx), RepAM(Idx), ...
                           RepP(Idx), RepNC(Idx), RepC(Idx), ...
                           'VariableNames', {'JD','Telescope','TargetInd','Name', ...
                                             'RA','Dec','Alt','AirMass','Priority', ...
                                             'NightCounter','Counter'});

            if Args.Verbose
                fprintf('\nSimulation complete: %d observations over %d night-time steps.\n', ...
                        Nrow, Njd);
                disp(Report);
            end
        end
    end

    methods (Static)
        function W = cadenceWeight(dT, CadVec, MinP, MaxP, AsymP)
            % Fermi-rise / plateau / exp-decay cadence weight kernel.
            %   W rises (logistic) from MinP toward MaxP around Trise (scale
            %   Srise), holds near MaxP until Tdecay, then decays exponentially
            %   toward AsymP with scale Sdecay.
            % Input  : - dT: time since last observation [day] (vector).
            %          - CadVec: [Trise, Srise, Tdecay, Sdecay], either a 1x4
            %            row (broadcast to all) or an Nx4 matrix (per target).
            %          - MinP: minimum/floor priority (scalar or N-vector).
            %          - MaxP: peak/plateau priority (scalar or N-vector).
            %          - AsymP: asymptotic priority after decay (scalar or N-vector).
            % Output : - Weight per element (N x 1).
            % Author : Eran Ofek (Jun 2026)
            % Example: t = (0:0.05:20).';
            %          W = telescope.TelescopeScheduler.cadenceWeight(t,[0.7 0.05 1 10],0.1,1,0.9);

            dT = dT(:);
            N  = numel(dT);
            if size(CadVec,1)==1
                CadVec = repmat(CadVec, N, 1);
            end
            MinP  = MinP(:)  .* ones(N,1);
            MaxP  = MaxP(:)  .* ones(N,1);
            AsymP = AsymP(:) .* ones(N,1);

            Trise  = CadVec(:,1);
            Srise  = CadVec(:,2);
            Tdecay = CadVec(:,3);
            Sdecay = CadVec(:,4);

            % Logistic rise 0 -> 1.
            R = 1 ./ (1 + exp(-(dT - Trise)./Srise));

            % Decay factor 1 -> 0 (only after Tdecay).
            Dfac = ones(N,1);
            Fd   = dT >= Tdecay;
            Dfac(Fd) = exp(-(dT(Fd) - Tdecay(Fd))./Sdecay(Fd));

            Base = MinP + (MaxP - MinP).*R;
            W    = AsymP + (Base - AsymP).*Dfac;
        end

        function T = defaultTargetTable(N)
            % Build an N-row target table with default values (schema #949).
            % Input  : - Number of target rows. Default is 0.
            % Output : - A MATLAB table with the full target schema and the
            %            default values per AstroPack discussion #949.
            %            RA/Dec are mandatory and default to NaN.
            % Author : Eran Ofek (Jun 2026)
            % Example: T = telescope.TelescopeScheduler.defaultTargetTable(5);

            arguments
                N (1,1) double = 0;
            end
            o = ones(N,1);

            % --- Target ---
            Name         = repmat("", N, 1);
            RA           = nan(N,1);
            Dec          = nan(N,1);
            PM_RA        = 0.*o;
            PM_Dec       = 0.*o;
            Epoch        = 2000.*o;
            OffsetRA     = 0.*o;
            OffsetDec    = 0.*o;
            IsActive     = true(N,1);
            CanInterrupt = false(N,1);
            % --- Exp ---
            IsSingle     = false(N,1);
            ExpTime      = 20.*o;
            Nexp         = 20.*o;
            ExtraArgs    = repmat({{}}, N, 1);
            % --- Const ---
            MinAlt                   = 45.*o;
            MaxAlt                   = (90+eps).*o;
            MoonDist                 = nan(N,1);
            MinVisibility            = (3./24).*o;
            VisibilityTimeExcess     = repmat([2 3]./24, N, 1);
            VisibilityPriorityExcess = 0.5.*o;
            MaxCounter               = Inf.*o;
            MaxNightCounter          = 8.*o;
            StartJD                  = 0.*o;
            StopJD                   = Inf.*o;
            IsTimeCritical           = false(N,1);
            % --- Cadence ---
            MinPriority      = 0.1.*o;
            MaxPriority      = 1.*o;
            AsymPriority     = 0.9.*o;
            Cadence          = repmat([0.7 0.05 1 10], N, 1);
            NightMaxPriority = 1.*o;
            NightAsymPriority= 0.9.*o;
            NightCadence     = repmat([1./24 1./72 1 10], N, 1);
            % --- Mount ---
            Mount            = repmat({NaN}, N, 1);
            IsSimultaneous   = false(N,1);
            ObscurationModel = 0.*o;
            % --- Dynamic ---
            LastJD       = 0.*o;
            Counter      = 0.*o;
            NightCounter = 0.*o;
            ObserveNow   = 0.*o;

            T = table(Name, RA, Dec, PM_RA, PM_Dec, Epoch, OffsetRA, OffsetDec, IsActive, CanInterrupt, ...
                      IsSingle, ExpTime, Nexp, ExtraArgs, ...
                      MinAlt, MaxAlt, MoonDist, MinVisibility, VisibilityTimeExcess, VisibilityPriorityExcess, ...
                      MaxCounter, MaxNightCounter, StartJD, StopJD, IsTimeCritical, ...
                      MinPriority, MaxPriority, AsymPriority, Cadence, NightMaxPriority, NightAsymPriority, NightCadence, ...
                      Mount, IsSimultaneous, ObscurationModel, ...
                      LastJD, Counter, NightCounter, ObserveNow);
        end
    end

end
