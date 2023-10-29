% celestial.OrbitalEl - A class for storing and manipulating orbital elements, and two body motion calculations.
% Author : Eran Ofek (Jan 2022)
%
% #functions (autogen)
% OrbitalEl - Constractor for OrbitalEl class
% eccAnom2radius - Eccentric anomaly to radius vector
% eccAnom2trueAnom - Eccentric Anomaly to True Anomaly
% ephem - Calculate epjemerides for OrbitalEl object. For each orbital-element or time, return the Geocentric or topocentric ephemerides of the target. For definitions and formulae, see Explanatory Supplement to the Astronomical
% get.A - getter for A (semi-major axis)
% get.PeriDist - getter for A (semi-major axis)
% get.Tp - getter for periapsis time [JD]
% keplerSolve - Solve the Kepler equation for OrbitalEl object. For elliptic, parabolic, and hyperbolic orbits Simoultanously solves multiple orbtial elemsnts for a single time or for vector of times of the same length, or a single orbital element at multiple times.
% loadSolarSystem - Load the JPL Solar System orbital elements from local disk To install the orbotal elements use the Installer class.
% magnitude - Calculate magnitude for an OrbitalEl object
% meanMotion - Return the mean motion [deg/day]
% merge - Merge the orbital elements in several elements of the OrbitalEl object. This function is custom made for merging the JPL epehmerides, and may fail in other cases.
% nuDot - Calculate the time derivative of the true anomaly Description: Calculate the time derivative of the true anomaly. Correct only for e<1
% numEl - Return the number or orbital elements in each OrbitalEl element.
% period - Return the orbital period
% r2vel - Calculate orbital velocity from radius vector Description: Calculate orbital velocity from radius vector Correct only for e<1
% rDot - Calculate the time derivative of the radius vector Description: Calculate the time derivative of the radius vector. correct only for e<1
% searchMinorPlanetsNearPosition - Search all minor planets/comets near position on a specific date. Given an OrbitalEl object with multiple elements, in which each elements contains vectors of multiple orbital elements, generate epehmerides and search for all minor planets and comets that are near position (cone search).
% selectFlag - Select specific orbital-elements (targets) from an OrbitalEl object.
% semiLatusRectum - Return the semilatus rectum
% table - Generate a matlab table or orbital elements from OrbitalEl object.
% thiele_innes - Convert orbital elements to Thiele-Innes elements Description: Convert orbital elements to Thiele-Innes orbital elements.
% trueAnom2eccAnom - True Anomaly to Eccentric Anomaly
% trueAnom2radius - True anomaly to radius vector
% trueAnom2rectPos - True anomaly and radius vector to rectangular position Description: True anomaly to rectangular position
% #/functions (autogen)
%

classdef OrbitalEl < Base
    % OrbitalEl class for storing and manipulating orbital elements

    % Properties
    properties
        Number
        Designation
        Node
        W
        Incl
        A
        PeriDist
        Eccen
        Tp
        Epoch
        Mepoch   % Mean Anomaly at Epoch
        MagPar
        MagType = 'HG';
       
        Equinox   = 'J2000.0';
        AngUnits  = 'deg';
        LenUnits  = 'au';
        TimeUnits = 'day';  % must be days!
        K = 0.017202098950000; % Gaussian Gravitational Constant
        Ref
    end
    
    methods % constructor
        function Obj = OrbitalEl(Args)
            % Constractor for OrbitalEl class
            % Input  * ...,key,val,...
            %          Any of the OrbitalEl properties folloed by value.
            %          Defaults 'MagType='HG', K=0.017202098950000.
            % Output : - An OrbitalEl object
            % Author : Eran Ofek (Sep 2021)
            % Example: Obj = celestial.OrbitalEl
           
            arguments
                Args.Number       = [];
                Args.Designation  = [];
                Args.Node         = [];
                Args.W            = [];
                Args.Incl         = [];
                Args.A            = [];
                Args.PeriDist     = [];
                Args.Eccen        = [];
                Args.Tp           = [];
                Args.Epoch        = [];
                Args.Mepoch       = [];
                Args.MagPar       = [];
                Args.MagType      = 'HG';  % or function handle
                Args.Equinox      = 'J2000.0';
                Args.AngUnits     = 'deg';
                Args.LenUnits     = 'au';
                Args.TimeUnits    = 'day';
                Args.K            = 0.017202098950000; % Gaussian Gravitational Constant
                Args.Ref          = '';
            end
            
            FN = fieldnames(Args);
            for I=1:1:numel(FN)
                Obj.(FN{I}) = Args.(FN{I});
            end
            
        end
    end
    
    methods % setter/getters
        function Result = get.A(Obj)
            % getter for A (semi-major axis)
           
            if isempty(Obj.A)
                % check if PeriDist and Eccen are available
                if ~isempty(Obj.PeriDist) && ~isempty(Obj.Eccen)
                    % calc A
                    Obj.A = Obj.PeriDist./(1 - Obj.Eccen);
                end
            end
            Result = Obj.A;
        end
        
        function Result = get.PeriDist(Obj)
            % getter for A (semi-major axis)
           
            if isempty(Obj.PeriDist)
                % check if A and Eccen are available
                if ~isempty(Obj.A) && ~isempty(Obj.Eccen)
                    % calc PeriDist
                    Obj.PeriDist = Obj.A.*(1 - Obj.Eccen);
                end
            end
            Result = Obj.PeriDist;
        end
        
        function Result = get.Tp(Obj)
            % getter for periapsis time [JD]
            
            if isempty(Obj.Tp) && (~isempty(Obj.Mepoch) && ~isempty(Obj.A))
                Obj.Tp = Obj.Epoch - Obj.Mepoch./Obj.meanMotion(Obj.AngUnits);
            end
            
            Obj.Tp = real(Obj.Tp);
            
            Result = Obj.Tp;
            
        end
    end
    
    methods % basic functions
        function Result = numEl(Obj)
            % Return the number or orbital elements in each OrbitalEl
            % element.
           
            Nobj   = numel(Obj);
            Result = zeros(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = numel(Obj(Iobj).Node);
            end
        end
        
        function Result = selectFlag(Obj, Flag, CreateNewObj)
            % Select specific orbital-elements (targets) from an OrbitalEl object.
            % Input  : - A single element OrbitalEl object that may contain
            %            multiple orbital elements.
            %          - A vector of logical flags, or indices to select
            %            from the OrbitalEl input object.
            %          - Indicate if to create a new deep copy of the object.
            %            [], true, false.
            %            If true, create new deep copy
            %            If false, return pointer to object
            %            If [] and Nargout==0 then do not create new copy.
            %            Otherwise, create new copy.
            % Output : - An OrbitalEl object with the selected orbits.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Res   = selectFlag(OrbEl, 1, true);
            
            arguments
                Obj(1,1)
                Flag
                CreateNewObj      = [];
            end
            
            [Result] = createNewObj(Obj, CreateNewObj, nargout);
            
            Ne    = numEl(Result);
            Prop  = fieldnames(Result);
            Nprop = numel(Prop);
            for Iprop=1:1:Nprop
                Ndata = size(Result.(Prop{Iprop}), 1);
                if numel(Flag)==Ndata
                    if iscell(Result.(Prop{Iprop}))
                        Result.(Prop{Iprop}) = Result.(Prop{Iprop})(Flag);
                    else
                        Result.(Prop{Iprop}) = Result.(Prop{Iprop})(Flag,:);
                    end
                end
            end
            
        end
        
        function Result = merge(Obj)
            % Merge the orbital elements in several elements of the OrbitalEl object.
            %   This function is custom made for merging the JPL
            %   epehmerides, and may fail in other cases.
            % Input  : - An OrbitalEl object, with multiple elements.
            % Output : - A merged OrbitalEl objt with a single element.
            %            This is always a new copy.
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem;
            %          O = merge(OrbEl);
            
            ConCatProp  = {'Number','Designation','Node','W','Incl','Eccen','PeriDist','A','Epoch','Tp','Mepoch','Ref','MagPar'};
            SingleProp  = {'Equinox','AngUnits','LenUnits','TimeUnits','K','UserData'};
            NccProp     = numel(ConCatProp);
            NsProp      = numel(SingleProp);
            Nobj = numel(Obj);
            Result = celestial.OrbitalEl;
            for Iobj=1:1:Nobj
                for Icc=1:1:NccProp
                    if isempty(Obj(Iobj).(ConCatProp{Icc}))
                        Nel = numEl(Obj(Iobj));
                        Ncol = size(Obj(1).(ConCatProp{Icc}),2);
                        Obj(Iobj).(ConCatProp{Icc}) = nan(Nel, Ncol);
                    end
                    Result.(ConCatProp{Icc}) = [Result.(ConCatProp{Icc}); Obj(Iobj).(ConCatProp{Icc})];
                end
                
                for Is=1:1:NsProp
                    if Iobj==1
                        Result.(SingleProp{Is}) = Obj(Iobj).(SingleProp{Is});
                    end
                    if ~isempty(Obj(Iobj).(SingleProp{Is}))
                        if isnumeric(Obj(Iobj).(SingleProp{Is}))
                            if Result.(SingleProp{Is})~=Obj(Iobj).(SingleProp{Is})
                                error('Prop %s betweein element %d and %d are not equal',SingleProp{Is},1,Is);
                            end
                        else
                            if ~strcmp(Result.(SingleProp{Is}), Obj(Iobj).(SingleProp{Is}))
                                error('Prop %s betweein element %d and %d are not equal',SingleProp{Is},1,Is);
                            end
                        end
                    end
                end
            end
            
        end
                
    end

    methods % Keplerian orbit functions
        function Result = meanMotion(Obj, AngUnits)
            % Return the mean motion [deg/day]
            % Input  : - A single element OrbitalEl object
            %          - AngularUnits. Default is 'rad'.
            % Output : - Mean motion [rad/day]
            
            arguments
                Obj(1,1)
                AngUnits = 'rad';
            end
            
            switch lower(AngUnits)
                case 'rad'
                    Result = 2.*pi./period(Obj, 'day');
                case 'deg'
                    Result = 360./period(Obj, 'day');
                otherwise
                    error('AngUnits not supported');
            end
                        
        end
        
        function Result = period(Obj, Units)
            % Return the orbital period
            % Input  : - A single element OrbitalEl object
            %          - Units. Default is 'day'
            % Output : - Orbital period
            % Example: Result = period(OrbEl)
            
            arguments
                Obj(1,1)
                Units     = 'day';
            end
            
            Result = 2.*pi .* abs(Obj.A).^1.5 ./ Obj.K;  % days
            switch lower(Units)
                case 'day'
                    % do nothing
                case 'yr'
                    Result = Result./365.25;
                case 's'
                    Result = Result.*86400;
                otherwise
                    error('Unknown Units option');
            end
        end
        
        function Result = semiLatusRectum(Obj)
            % Return the semilatus rectum
            
            Result = Obj.PeriDist.*(1 + Obj.Eccen);

        end
        
        function Result = eccAnom2radius(Obj, E, Units)
            % Eccentric anomaly to radius vector
            % Input  : - A single element OrbitalEl object.
            %          - Eccentric Anomaly.
            %          - Units of Eccentric Anomaly. Default is 'rad'.
            % Output : - Radius vector.
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = eccAnom2radius(OrbEl, 1);
            
            arguments
                Obj(1,1)
                E
                Units     = 'rad';
            end
            E      = convert.angular(Units, 'rad', E);
            Result = (1 - Obj.Eccen .* cos(E)) .* Obj.PeriDist ./ (1 - Obj.Eccen);
        end
        
        function Result = trueAnom2radius(Obj, Nu, Units)
            % True anomaly to radius vector
            % Input  : - A single element OrbitalEl object.
            %          - True Anomaly.
            %          - Units of True Anomaly. Default is 'rad'.
            % Output : - Radius vector.
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = trueAnom2radius(OrbEl, 1);
            
            arguments
                Obj(1,1)
                Nu
                Units     = 'rad';
            end
            
            Nu      = convert.angular(Units, 'rad', Nu);
            Nu    = Nu.*ones(size(Obj.PeriDist));
            FlagP = Obj.Eccen==1;
            FlagH = Obj.Eccen>1;
            Result        = Obj.PeriDist.*(1 + Obj.Eccen)./(1 + Obj.Eccen.*cos(Nu));
            Result(FlagP) = Obj.PeriDist(FlagP).*(1 + tan(0.5.*Nu(FlagP)).^2);
            Result(FlagH) = Obj.PeriDist(FlagH).*(1 + Obj.Eccen(FlagH))./(1 + Obj.Eccen(FlagH).*cos(Nu(FlagH)));
        end

        function Result = eccAnom2trueAnom(Obj, E, Units)
            % Eccentric Anomaly to True Anomaly
            % Input  : - A single element OrbitalEl object.
            %          - Eccentric anomaly.
            %          - Input and output units. Default is 'rad'.
            % Output : - True anomaly.
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = eccAnom2trueAnom(OrbEl(1), 1)
           
            arguments
                Obj(1,1)
                E
                Units     = 'rad';
            end
            
            E = convert.angular(Units, 'rad', E);
            
            % is this correct for e>1  ???
            Result = 2.*atan(sqrt( (1 + Obj.Eccen)./(1 - Obj.Eccen) ).*tan(0.5.*E));
            
            Result = convert.angular('rad', Units, Result);
        end
        
        function Result = trueAnom2eccAnom(Obj, Nu, Units)
            % True Anomaly to Eccentric Anomaly
            % Input  : - A single element OrbitalEl object.
            %          - True anomaly.
            %          - Input and output units. Default is 'rad'.
            % Output : - Eccentric anomaly.
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = trueAnom2eccAnom(OrbEl(1), 1)
           
            arguments
                Obj(1,1)
                Nu
                Units     = 'rad';
            end
            
            Nu = convert.angular(Units, 'rad', Nu);
            
            % is this correct for e>1  ???
            Result = 2.*atan(sqrt( (1 - Obj.Eccen)./(1 + Obj.Eccen) ).*tan(0.5.*Nu));
            
            Result = convert.angular('rad', Units, Result);
        end
        
        function dNUdt = nuDot(Obj, Nu, Units)
            % Calculate the time derivative of the true anomaly
            % Description: Calculate the time derivative of the true anomaly.
            %              Correct only for e<1
            % Input  : - A single element OrbitalEl object.
            %          - True anomaly.
            %          - Input and output units. Default is 'rad'.
            % Output : - dNu/dt
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = nuDot(OrbEl(1), 1)
            
            arguments
                Obj(1,1)
                Nu
                Units     = 'rad';
            end
            
            if any(Obj.Eccen>=1)
                error('nudot correct only for e<1');
            end
            
            
            E = trueAnom2eccAnom(Obj, Nu, Units); % Eccentric anomaly
            E = convert.angular(Units, 'rad', E);
            dNUdt = meanMotion(Obj, Units) .* sqrt(1 - Obj.Eccen.^2)./((1 - Obj.Eccen.*cos(E)).^2);
            dNUdt = convert.angular('rad', Units, dNUdt);
        end
        
        function drdt = rDot(Obj, Nu, Units)
            % Calculate the time derivative of the radius vector
            % Description: Calculate the time derivative of the radius
            %              vector.
            %              correct only for e<1
            % Input  : - A single element OrbitalEl object.
            %          - True anomaly [rad].
            % Output : - dr/dt
            % Author : Eran Ofek (Sep 2021)
            % Example: Result = rDot(OrbEl(1), 1)
            
            arguments
                Obj(1,1)
                Nu
                Units     = 'rad';
            end
            
            E = trueAnom2eccAnom(Obj, Nu, Units); % Eccentric anomaly
            drdt = meanMotion(Obj, Units) .* Obj.A .* Obj.Eccen .* sin(E)./(1 - Obj.Eccen.*cos(E));
            
        end

        function V=r2vel(Obj, R, TimeUnits)
            % Calculate orbital velocity from radius vector
            % Description: Calculate orbital velocity from radius vector
            %              Correct only for e<1
            % Input  : - A single elements OrbitalEl object.
            %          - Radius vector
            %          - TimeUnits. Default is 'day'
            % Output : - Velocity (default units au/day)
            % Author : Eran Ofek (Sep 2021)
            % Example: V=r2vel(OrbEl(1), 1); % [au/day]
            %          V=r2vel(OrbEl(1), 1, 's').*constant.au./1e5; %[km/s]
            arguments
                Obj(1,1)
                R
                TimeUnits    = 'day';
            end
            
            
            %V = (sqrt(2).*2.*pi.*Obj.A ./ period(Obj,TimeUnits)).*sqrt( 1./R - 1./(2.*Obj.A) );
            
            %V = sqrt( pi.*Obj.A ./ period(Obj,TimeUnits)).*( 1./R - 1./(2.*Obj.A) );
            
            
            V = sqrt((constant.G.*constant.SunM)./convert.length(Obj.LenUnits,'cm') .* (2./R - 1./Obj.A));  % cm/s
            V = V.*convert.length('cm',Obj.LenUnits)./convert.timeUnits('s',Obj.TimeUnits);
            
        end

    end
    
    methods  % probability distributions
        function Prob = probabilityDist(Obj, Args)
            % Return probability distribution of Nu,R,E,V,M
            %   For each source in the OrbitalEl object calculate Nsim
            %   realizations and return all the evaluated parameters.
            % Input  : - A celestial.OrbitalEl object.
            %          * ...,key,val,...
            %            'Nsim' - Number of realizations per source.
            %                   Default is 100.
            %            'TimeRange' - Time range from t0 in which to
            %                   randomally calculate the realizations.
            %                   Default is 1e4 [days].
            %            'T0' - JD of t0. Default is 2451545.
            %            'keplerSolveArgs' - A cell array of arguments to
            %                   pass to keplerSolve. Default is {}.
            % Output : - A structure with all the simulated parameters.
            % Author : Eran Ofek (Aug 2023)
            % Example: O=celestial.OrbitalEl.loadSolarSystem;
            %          F=O(1).A>35; O(1)
            %          K=O(1).selectFlag(F);
            %          P=K.probabilityDist;
           
            arguments
                Obj
                Args.Nsim                      = 100;
                Args.TimeRange                 = 1e4;  % [days]
                Args.T0                        = 2451545;
                Args.keplerSolveArgs cell      = {};
            end
            
            Time = Args.T0 + Args.TimeRange.*rand(Args.Nsim,1); 
            All = struct('V',cell(Args.Nsim,1), 'Nu',cell(Args.Nsim,1), 'R',cell(Args.Nsim,1), 'M',cell(Args.Nsim,1));
            for Isim=1:1:Args.Nsim
                [All(Isim).Nu, All(Isim).R, All(Isim).E, All(Isim).V, All(Isim).M] = keplerSolve(Obj, Time(Isim), Args.keplerSolveArgs{:});
            end
            
            Prob.Nu = [All.Nu];
            Prob.Nu = Prob.Nu(:);
            Prob.R  = [All.R];
            Prob.R  = Prob.R(:);
            Prob.E  = [All.E];
            Prob.E  = Prob.E(:);
            Prob.V  = [All.V];
            Prob.V  = Prob.V(:);
            Prob.M  = [All.M];
            Prob.M  = Prob.M(:);
            
        end
    end
    
    methods % Solving Kepler equation and related functions
        function [Nu, R, E, Vel, M] = keplerSolve(Obj, Time, Args)
            % Solve the Kepler equation for OrbitalEl object.
            %   For elliptic, parabolic, and hyperbolic orbits
            %   Simoultanously solves multiple orbtial elemsnts for a
            %   single time or for vector of times of the same length,
            %   or a single orbital element at multiple times.
            % Input  : - A single element OrbitalEl object.
            %          - Vector or scalar of times (e.g. JD).
            %            The time of periastron will be subtracted from
            %            this time.
            %          * ...,key,val,...
            %            'Tol' - Tolerance. Default is 1e-8 (radians).
            %            'K' - Gaussian gravitational constant.
            %                  If empty, then use OrbitalEl object default.
            %                  Default is [].
            %            'SubTp' - A logical indicating if to subtract the
            %                   time of periapsis. Default is true.
            % Output : - Vctor of True anomaly [rad].
            %          - Vecor of radius vector [au].
            %          - Vector of Eccentric anomaly [rad].
            %          - Vector of velocity [au/day].
            %          - Vector of Mean anomaly [rad]. NaN for parabolic or
            %            hyperbolic.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbElA = celestial.OrbitalEl.loadSolarSystem;
            %          [Nu, R, E, Vel, M] = keplerSolve(OrbElA(1), 2451545)
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
            %          [Nu, R, E, Vel, M] = keplerSolve(OrbEl(1), 2451545+(1:1:10)')
            %     Test parabolic orbit
            %          E1.Tp = celestial.time.julday([14 4 1998 0.4358]);
            %          E1.PeriDist = 1.487469;
            %          JD = celestial.time.julday([ 5 8 1998]);
            %          E1.W=1; E1.Incl=1; E1.Node=1; E1.Eccen=1;
            %          [Nu, R, E, Vel, M] = keplerSolve(E1, JD);
            %          % Nu should be 66.78862 deg, R=2.133911
            
            arguments
                Obj(1,1)
                Time
                Args.Tol                = 1e-8;
                Args.K                  = [];  % use Obj default
                Args.SubTp(1,1) logical = true;
            end
            
            if ~isempty(Args.K)
                Obj.K = Args.K;
            end
            
            Nel  = numEl(Obj);
           
            Time = Time(:);
            Ntime = numel(Time);
            Nout  = max(Nel, Ntime);
            
            if Args.SubTp
                Time = Time - Obj.Tp;
            end
            Nu   = zeros(Nout, 1);
            R    = zeros(Nout, 1);
            E    = zeros(Nout, 1);
            Vel  = zeros(Nout, 1);
            M    = nan(Nout, 1);
            
            if Nel==1
                % single orbital element & multiple times
                if Obj.Eccen<1
                    [Nu, R, E, Vel, M] = celestial.Kepler.kepler_elliptic(Time, Obj.PeriDist, Obj.Eccen, Obj.K, Args.Tol);
                end
                if Obj.Eccen==1
                    [Nu, R, E, Vel] = celestial.Kepler.kepler_parabolic(Time, Obj.PeriDist, Obj.K);
                end
                if Obj.Eccen>1
                    [Nu, R, E, Vel] = celestial.Kepler.kepler_hyperbolic(Time, Obj.PeriDist, Obj.Eccen, Obj.K, Args.Tol);
                end
            else
                % check that Nel==Ntime
                if ~(Nel==Ntime || Ntime==1)
                    error('Number of times must be equal the number or orbital elements or 1');
                end
                
                % multipl epochs corresponding to multiple orbital elements
                Flag = Obj.Eccen<1;
                if any(Flag)
                    [Nu(Flag),R(Flag),E(Flag),Vel(Flag),M(Flag)] = celestial.Kepler.kepler_elliptic(Time(Flag), Obj.PeriDist(Flag), Obj.Eccen(Flag), Obj.K, Args.Tol);
                end

                Flag = Obj.Eccen==1;
                % E is S
                if any(Flag)
                    [Nu(Flag),R(Flag),E(Flag),Vel(Flag)] = celestial.Kepler.kepler_parabolic(Time(Flag), Obj.PeriDist(Flag), Obj.K);
                end

                Flag = Obj.Eccen>1;
                % E is H
                if any(Flag)
                    [Nu(Flag),R(Flag),E(Flag),Vel(Flag)] = celestial.Kepler.kepler_hyperbolic(Time(Flag), Obj.PeriDist(Flag), Obj.Eccen(Flag), Obj.K, Args.Tol);
                end
            end
            
        end
            
        function varargout=trueAnom2rectPos(Obj, Nu, R, AngUnits)
            % True anomaly and radius vector to rectangular Ecliptic position
            % Description: True anomaly to rectangular position
            % Input  : - OrbitalEl object.
            %          - True anomaly [rad].
            %          - Optional radius vector. If not given will be
            %            calculated from the True anaomaly.
            %          - Angilar units. Default is 'rad'.
            % Output : * Either a single 3 column matrix of [X,Y,Z] or
            %            X,Y,Z. Units the same as the radius vector units.
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          BodyPos = trueAnom2rectPos(OrbEl(1), 1, 1)
            %          [x,y,z] = trueAnom2rectPos(OrbEl(1), 1, 1)
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
            %          BodyPos = trueAnom2rectPos(OrbEl, [1;2], [1;2])
            
            arguments
                Obj(1,1)
                Nu
                R
                AngUnits   = 'rad';
            end
            RAD = 180./pi;
            
            if (nargin<3)
                % calc radius vector
                R = trueAnom2radius(OrbEl, Nu, AngUnits);
            end
            
            Nu = convert.angular(AngUnits, 'rad', Nu);
            Factor = convert.angular(Obj.AngUnits, 'rad', 1);
            [varargout{1:nargout}] = celestial.Kepler.trueanom2pos(R, Nu, Obj.Node.*Factor, Obj.W.*Factor, Obj.Incl.*Factor);
        end
        
        function [V,X] = trueAnom2rectVel(Obj, Nu, R, E, AngUnits)
            % Return rectangular velocity and position vectors in Ecliptic system
            %   calculated from the orbital elements and the true anomaly.
            % Input  : - An OrbitalEl object.
            %          - Vector of true anomaly.
            %          - Vector of radius vector. If empty, then calc.
            %            Default is [].
            %          - Vector of Eccentric anomaly. If empty, then calc.
            %            Default is [].
            %          - Units of true and eccentric anomaly.
            %            Default is 'rad'.
            % Output : - A 3 lines matrix of the rectangular velocity in
            %            the reference frame of the orbital elements (e.g.,
            %            equatorial J2000). [au/day]
            %          - The same, but for the position [au].
            % Author : Eran Ofek (Oct 2021)
            % Ref: https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf
            % Example: [V,X] = trueAnom2rectVel(OrbEl, 1);
           
            arguments
                Obj
                Nu
                R    = [];
                E    = [];
                AngUnits = 'rad';
            end
            
            RAD = 180./pi;
            
            [XP, VP] = nu2posOrbitalFrame(Obj, Nu, E, R, AngUnits);
            
            CosW = cos(Obj.W(:).'./RAD);
            SinW = sin(Obj.W(:).'./RAD);
            CosO = cos(Obj.Node(:).'./RAD);
            SinO = sin(Obj.Node(:).'./RAD);
            CosI = cos(Obj.Incl(:).'./RAD);
            SinI = sin(Obj.Incl(:).'./RAD);
            SinWcosI = SinW.*CosI;
            CosWcosI = CosW.*CosI;
            
            
            Part1 = [(CosW.*CosO - SinWcosI.*SinO);  (CosW.*SinO + SinWcosI.*CosO); SinW.*SinI];
            Part2 = [(-SinW.*CosO - CosWcosI.*SinO); (CosWcosI.*CosO - SinW.*SinO); CosW.*SinI];
                 
            V     = VP(1,:) .* Part1 + VP(2,:) .* Part2;
            
            if nargout>1
                X  = XP(1,:) .* Part1 + XP(2,:) .* Part2;
            end
            
        end

        function [X, V] = nu2posOrbitalFrame(Obj, Nu, E, R, Units)
            % Convert true anomaly to position in orbital frame
            % Input  : - OrbitalEl object.
            %          - Vector of true anomaly.
            %          - Vector of Eccentric anomaly. If empty, then calc.
            %            Default is [].
            %          - Vector of radius vector. If empty, then calc.
            %            Default is [].
            %          - Units of true and eccentric anomaly.
            %            Default is 'rad'.
            % Output : - A 3 lines matrix of X positions of the body on its
            %            orbital plan. Column per object/true anomaly.
            %            [au].
            %          - A 3 lines matrix of velocity [au/day].
            % Author : Eran Ofek (Oct 2021)
            % Example: [X, V] = nu2posOrbitalFrame(OrbEl,1);
            
            arguments
                Obj
                Nu
                E       = [];
                R       = [];
                Units   = 'rad';
            end
           
            %SqrtMu = k.*constant.au.^1.5 ./86400;  % cgs
            Mu = Obj.K.^2;  % au/day
            
            if isempty(E)
                E = trueAnom2eccAnom(Obj, Nu, Units);
            end
            if isempty(R)
                R = eccAnom2radius(Obj, E, Units);
            end
            Nu = convert.angular(Units, 'rad', Nu);
            E  = convert.angular(Units, 'rad', E);
            
            Nu = Nu(:).';
            E  = E(:).';
            R  = R(:).';
            N  = numel(Obj.Eccen);
            
            X = [R.*cos(Nu); R.*sin(Nu); zeros(1,N)];
            V = sqrt(Mu .* Obj.A(:).')./R .* [-sin(E); sqrt(1-Obj.Eccen(:).'.^2).*cos(E); zeros(1,N)];
            
            
        end
            
    end
    
    methods % ephemerides
        function Mag = magnitude(Obj, R, Delta, Phase, Args)
            % Calculate magnitude for an OrbitalEl object
            % Input  : - A single element OrbitalEl object.
            %          - R [au] Sun-target distance
            %          - Delta [au] Observer-target distance
            %          - Phase angle (Sun-Target-Observer angle.
            %          * ...,key,val,...
            %            'MagType' - MagType (e.g., 'HG'). If empty, use
            %                   object MagType. Default is [].
            %            'MagPar' - Mag parameters (e.g., [H, G]). If empty
            %                   use object MagPar. Default is [].
            %            'PhaseUnits' - Units of phase angle.
            %                   Default is 'deg'.
            % Output : - Magnitudes
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbElA = celestial.OrbitalEl.loadSolarSystem('num');
            %          Mag = magnitude(OrbElA, 1, 1, 0)
            
            arguments
                Obj(1,1)
                R
                Delta
                Phase            = [];
                Args.MagType     = [];  % if empty use OrbitalEl.MagType
                Args.MagPar      = [];  % if empty, use OrbitalEl.MagPar
                Args.PhaseUnits  = 'deg';
            end
            
            if isempty(Args.MagType)
                Args.MagType = Obj.MagType;
            end
            if isempty(Args.MagPar)
                Args.MagPar = Obj.MagPar;
            end
            
            Phase = convert.angular(Args.PhaseUnits, 'rad', Phase); % [rad]
            
            if ischar(Args.MagType)
                switch lower(Args.MagType)
                    case 'hg'
                        % H-G magnitude
                        switch size(Args.MagPar,2)
                            case 2
                                Mag = celestial.SolarSys.asteroid_magnitude(R, Delta, Phase, Args.MagPar(:,1), Args.MagPar(:,2));
                            case 1
                                Mag = celestial.SolarSys.asteroid_magnitude(R, Delta, Phase, Args.MagPar(:,1), nan(size(Args.MagPar(:,1))));
                            case 0
                                Mag = nan(size(Obj.Incl));
                            otherwise
                                error('Unknown MagPar size option');
                        end
                                
                    otherwise
                        error('Unknown planetray magnitude algorithm');
                end
            else
                % assume function handle is provided
                error('Unknown MagType option');
            end
            
        end
            
        
        
        function Result = ephem(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object.
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %
            %   For definitions and formulae, see Explanatory Supplement to the Astronomical
            %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
            % Input  : - A single element OrbitalEl object.
            %            This object may include multiple orbital elements
            %            in vectors of parameters.
            %          - A vector of JD in the TDT time scale.
            %            If the input OrbitalEl object contains multiple
            %            orbital elements, then the length of the vector of
            %            times may be 1 or equal to the number of orbital
            %            elements. In this case, different times corresponds
            %            to different orbital elements.
            %            Alternatively, if the input OrbitalEl object
            %            contains a single orbital element, then it will be
            %            calculated at the different times.
            %          * ...,key,val,...
            %            'Tol' - Tolerance [rad] for solving the Kepler
            %                   equation. Default is 1e-8.
            %            'TolLT' - Tolerance [day] for the light-time
            %                   correction iterations. Default is 1e-6.
            %            'OutUnitsDeg' - A logical indicating if to list
            %                   the RA and Dec in degrees. If false list in
            %                   radians. Default is true.
            %            'Aberration' - A logical indicating if to include
            %                   aberration of light. Default is false.
            %                   Note that for the default (false) the
            %                   output is in an "astrometric" reference
            %                   frame (i.e., relative to the stars).
            %            'EarthEphem' - Earth ephemeris to use:
            %                   'vsop87' - VSOP87
            %                   'inpop' - INPOP (default).
            %            'GeoPos' - Geodetic position of the observer (on
            %                   Earth). [Lon (rad), Lat (rad), Height (m)].
            %                   If empty, then calculate geocentric
            %                   positions. Default is [].
            %            'RefEllipsoid' - Reference ellipsoid for the
            %                   geodetic positions. Default is 'WGS84'.
            %            'OutType' - Output type:
            %                   'mat' - a matrix
            %                   'AstroCatalog' - An AstroCatalog object.
            %                   Default is 'AstroCatalog'
            %            'MaxIterLT' - Maximum numbre of iterations for
            %                   light-time corrections. Default is 5.
            %                   0 will force to no ligh-time correction
            %                   (e.g., for quick calculation).
            %            'IncludeMag' - A logical indicating if to include
            %                   magnitude in output catalog.
            %                   Default is true.
            %            'AddDesignation' - A logical indicating if to add
            %                   the asteroid designation (in the last
            %                   column) to the output.
            %                   If true, then the output will be in a
            %                   format of table instead of a matrix.
            %                   Default is true.
            %            'Integration' - A logical indicating if to use
            %                   integration to calculate the positions.
            %                   Default is false.
            %            'TolInt' - Tolerance of the integration ODE's.
            %                   Default is 1e-10.
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
            %                   If empty, the function will use EarthEphem and GeoPos.
            %                   In case of size [Nepoch,3], the function assume zero velocity.
            %                   Defauls is [].
            % Output : - Output ephemerides with the following columns:
            %            {'JD', 'RA', 'Dec', 'R', 'Delta','SOT','STO', 'Mag'}
            %            and units:
            %            {'day','deg','deg', 'au','au','deg','deg','mag'}.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
            %          JD = celestial.time.julday([9 9 2021])
            %          Cat = ephem(OrbEl, JD +(1:1:100)')
            %
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Cat = ephem(OrbEl, JD);
            %          tic;CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false);toc
            %
            %     compare to JPL
            %          JD = celestial.time.julday([19 9 2021])+(0:1./24:1)';
            %          Coo=[-116.865./RAD 33.3563./RAD 2000]
            %          OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],9804);
            %          CatE = ephem(OrbEl1, JD, 'GeoPos',Coo, 'OutUnitsDeg',false)
            %          [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','675')
            %          % RA nd Dec diff between JPL and ephem:
            %          [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600
            %     hyperbolic orbit
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('unnum','A/2017 U1');
            %          JD = celestial.time.julday([1 1 2018 0]);
            %          Cat = ephem(OrbEl, JD+(0:1./24:1), 'OutUnitsDeg',false);
            %          [CatJPL]=celestial.SolarSys.jpl_horizons('ObjectInd','A/2017 U1','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','399')
            %          [Cat.Catalog(:,2) - CatJPL.Catalog(:,2), Cat.Catalog(:,3) - CatJPL.Catalog(:,3)].*RAD.*3600

            arguments
                Obj(1,1)
                Time
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.OutUnitsDeg(1,1) logical    = true;
                Args.Aberration(1,1) logical     = false;
                Args.EarthEphem                  = 'vsop87';  % 'vsop87' | 'inpop'
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';  % 'mat' | 'AstroCatalog'
                Args.MaxIterLT                   = 5;  % use 0 for quick and dirty
                Args.IncludeMag(1,1) logical     = true;  % use false to speed up
                Args.AddDesignation(1,1) logical = true;  % works only for AstroCatalog output
                Args.Integration(1,1) logical    = false; %false; 
                Args.TolInt                      = 1e-10; 
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
            end
            RAD  = 180./pi;
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            Nt      = numel(Time);
            Ntarget = numEl(Obj);
            if ~(Nt==1 || Ntarget==1)
                error('Number of epochs or number of targets must be 1');
            end
            Ncat = max(Nt, Ntarget);
            
            ColNames      = {'JD', 'RA', 'Dec', 'R', 'Delta','SOT','STO', 'Mag'};
            if Args.OutUnitsDeg
                ColUnits      = {'day','deg','deg', 'au','au','deg','deg','mag'};
            else
                ColUnits      = {'day','rad','rad', 'au','au','deg','deg','mag'};
            end
            Cat           = nan(Ncat, numel(ColNames));
            
            if Args.Integration
                [Nu0]  = keplerSolve(Obj, Obj.Epoch, 'Tol',Args.Tol);
                [V0,X0] = trueAnom2rectVel(Obj,Nu0,[],[]);
                [StartEpochs,~,IndEpochs] = unique(Obj.Epoch); % devide to groups with same initial epoch
            end

            for It=1:1:Nt
                LightTimeNotConverged = true;
                LightTime             = 0;
                Iter                  = 0;              
                while LightTimeNotConverged
                    Iter = Iter + 1;
                    if Args.Integration
                        % Orbital integration
                        U_B = zeros(3,Ntarget);
                        % loop for each group with same initial epoch
                        for Iepoch = 1:numel(StartEpochs)
                            IndTargets = find(IndEpochs ==Iepoch);
                            NtargetsEpoch = numel(IndTargets);
                             % if light times are different
                            if NtargetsEpoch>1 && numel(LightTime)>1 && any(LightTime(IndTargets)~=LightTime(IndTargets(1))) 
                                X_B = X0(:,IndTargets);
                                V_B = V0(:,IndTargets);

                                % first integrate all targets to minimal
                                % time (maximal light time)
                                [MaxLightTime,ImaxLightTime] = max(LightTime(IndTargets));
                                [X_B,V_B] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch),Time(It)-MaxLightTime]...
                                        ,X_B,V_B, 'RelTol',Args.TolInt,'AbsTol',Args.TolInt);

                                % then integrate one by one according to
                                % light time
                                for Itarget = 1:numel(IndTargets)
                                     [X_B(:,Itarget),~] = celestial.SolarSys.orbitIntegration([Time(It)-MaxLightTime,Time(It)-LightTime(IndTargets(Itarget))]...
                                        ,X_B(:,Itarget),V_B(:,Itarget), 'RelTol',Args.TolInt,'AbsTol',Args.TolInt);
                                end    
                                U_B(:,IndTargets) = X_B;
                            else % if light times are equal integrate all at once
                                if numel(LightTime)>1
                                    IndLightTime = IndTargets(1);
                                else
                                    IndLightTime =1;
                                end
                                [U_B(:,IndTargets),~] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch),Time(It)-LightTime(IndLightTime)],...
                                                                    X0(:,IndTargets),V0(:,IndTargets),'RelTol',Args.TolInt,'AbsTol',Args.TolInt);
                            end
                        end
                    else
                        % Solution via Kepler equation
                        [Nu, R, E, Vel, M]          = keplerSolve(Obj, Time(It)-LightTime,'Tol',Args.Tol);
                        % target ecliptic Heliocentric rect. position
                        [U_B] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                        U_B   = U_B.';  % a 3 X N matrix
                    end


                    % verified
                    %RAD = 180./pi;
                    %atan2(Ytarget, Xtarget).*RAD
                    %atan(Ztarget./sqrt(Xtarget.^2 + Ytarget.^2)).*RAD

                    % rectangular ecliptic coordinates of Earth with equinox of J2000
                    if ~isempty(Args.ObserverEphem)
                        
                        E_H = Args.ObserverEphem(It,1:3)';
                        E_dotH = Args.ObserverEphem(It,4:6)';
                    else
                    
                    switch lower(Args.EarthEphem)
                        case 'vsop87'
                            [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time(It), 'Earth', 'a', 'd');
                        case 'inpop'
%                             error('INPOP is not implemented yet - use vsop87');
                            IN = celestial.INPOP;  % need to make it singelton
                            IN.populateTables({'Ear','Sun'});
                            IN.populateTables({'Ear','Sun'},'FileData','vel');
                            %
                            E_H = IN.getPos('Ear',Time(It),'IsEclipticOut',true) - IN.getPos('Sun',Time(It),'IsEclipticOut',true);
                            E_dotH = IN.getVel('Ear',Time(It),'IsEclipticOut',true) - IN.getVel('Sun',Time(It),'IsEclipticOut',true);
                            
                            % convert to eclipic coordinates
                            
                        otherwise
                            error('Unknown EarthEphem option');
                            
                    end
                    end
                    Gau = celestial.coo.topocentricVector(Time(It), Args.GeoPos, 'OutUnits','au',...
                                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                                             'Convert2ecliptic',true,...
                                                                             'Equinox','J2000');

                    E_H = E_H + Gau;
                    

                    U = U_B - E_H;  % U_B(t-tau)
                    % Q = U_B - S_B; % U_B(t-tau) - S_B(t-tau)

                    Delta = sqrt(sum(U.^2, 1));

                    PrevLightTime = LightTime;
                    LightTime = Delta./Caud;
                    % more accuratly - use:
                    % celestial.Kepler.LightTimeCorrection

                    if all(abs(LightTime - PrevLightTime))<Args.TolLT || Iter>Args.MaxIterLT
                        LightTimeNotConverged = false;
                    end
                end
                R     = sqrt(sum(U_B.^2, 1));

                % ignore light deflection
                if Args.Aberration
                    U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta);
                else
                    U2 = U;
                end

                % Rotate from Ecliptic to Equatorial reference frame
                RotMat = celestial.coo.rotm_coo('E');
                Equatorial_U2 = RotMat * U2;
                
                RA  = atan2(Equatorial_U2(2,:), Equatorial_U2(1,:));
                Dec = atan(Equatorial_U2(3,:)./sqrt( Equatorial_U2(1,:).^2 + Equatorial_U2(2,:).^2  ));
                
                RA = mod(RA, 2.*pi);
                
                if Args.OutUnitsDeg
                    RA  = RA.*RAD;
                    Dec = Dec.*RAD;
                end
                
                % calculate angles
                Rsun = sqrt(sum(E_H.^2, 1));  % Sun-Earth distance
                % Target-Observer-Sun
                Ang_SOT = acosd((Rsun.^2 + Delta.^2 - R.^2)./(2.*Rsun.*Delta));  % [deg]
                % Observer-Target-Sun
                Ang_STO = acosd((R.^2 + Delta.^2 - Rsun.^2)./(2.*R.*Delta));   % [deg]
                
                if Args.IncludeMag
                    Mag = magnitude(Obj, R(:), Delta(:), Ang_STO(:), 'PhaseUnits','deg');
                else
                    Mag = nan(size(RA));
                end
                
                if Nt==1
                    % single time, multiple elements
                    Cat = [Time(:).*ones(Ntarget,1), RA(:), Dec(:), R(:), Delta(:), Ang_SOT(:), Ang_STO(:), Mag(:)];
                else
                    % assume single orbital element and multiple times
                    Cat(It, :) = [Time(It), RA, Dec, R, Delta, Ang_SOT, Ang_STO, Mag];
                end
            end
            
             if Args.AddDesignation
                Cat = array2table(Cat);
                if Nt>1
                    % assume a single asteroid ephemerides -
                    % duplicate name
                    [NameCell{1:1:Nt}] = deal(Obj.Designation{1});
                else
                    NameCell = Obj.Designation;
                end
                Cat = [Cat, NameCell(:)];
                ColNames = {ColNames{:}, 'Designation'};
                ColUnits = {ColUnits{:}, ''};
            end

            switch lower(Args.OutType)
                case 'mat'
                    Result = Cat;
                case 'astrocatalog'
                    Result = AstroCatalog({Cat}, 'ColNames',ColNames', 'ColUnits',ColUnits);
                otherwise
                    error('Unknown OutType option');
            end
                
            %celestial.coo.convertdms(RA,'r','SH')
            %celestial.coo.convertdms(Dec,'R','SD')
            % geocentric  05 39 59.38 +11 02 53.3
            % topocentric 05 39 59.53 +11 02 51.9
            
        end

        function Result = ephemIntegrate(Obj, Time, Args)
        end
        
        function [Result, Names] = searchMinorPlanetsNearPosition(Obj, JD, RA, Dec, SearchRadius, Args)
            % Search all minor planets/comets near position on a specific date.
            %   Given an OrbitalEl object with multiple elements, in which
            %   each elements contains vectors of multiple orbital
            %   elements, generate epehmerides and search for all minor
            %   planets and comets that are near position (cone search).
            %   The search is done in two steps. In the first iteration,
            %   rough positions are calculated for all orbital elements,
            %   while in the second iteration accurate positions are
            %   calculated for the objects near the position.
            % Input  : - An OrbitalEl object (multiple elements supported).
            %          - JD in TDB time scale.
            %          - J2000.0 R.A. of the position to search.
            %            Units are either 'deg'|'rad' (controlled via the
            %            'CooUnits' key/val).
            %          - J2000.0 Dec. of the position to search.
            %          - Search Radius. Default is 1000.
            %          * ...,key,val,...
            %            'SearchRadiusUnits' - Search Radius units.
            %                   Default is 'arcsec'.
            %            'CooUnits' - Search coordinate units.
            %                   Default is 'deg'.
            %            'MagLimit' - Magnitude limit. Default is Inf.
            %            'GeoPos' - Geodetic position of the observer (on
            %                   Earth). [Lon (rad), Lat (rad), Height (m)].
            %                   If empty, then calculate geocentric
            %                   positions. Default is [].
            %            'RefEllipsoid' - Reference ellipsoid for the
            %                   geodetic positions. Default is 'WGS84'.
            %            'OutUnitsDeg' - A logical indicating if the output
            %                   objects coordinates are in degrees (true)
            %                   or radians (false). Default is true.
            %            'coneSearchArgs' - A cell array of additional
            %                   arguments to pass to imProc.match.coneSearch
            %                   Default is {}.
            %            'AddDesignation' - A logical indicating if to add
            %                   the asteroid designation (in the last
            %                   column) to the output.
            %                   If true, then the output will be in a
            %                   format of table instead of a matrix.
            %                   Default is true.
            %            'QuickSearchBuffer' - In the first iteration the
            %                   search radius is increased by this amount.
            %                   Default is 500 (units given by the
            %                   'SearchBufferUnits' key/val).
            %            'SearchBufferUnits' - Units of
            %                   'QuickSearchBuffer'. Default is 'arcsec'.
            %            'Integration' - A logical indicating if to perform
            %                   orbital integartion on the objects within the
            %                   search radius.
            %                   Default is false.
            % Output : - An AstroCatalog object with the ephemerides of the
            %            minor planets / comets found near the search
            %            position. The number of elements are equal to the
            %            number of elements in the input OrbitalEl object.
            %            You can merge the results using AstroTable/merge.
            %          - A structure array (element per Result element)
            %            with the selected minor planets 'Number' and 'Designation'.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl= celestial.OrbitalEl.loadSolarSystem;
            %          [Result, Names] = searchMinorPlanetsNearPosition(OrbEl, 2451545, 0, 0, 1000)
            

            arguments
                Obj
                JD
                RA
                Dec
                SearchRadius             = 1000;
                Args.SearchRadiusUnits   = 'arcsec';
                Args.CooUnits            = 'deg';
                Args.MagLimit            = Inf;
                Args.GeoPos              = [];
                Args.RefEllipsoid        = 'WGS84';
                Args.OutUnitsDeg logical = true;
                Args.coneSearchArgs cell = {};
                Args.AddDesignation(1,1) logical = true;
                Args.QuickSearchBuffer   = 500;    % to be added to SearchRadis (same units).
                Args.SearchBufferUnits   = 'arcsec';
                Args.Integration logical = false;
            end
            
            SearchRadiusRAD      = convert.angular(Args.SearchRadiusUnits, 'rad', SearchRadius);
            QuickSearchBufferRAD = convert.angular(Args.SearchBufferUnits, 'rad', Args.QuickSearchBuffer);
            
            RA  = convert.angular(Args.CooUnits,'rad', RA);
            Dec = convert.angular(Args.CooUnits,'rad', Dec);
            
            
            if isinf(Args.MagLimit)
                IncludeMag = false;
            else
                IncludeMag = true;
            end
            
            ObjNew = Obj.copy();
            
            Nobj = numel(ObjNew);
            
            % quick and dirty
            for Iobj=1:1:Nobj
                Cat    = ephem(ObjNew(Iobj), JD, 'GeoPos',[], 'MaxIterLT',0, 'IncludeMag',IncludeMag, 'OutUnitsDeg',false, 'OutType','mat', 'AddDesignation',false);
                
                Dist   = celestial.coo.sphere_dist_fast(RA, Dec, Cat(:,2), Cat(:,3));
                
                % within search radius and MagLimit
                % RA - col 2
                % Dec - col 3
                % Mag - col 8
                Flag   = Dist<(SearchRadiusRAD + QuickSearchBufferRAD) & (Cat(:,8)<Args.MagLimit | isnan(Cat(:,8)));
            
                ObjNew(Iobj).selectFlag(Flag);
            end
            
            % accurate search on selected sample:
            Result = AstroCatalog(size(ObjNew));
            for Iobj=1:1:Nobj
                if  numEl(ObjNew(Iobj))==0
                    Flag = [];
                else
                    Result(Iobj) = ephem(ObjNew(Iobj), JD, 'GeoPos',Args.GeoPos,...
                                                  'RefEllipsoid',Args.RefEllipsoid,...
                                                  'OutUnitsDeg',false,...
                                                  'AddDesignation',Args.AddDesignation,...
                                                  'OutUnitsDeg',Args.OutUnitsDeg,...
                                                  'Integration',Args.Integration);


                    [Result(Iobj), Flag] = imProc.match.coneSearch(Result(Iobj), [RA, Dec], 'CooType','sphere',...
                                                      'Radius',SearchRadiusRAD,...
                                                      'RadiusUnits','rad',...
                                                      'CooUnits','rad',...
                                                      'CreateNewObj',false,...
                                                      Args.coneSearchArgs{:});
                end
                if isempty(ObjNew(Iobj).Number)
                    Names(Iobj).Number      = nan(sizeCatalog(Result(Iobj)));
                else
                    Names(Iobj).Number      = ObjNew(Iobj).Number(Flag);
                end
                if isempty(ObjNew(Iobj).Designation)
                    Names(Iobj).Designation = cell(sizeCatalog(Result(Iobj)));
                else
                    Names(Iobj).Designation = ObjNew(Iobj).Designation(Flag);
                end
            end
            
            
            
                        
        end
    end
    
    methods % conversion
        
        function TI=thiele_innes(Obj)
            % Convert orbital elements to Thiele-Innes elements
            % Description: Convert orbital elements to Thiele-Innes
            %              orbital elements.
            % Input  : - OrbitalEl object
            % Output : - Structure array with Thiele-Innes elements.
            % See also: celestial.Kepler.thiele_innes2el.m
            % Example: TI=thiele_innes(OrbEl(1));
            
            TI=celestial.Kepler.thiele_innes(Obj.A, Obj.W, Obj.Node, Obj.Incl);
        end

        
    end

    methods % display and organize
        function Result = table(Obj)
            % Generate a matlab table or orbital elements from OrbitalEl object.
            % Input  : - An OrbitalEl object.
            % Output : - A table.
            % Author : Eran Ofek (Sep 2021)
            % Example: E=celestial.OrbitalEl.loadSolarSystem;
            %          table(E)

            arguments
                Obj
            end

            Nobj = numel(Obj);
            Nel  = numEl(Obj);
            for Iobj=1:1:Nobj
                if isempty(Obj(Iobj).Number)
                    Number = nan(Nel(Iobj),1);
                else
                    Number = Obj(Iobj).Number;
                end
                if isempty(Obj(Iobj).Designation)
                    Designation = cell(Nel(Iobj),1);
                else
                    Designation = Obj(Iobj).Designation;
                end

                T = table(Number,...
                               Designation,...
                               Obj(Iobj).Node,...
                               Obj(Iobj).W,...
                               Obj(Iobj).Incl,...
                               Obj(Iobj).A,...
                               Obj(Iobj).PeriDist,...
                               Obj(Iobj).Eccen,...
                               Obj(Iobj).Tp);
                T.Properties.VariableNames = {'Number','Designation','Node','W','Incl','A','PeriDist','Eccen','Tp'};
                if Iobj==1
                    Result = T;
                else
                    Result = [Result; T];
                end

             end
             

                        

        end

    end
    
    methods (Static)   % upload orbital elenments
        function Result = loadSolarSystem(Type, Desig)
            % Load the JPL Solar System orbital elements from local disk
            %   To install the orbital elements use the Installer class.
            % Input  : - Type: [] - read all | 'num' | 'unnum' | 'comet'
            %            Default is [].
            %          - Minor planets designation (string) or number.
            %            If empty, return all. Default is [].
            % Output : - OrbitalEl object.
            %            Number of elements equal to the number of files
            %            read, and in each elements there may be multiple
            %            entries.
            % AUthor : Eran Ofek (Sep 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804)
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804)
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem([],'Ceres')
           
            arguments
                Type     = [];   % [] - read all | 'num' | 'unnum' | 'comet'
                Desig    = [];   % [] - read all
            end
            MJD0 = 2400000.5;
            
            if isempty(Type)
                Type = {'num','unnum','comet'};
            end
            if ischar(Type)
                Type = {Type};
            end
            
            Result = celestial.OrbitalEl;
            
            Ntype = numel(Type);
            for Itype=1:1:Ntype
                switch lower(Type{Itype})
                    case 'num'
                        T = Installer.readElementsFileJPL('ELEMENTS.NUMBR');
                        if isempty(Desig)
                            Flag = true(numel(T.e),1);
                        else
                            if isnumeric(Desig)
                                Flag = ismember(T.Number, Desig);
                            else
                                % cell array or string
                                Flag = ismember(T.Designation, Desig);
                            end
                        end
                        if sum(Flag)>0
                            Result(Itype).Number      = T.Number(Flag);
                            Result(Itype).Designation = T.Designation(Flag);
                            Result(Itype).Epoch       = T.Epoch(Flag) + MJD0;
                            Result(Itype).A           = T.a(Flag);
                            Result(Itype).Eccen       = T.e(Flag);
                            Result(Itype).Incl        = T.i(Flag);
                            Result(Itype).W           = T.w(Flag);
                            Result(Itype).Node        = T.Node(Flag);
                            Result(Itype).Mepoch      = T.M(Flag);
                            Result(Itype).MagPar      = [T.H(Flag), T.G(Flag)];
                            Result(Itype).MagType     = 'HG';
                            Result(Itype).Ref         = T.Ref(Flag);
                        end
                    case 'unnum'
                        T = Installer.readElementsFileJPL('ELEMENTS.UNNUM');
                        if isempty(Desig)
                            Flag = true(numel(T.e),1);
                        else
                            if isnumeric(Desig)
                                Flag = false(numel(T.e),1);
                            else
                                % cell array or string
                                Flag = ismember(T.Designation, Desig);
                            end
                        end
                        if sum(Flag)>0
                            Result(Itype).Designation = T.Designation(Flag);
                            Result(Itype).Epoch       = T.Epoch(Flag) + MJD0;
                            Result(Itype).A           = T.a(Flag);
                            Result(Itype).Eccen       = T.e(Flag);
                            Result(Itype).Incl        = T.i(Flag);
                            Result(Itype).W           = T.w(Flag);
                            Result(Itype).Node        = T.Node(Flag);
                            Result(Itype).Mepoch      = T.M(Flag);
                            Result(Itype).MagPar      = [T.H(Flag), T.G(Flag)];
                            Result(Itype).MagType     = 'HG';
                            Result(Itype).Ref         = T.Ref(Flag);
                        end
                    case 'comet'
                        T = Installer.readElementsFileJPL('ELEMENTS.COMET');
                        if isempty(Desig)
                            Flag = true(numel(T.e),1);
                        else
                            if isnumeric(Desig)
                                Flag = false(numel(T.e),1);
                            else
                                % cell array or string
                                Flag = ismember(T.Designation, Desig);
                            end
                        end
                        if sum(Flag)>0
                            Result(Itype).Designation = T.Designation(Flag);
                            Result(Itype).Epoch       = T.Epoch(Flag) + MJD0;
                            Result(Itype).PeriDist    = T.q(Flag);
                            Result(Itype).Eccen       = T.e(Flag);
                            Result(Itype).Incl        = T.i(Flag);
                            Result(Itype).W           = T.w(Flag);
                            Result(Itype).Node        = T.Node(Flag);
                            % Tp is stored in [yyyymmdd.frac]
                            Tp = T.Tp(Flag);
                            Frac = mod(Tp,1);
                            DV   = datevec(cellfun(@num2str, num2cell(Tp),'UniformOutput',false),'yyyymmdd');
                            DV   = [DV(:,1:3), Frac];
                            Result(Itype).Tp          = celestial.time.julday(DV(:,[3 2 1 4]));
                            %Result(Itype).Tp          = T.Tp(Flag);
                            Result(Itype).Ref         = T.Ref(Flag);
                        end
                    otherwise
                        error('Unknown Type option');
                end
            end
                
        end
        
        function Result = randomElements(N, Args)
            % Generate random orbital elements
            % Input  : - Number of random elements to generate.
            %          * ...,key,val,...
            %            'A' - Semi major axis. Default is 1.
            %            'RangeE' - Eccntricity range.
            %                   Default is [0, 0.9].
            %            'Epoch' - Epoch of observations.
            %                   Default is 0.
            % Output : - An OrbitalEl object with random elements.
            % Author : Eran Ofek (Apr 2022)
            % Example: R = celestial.OrbitalEl.randomElements;
            
            arguments
                N             = 1e6;
                Args.A        = 1;
                Args.RangeE   = [0, 0.9];
                Args.Epoch    = 0;
            end
            
            Result = celestial.OrbitalEl;
            Result.Number    = (1:1:N).';
            Result.A         = ones(N,1).*Args.A;
            Result.Node      = rand(N,1).*360;
            Result.W         = rand(N,1).*360;
            Result.Incl      = rand(N,1).*180 - 90;
            Result.Eccen     = rand(N,1).*range(Args.RangeE) + min(Args.RangeE);
            Result.Epoch     = ones(N,1).*Args.Epoch;
            Result.Mepoch    = rand(N,1).*360;
            
            
        end
        
        function Result = compareEphem2JPL(Args)
            % Compare ephemeris with JPL ephemeris
            %   A function for testing the performences of ephem
            % Example: VecJD = ((2460110.5 - 1000):10:(2460110.5 +100))';
            %          R1=celestial.OrbitalEl.compareEphem2JPL('StartJD',VecJD(1),'EndJD',VecJD(end));
            %          R2=celestial.OrbitalEl.compareEphem2JPL('StartJD',VecJD(1),'EndJD',VecJD(end),'Integration',true);
            %          plot(VecJD,R1(:,1)); hold on; plot(VecJD,R2(:,1))
            
            arguments
                Args.ObjectInd           = 9804;
                Args.StartJD             = 2460110.5 - 1000;
                Args.StepSize            = 10;
                Args.EndJD               = 2460110.5 + 100;
                Args.GeodPos             = [];  % [deg deg m]
                Args.Integration logical = false;
            end
            
            RAD = 180./pi;
            
            %JD = celestial.time.julday([19 9 2021])+(0:1./24:1)';
            %          Coo=[-116.865./RAD 33.3563./RAD 2000]
            OrbEl1 = celestial.OrbitalEl.loadSolarSystem([],Args.ObjectInd);
            if ~isempty(Args.GeodPos)
                GeodPosKM = [Args.GeodPos(1:2), Args.GeodPos(3)./1000];
            else
                GeodPosKM = [];
            end
            VecJD  = (Args.StartJD:Args.StepSize:Args.EndJD)';
            CatE   = ephem(OrbEl1, VecJD, 'GeoPos',Args.GeodPos, 'OutUnitsDeg',false, 'Integration',Args.Integration);
            
            CatJPL = celestial.SolarSys.jpl_horizons('ObjectInd',num2str(Args.ObjectInd),'StartJD',Args.StartJD,'StopJD',Args.EndJD,...
                                                     'StepSize',Args.StepSize, 'StepSizeUnits','d','CENTER','500', 'GeodCoo',GeodPosKM);
            % RA nd Dec diff between JPL and ephem:
            Result = [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*RAD.*3600;
            
        end
        
    end


    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
