% celestial.OrbitalEl - A class for storing and manipulating orbital elements, and two body motion calculations.
% Author : Eran Ofek (Jan 2022)
%
% Comments on usage:
% Each element of the object may contain multiple orbital parameters.
% Typically, orbital elements of a single object as a function of epoch are stored in the same row,
% while orbital elements for different targets are stored in different
% rows.
% 
% Examples:
% % To download the JPL orbital elements use the Installer.
% % Load arguments of all numbered asteroids
% OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
% % Load parameters for numbreed, unnumbered and comets
% OrbEl = celestial.OrbitalEl.loadSolarSystem([]);
% % Load parameters for asteroid number 9804
% OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
%
% % Generate ephemeris:
% OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
% JD = 2451545;
% CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',1,'IncludeMag',false);
% CatE = ephem(OrbEl, JD, 'GeoPos',[],'MaxIterLT',2,'IncludeMag',true); 
%
% % or for a single object and multiple times:
% OrbEl = celestial.OrbitalEl.loadSolarSystem('num',9804);
% CatE = ephem(OrbEl, JD+(1:1:100)', 'GeoPos',[]);
%
%
%
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
%         function Result = get.A(Obj)
%             % getter for A (semi-major axis)
%            
%             if isempty(Obj.A)
%                 % check if PeriDist and Eccen are available
%                 if ~isempty(Obj.PeriDist) && ~isempty(Obj.Eccen)
%                     % calc A
%                     Obj.A = Obj.PeriDist./(1 - Obj.Eccen);
%                 end
%             end
%             Result = Obj.A;
%         end
%         
%         function Result = get.PeriDist(Obj)
%             % getter for A (semi-major axis)
%            
%             if isempty(Obj.PeriDist)
%                 % check if A and Eccen are available
%                 if ~isempty(Obj.A) && ~isempty(Obj.Eccen)
%                     % calc PeriDist
%                     Obj.PeriDist = Obj.A.*(1 - Obj.Eccen);
%                 end
%             end
%             Result = Obj.PeriDist;
%         end
%         
%         function Result = get.Tp(Obj)
%             % getter for periapsis time [JD]
%             
%             if isempty(Obj.Tp) && (~isempty(Obj.Mepoch) && ~isempty(Obj.A))
%                 Obj.Tp = Obj.Epoch - Obj.Mepoch./Obj.meanMotion(Obj.AngUnits);
%             end
%             
%             Obj.Tp = real(Obj.Tp);
%             
%             Result = Obj.Tp;
%             
%         end
    end
    
    methods % basic functions
        function Obj = populate(Obj)
            % Populate A, PeriDist, Tp from other parameters.
            % Author : Eran Ofek (Nov 2023)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          OrbEl.populate;
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Obj(Iobj).A)
                    % check if PeriDist and Eccen are available
                    if ~isempty(Obj(Iobj).PeriDist) && ~isempty(Obj(Iobj).Eccen)
                        % calc A
                        Obj(Iobj).A = Obj(Iobj).PeriDist./(1 - Obj(Iobj).Eccen);
                    end
                end
                if isempty(Obj(Iobj).PeriDist)
                    % check if A and Eccen are available
                    if ~isempty(Obj(Iobj).A) && ~isempty(Obj(Iobj).Eccen)
                        % calc PeriDist
                        Obj(Iobj).PeriDist = Obj(Iobj).A.*(1 - Obj(Iobj).Eccen);
                    end
                end
                if isempty(Obj(Iobj).Tp) && (~isempty(Obj(Iobj).Mepoch) && ~isempty(Obj(Iobj).A))
                    Obj(Iobj).Tp = Obj(Iobj).Epoch - Obj(Iobj).Mepoch./Obj(Iobj).meanMotion(Obj(Iobj).AngUnits);
                end

                Obj(Iobj).Tp = real(Obj(Iobj).Tp);
            end
            
        end
        
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
            %            ecliptic J2000). [au/day]
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
            
        function [X0, V0, JD, INPOP] = elements2pos(Obj, Args)
            % Get rectangular coordinates and velocity from orbital elements at some epoch.
            % Input  : - A single element celestial.OrbitalEl object.
            %          * ...,key,val,...
            %            'JD' - JD at which to evaulate the objects
            %                   coordonates.
            %                   If empty, then will be evaulated at the
            %                   orbotal elements epoch (.Epoch prop).
            %                   Default is [].
            %            'TimeScale' - Time scale (for barycentric
            %                   calcualtions). Default is 'TDB'.
            %            'CooSys' - Coordinate system:
            %                   'eq' - J2000.0 equatorial.
            %                   'ec' - J2000.0 ecliptic.
            %                   Default is 'eq'.
            %            'RefFrame' - Reference frame:
            %                   'helio' - heliocentric.
            %                   'bary' - barycentric.
            %                   Default is 'bary'.
            %            'Tol' - Tolerance for Kepler equation solver.
            %                   Default is 1e-8.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty, will be constructed.
            %                   Default is [].
            % Output : - A 3 x N matrix of [X;Y;Z] positions. Colum per
            %            orbital element.
            %          - A 3 x N matrix of [X;Y;Z] velocities. Colum per
            %            orbital element.
            %          - JD of position.
            %          - A populated INPOP object.
            % Author : Eran Ofek (Nov 2023)
            % Example: EA = celestial.OrbitalEl.loadSolarSystem('num');
            %          [X, V] = elements2pos(EA);
            %          E = celestial.OrbitalEl.loadSolarSystem('num',9804);
            %          [X, V, JD0] = elements2pos(E, 'CooSys','ec');
            %          [T] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD0, 'StopTime',JD0+0.5,'CENTER','500@0');
            %          [T.X;T.Y;T.Z]-X
            
            arguments
                Obj(1,1)
                Args.JD        = [];      % if empty, use Epoch
                Args.TimeScale = 'TDB';
                Args.CooSys    = 'eq';    % 'ec'
                Args.RefFrame  = 'bary'; % 'bary'
                Args.Tol       = 1e-8;
                Args.INPOP     = [];
            end
            
            if isempty(Args.JD)
                Args.JD = Obj.Epoch;
            end
            
            % Get initial X, Y, Z, VX, VY, VZ of target
            % Heliocentric system
            [Nu0]    = keplerSolve(Obj, Args.JD, 'Tol',Args.Tol);
            [V0, X0] = trueAnom2rectVel(Obj,Nu0,[],[]);  % ecliptic J2000
            switch lower(Args.CooSys)
                case 'ec'
                    % do nothing
                    IsEcOut = true;
                case 'eq'
                    RotM = celestial.coo.rotm_coo('E');
                    X0 = RotM*X0;
                    V0 = RotM*V0;
                    IsEcOut = false;
                otherwise
                    error('Unknown CooSys option');
            end
            
            switch lower(Args.RefFrame)
                case 'helio'
                    % do nothing
                case 'bary'
                    % get sun position
                    if isempty(Args.INPOP)
                        Args.INPOP = celestial.INPOP;
                        Args.INPOP.populateTables('Sun');
                        Args.INPOP.populateTables('Sun','FileData','vel');
                    end
                    
                    S_B  = Args.INPOP.getPos('Sun',Args.JD, 'TimeScale',Args.TimeScale, 'IsEclipticOut',IsEcOut, 'OutUnits','au');
                    Sv_B = Args.INPOP.getVel('Sun',Args.JD, 'TimeScale',Args.TimeScale, 'IsEclipticOut',IsEcOut, 'OutUnits','au');
                    
                    X0   = X0 + S_B;
                    V0   = V0 + Sv_B;
                otherwise
                    error('Unknown RefFrame option');
            end
                            
            JD    = Args.JD;
            INPOP = Args.INPOP;
        end
    end
    
    methods % ephemerides
        function [Result, ColNames, ColUnits] = prepEphemOutput(Obj, Time, AllU, AllU_B, AllE_H, AllE_dotH, Args)
            % Prepare ephemeris output
            %   Given U, U_B, E_dotH matrices, prepare ephemeris output.
            %   Internal function for the ephem family of functions.
            % Input  : - A celestial.OrbitalEl object.
            %            Needed for the magnitude parameters.
            %          - A vector of (Nt) times for which the positions were
            %            calculated.
            %          - A 3xNt U matrix in which each column is the XYZ
            %            position at the corresponding time.
            %            U represent the target topocentric position in the
            %            equatorial J2000 system. Units are AU.
            %          - Like the U matrix, but for U_B - the target
            %            barycentrix position.
            %          - Like the U matrix, but for E_H - the topocentric
            %            heliocentric poistion. Default is [].
            %          - Like the U matrix, but for E_dotH - the
            %            topocentrix heliocentric/barycentric velocity
            %            [AU/day]. This is used for abberation of light.
            %            If empty, then ignore.
            %            Default is [].
            %          * ...,key,val,...
            %            'OutType' - Output type:
            %                   'mat' - A matrix output.
            %                   'AstroCatalog' - An AstroCatalog object.
            %                   'table' - A table.
            %            'OrbEl' - An optional celestial.OrbitalEl object.
            %                   Will be used for the mag calculations.
            %                   If empty, then exclude mag.
            %                   Default is [].
            %
            %            'OutUnitsDeg' - A logical indicating if to list
            %                   the RA and Dec in degrees. If false list in
            %                   radians. Default is true.
            %            'Aberration' - A logical indicating if to include
            %                   aberration of light. Default is false.
            %                   Note that for the default (false) the
            %                   output is in an "astrometric" reference
            %                   frame (i.e., relative to the stars).
            %            'IncludeMag' - A logical indicating if to add
            %                   magnitude to the output table.
            %                   Default is true.
            %            'IncludeAngles' - A logical indicating if to
            %                   include angles. Default is true.
            %            'IncludeDesignation' - A logical indicatig if to
            %                   include desigmation.
            %                   Default is true.
            % Output : - An output table, matrix or AstroCatalog
            %          - A cell array of column names.
            %          - A cell array of column units.
            % Author : Eran Ofek (Nov 2023)            
            
            arguments
                Obj
                Time
                AllU
                AllU_B
                AllE_H
                AllE_dotH                  = [];
                
                Args.OutType               = 'AstroCatalog'; % 'mat'|'astrocatalog'|'table'
                                
                Args.Aberration logical    = false;
                Args.OutUnitsDeg logical   = true;
                Args.IncludeMag logical    = true;
                Args.IncludeAngles logical = true;
                Args.IncludeDesignation logical = true;
            end
            
            if Args.OutUnitsDeg
                AngUnits = 'deg';
            else
                AngUnits = 'rad';
            end
            
            % Topocentric distance
            Delta = sqrt(sum(AllU.^2, 1));

            R     = sqrt(sum(AllU_B.^2, 1));

            
            % U2 is already in equatorial caretesian coordinates
            [RA, Dec, Delta] = celestial.SolarSys.cart2eqAng(AllU, 'InputSys','eq', 'Delta',Delta, 'Aberration',Args.Aberration, 'E_dotH',AllE_dotH, 'OutUnitsDeg',Args.OutUnitsDeg);

            ColNames = {'JD','RA','Dec','R','Delta'};
            ColUnits = {'day',AngUnits,AngUnits,'au','au'};
            Nra      = numel(RA);
            Cat = [Time(:).*ones(Nra,1), RA(:), Dec(:), R(:), Delta(:)]; % Ang_SOT(:), Ang_STO(:), Mag(:)];
                
            % calculate angles
            if Args.IncludeAngles
                R_obs_sun = sqrt(sum(AllE_H.^2, 1));  % Sun-Earth distance
                [Ang_SOT, Ang_STO, Ang_TSO] = celestial.SolarSys.anglesFromDistances(R_obs_sun, R, Delta, Args.OutUnitsDeg);
                
                Cat = [Cat, Ang_SOT(:), Ang_STO(:), Ang_TSO(:)];
                ColNames = [ColNames, 'SOT', 'STO', 'TSO'];
                ColUnits = [ColUnits, AngUnits, AngUnits, AngUnits];
            end
            
            if Args.IncludeMag
                
                Mag = magnitude(Obj, R(:), Delta(:), Ang_STO(:), 'PhaseUnits','deg');
            
                Cat      = [Cat, Mag];
                ColNames = [ColNames, 'Mag'];
                ColUnits = [ColUnits, 'mag'];
            end
                
            Desig = [];
            if Args.IncludeDesignation
                if isempty(Obj.Designation)
                    Designation = Obj.Number;
                else
                    Designation = Obj.Designation;
                end
                Ncat = size(Cat,1);
                if isnumeric(Designation)
                    Desig = Designation(:).*ones(Ncat,1);
                end
                if ischar(Designation)
                    Designation = {Designation};
                end
                if iscell(Designation)
                    if numel(Designation)==1
                        Desig = repmat(Designation, Ncat, 1);
                    else
                        % assume number of elements equal to the number of
                        % designations
                        Desig = Designation;
                    end
                end
                ColUnits = [ColUnits, ''];
            end
                                                           
            switch lower(Args.OutType)
                case 'mat'
                    Result = Cat;
                case 'astrocatalog'
                    %Result = AstroCatalog({Cat}, 'ColNames',ColNames', 'ColUnits',ColUnits);
                    Result          = AstroCatalog;
                    if isempty(Desig)
                        Result.Catalog  = Cat;
                        Result.ColNames = ColNames;
                        Result.ColUnits = ColUnits;
                    else
                        Cat = array2table(Cat, 'VariableNames',ColNames);
                        Cat.Properties.VariableUnits = ColUnits;
                        Cat.Desig    = Desig;
                        Result.Catalog  = Cat;
                        Result.ColNames = Cat.Properties.VariableNames;
                        Result.ColUnits = Cat.Properties.VariableUnits;
                    end
                    
                case 'table'
                    Result = array2table(Cat, 'VariableNames',ColNames);
                    Result.Properties.VariableUnits = ColUnits;
                    if ~isempty(Desig)
                        Result.Desig    = Desig;
                    end
                otherwise
                    error('Unknown OutType option');
            end
            
        end
    
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
                
        function Result = ephemKeplerMultiObj(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object by solving the Kepler equation.
            %   This function is optimized for multi objects.
            %   See ephemKeplerMultiTime for a function optimied for
            %   multiple times.
            %
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %   This is like the ephem function, but not including the
            %   orbital integation option.
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
            %                   'vsop87' - VSOP87 (default).
            %                   'inpop' - INPOP.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty, will generate one.
            %            'TimeScale' - Time scale of JD. Relevant only if
            %                   EarthEphem='INPOP'.
            %                   Default is 'TDB'.
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
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
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
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric equatorial coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
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
            %          Cat = ephemKeplerMultiObj(OrbEl, JD +(1:10:100)');
            %
            %          %Single time, multiple objects:
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Cat = ephemKeplerMultiObj(OrbEl, JD);
            
            arguments
                Obj(1,1)
                Time
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.OutUnitsDeg(1,1) logical    = true;
                Args.Aberration(1,1) logical     = false; %false;
                Args.EarthEphem                  = 'inpop'; %'vsop87';  % 'vsop87' | 'inpop'
                Args.INPOP                       = [];
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';  % 'mat' | 'AstroCatalog'
                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                Args.IncludeMag(1,1) logical     = true;  % use false to speed up
                Args.AddDesignation(1,1) logical = true;  % works only for AstroCatalog output
                %Args.Integration(1,1) logical    = false; %false; 
                %Args.TolInt                      = 1e-10; 
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
            
            % Rotation matrix: Ecliptic to Equatorial, J2000
            RotMatEc2Eq = celestial.coo.rotm_coo('E');
            
            for It=1:1:Nt
                LightTime             = 0;
                for Iter=1:1:Args.MaxIterLT
                    
                    % Solution via Kepler equation
                    % Target, Heliocentric, ecliptic
                    [Nu, R]          = keplerSolve(Obj, Time(It)-LightTime,'Tol',Args.Tol);
                    % Target, Ecliptic Heliocentric rect. position
                    [U_B] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                    U_B   = U_B.';  % a 3 X N matrix
                    % convert to Equatorial, Heliocentric
                    U_B   = RotMatEc2Eq * U_B;
             
                    [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time(It), 'CooSys','h',...
                                                                                'RefFrame','eq',...
                                                                                'INPOP',Args.INPOP,...
                                                                                'ObserverEphem',Args.ObserverEphem,...
                                                                                'SunLightTime',LightTime,...
                                                                                'RefEllipsoid',Args.RefEllipsoid);

                    U = U_B - E_H;  % U_B(t-tau)
                    Delta = sqrt(sum(U.^2, 1));

                    %PrevLightTime = LightTime;
                    LightTime = Delta./Caud;
                   
                end
                R     = sqrt(sum(U_B.^2, 1));

                % ignore light deflection
                if Args.Aberration
                    U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta);
                else
                    U2 = U;
                end

                % U2 is already in equatorial caretesian coordinates
                [RA, Dec, Delta] = celestial.SolarSys.cart2eqAng(U2, 'InputSys','eq', 'Delta',Delta, 'Aberration',Args.Aberration, 'E_dotH',E_dotH, 'OutUnitsDeg',Args.OutUnitsDeg);
                
                % calculate angles
                R_obs_sun = sqrt(sum(E_H.^2, 1));  % Sun-Earth distance
                [Ang_SOT, Ang_STO, Ang_TSO] = celestial.SolarSys.anglesFromDistances(R_obs_sun, R, Delta, Args.OutUnitsDeg);
                
                
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
                    %Result = AstroCatalog({Cat}, 'ColNames',ColNames', 'ColUnits',ColUnits);
                    Result          = AstroCatalog;
                    Result.Catalog  = Cat;
                    Result.ColNames = ColNames;
                    Result.ColUnits = ColUnits;
                otherwise
                    error('Unknown OutType option');
            end
                
            
            
            
        end
        
        function Result = ephemKeplerMultiTime(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object by solving the Kepler equation.
            %   This function is optimized for multi times.
            %   See ephemKeplerMultiObj for a function optimied for
            %   multiple objects.
            %
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %   This is like the ephem function, but not including the
            %   orbital integation option.
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
            %                   'vsop87' - VSOP87 (default).
            %                   'inpop' - INPOP.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty, will generate one.
            %            'TimeScale' - Time scale of JD. Relevant only if
            %                   EarthEphem='INPOP'.
            %                   Default is 'TDB'.
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
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
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
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric equatorial coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
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
            %          Cat = ephemKeplerMultiTime(OrbEl, JD +(1:10:100)');
            %
            %          %Single time, multiple objects:
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Cat = ephemKeplerMultiTime(OrbEl, JD);
            %          tic;CatE = ephemKeplerMultiTime(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false);toc    
            %
            %          % This time with defining INPOP prior to run
            %          IN = celestial.INPOP;
            %          IN.populateTables('all');
            %          IN.populateTables('all','FileData','vel');
            %          tic;CatE = ephemKeplerMultiTime(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false,'INPOP',IN);toc    
            
            
            arguments
                Obj(1,1)
                Time
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.OutUnitsDeg(1,1) logical    = true;
                Args.Aberration(1,1) logical     = false; %false;
                Args.EarthEphem                  = 'inpop'; %'vsop87';  % 'vsop87' | 'inpop'
                Args.INPOP                       = [];
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';  % 'mat' | 'AstroCatalog'
                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                Args.IncludeMag(1,1) logical     = true;  % use false to speed up
                Args.AddDesignation(1,1) logical = true;  % works only for AstroCatalog output
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
            end
            RAD  = 180./pi;
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            Nt      = numel(Time);
            Ntarget = numEl(Obj);
            if Ntarget>1
                error('This function can be used for a single object and multiple times - see instead ephemKeplerMultiObj');
            end
            
            Ncat = Nt; %max(Nt, Ntarget);
            
            ColNames      = {'JD', 'RA', 'Dec', 'R', 'Delta','SOT','STO', 'Mag'};
            if Args.OutUnitsDeg
                ColUnits      = {'day','deg','deg', 'au','au','deg','deg','mag'};
            else
                ColUnits      = {'day','rad','rad', 'au','au','deg','deg','mag'};
            end
            Cat           = nan(Ncat, numel(ColNames));
            
            % Rotation matrix: Ecliptic to Equatorial, J2000
            RotMatEc2Eq = celestial.coo.rotm_coo('E');
            
            LightTime             = 0;
            for Iter=1:1:Args.MaxIterLT

                % Solution via Kepler equation
                % Target, Heliocentric, ecliptic
                [Nu, R, E, Vel, M]          = keplerSolve(Obj, Time(:)-LightTime(:),'Tol',Args.Tol);
                % Target, Ecliptic Heliocentric rect. position
                [U_B] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                U_B   = U_B.';  % a 3 X N matrix
                % convert to Equatorial, Heliocentric
                U_B   = RotMatEc2Eq * U_B;

                [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time(:), 'CooSys','h',...
                                                                        'RefFrame','eq',...
                                                                        'INPOP',Args.INPOP,...
                                                                        'ObserverEphem',Args.ObserverEphem,...
                                                                        'SunLightTime',LightTime(:),...
                                                                        'RefEllipsoid',Args.RefEllipsoid);

                U = U_B - E_H;  % U_B(t-tau)

                Delta = sqrt(sum(U.^2, 1));

                %PrevLightTime = LightTime;
                LightTime = Delta./Caud;

            end
            R     = sqrt(sum(U_B.^2, 1));

            % ignore light deflection
            if Args.Aberration
                U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta);
            else
                U2 = U;
            end

            % U2 is already in equatorial caretesian coordinates
            [RA, Dec, Delta] = celestial.SolarSys.cart2eqAng(U2, 'InputSys','eq', 'Delta',Delta, 'Aberration',Args.Aberration, 'E_dotH',E_dotH, 'OutUnitsDeg',Args.OutUnitsDeg);

            % calculate angles
            R_obs_sun = sqrt(sum(E_H.^2, 1));  % Sun-Earth distance
            [Ang_SOT, Ang_STO, Ang_TSO] = celestial.SolarSys.anglesFromDistances(R_obs_sun, R, Delta, Args.OutUnitsDeg);


            if Args.IncludeMag
                Mag = magnitude(Obj, R(:), Delta(:), Ang_STO(:), 'PhaseUnits','deg');
            else
                Mag = nan(size(RA));
            end
                
            % assume single orbital element and multiple times
            Cat = [Time(:), RA(:), Dec(:), R(:), Delta(:), Ang_SOT(:), Ang_STO(:), Mag(:)];
            
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
                    Result          = AstroCatalog;
                    Result.Catalog  = Cat;
                    Result.ColNames = ColNames;
                    Result.ColUnits = ColUnits;
                    %Result = AstroCatalog({Cat}, 'ColNames',ColNames', 'ColUnits',ColUnits);
                otherwise
                    error('Unknown OutType option');
            end
                            
            
        end
        
        function Result = ephem(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object.
            %   This function calls ephemKeplerMultiObj or
            %   ephemKeplerMultiTime, or ephemIntegrate.
            %
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %   This is like the ephem function, but not including the
            %   orbital integation option.
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
            %                   'vsop87' - VSOP87 (default).
            %                   'inpop' - INPOP.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty, will generate one.
            %            'TimeScale' - Time scale of JD. Relevant only if
            %                   EarthEphem='INPOP'.
            %                   Default is 'TDB'.
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
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
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
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric equatorial coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
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
            %          Cat = ephemKeplerMultiObj(OrbEl, JD +(1:10:100)');
            %
            %          %Single time, multiple objects:
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Cat = ephemKeplerMultiObj(OrbEl, JD);
            %          tic;CatE = ephemKepler(OrbEl, JD, 'GeoPos',[],'MaxIterLT',0,'IncludeMag',false);toc           
            
            arguments
                Obj
                Time
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.OutUnitsDeg(1,1) logical    = true;
                Args.Aberration(1,1) logical     = false; %false;
                Args.EarthEphem                  = 'inpop'; %'vsop87';  % 'vsop87' | 'inpop'
                Args.INPOP                       = [];
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';  % 'mat' | 'AstroCatalog'
                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                Args.IncludeMag(1,1) logical     = true;  % use false to speed up
                Args.AddDesignation(1,1) logical = true;  % works only for AstroCatalog output
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
                Args.Integrate(1,1) logical      = false; %false; 
                Args.TolInt                      = 1e-10; 
                
            end
            
            Ntarget = Obj.numEl;
            Ntime   = numel(Time);
            
            if Ntarget>1 && Ntime>1
                error('Number of targets or number of times must be 1');
            end
            
            if Args.Integrate
                % orbital integration
                error('Integrate not ready');
            else
                % use kepler equation
                if Ntarget==1
                    Result = ephemKeplerMultiTime(Obj, Time, 'Tol',Args.Tol,...
                                                             'TolLT',Args.TolLT,...
                                                             'OutUnitsDeg',Args.OutUnitsDeg,...
                                                             'Aberration',Args.Aberration,...
                                                             'EarthEphem',Args.EarthEphem,...
                                                             'INPOP',Args.INPOP,...
                                                             'TimeScale',Args.TimeScale,...
                                                             'GeoPos',Args.GeoPos,...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'OutType',Args.OutType,...
                                                             'MaxIterLT',Args.MaxIterLT,...
                                                             'IncludeMag',Args.IncludeMag,...
                                                             'AddDesignation',Args.AddDesignation,...
                                                             'ObserverEphem',Args.ObserverEphem);
                
                else
                    % Ntime==1
                    Result = ephemKeplerMultiObj(Obj, Time, 'Tol',Args.Tol,...
                                                             'TolLT',Args.TolLT,...
                                                             'OutUnitsDeg',Args.OutUnitsDeg,...
                                                             'Aberration',Args.Aberration,...
                                                             'EarthEphem',Args.EarthEphem,...
                                                             'INPOP',Args.INPOP,...
                                                             'TimeScale',Args.TimeScale,...
                                                             'GeoPos',Args.GeoPos,...
                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                             'OutType',Args.OutType,...
                                                             'MaxIterLT',Args.MaxIterLT,...
                                                             'IncludeMag',Args.IncludeMag,...
                                                             'AddDesignation',Args.AddDesignation,...
                                                             'ObserverEphem',Args.ObserverEphem);
                end
            end
                    
                            
        end
      
        % done
        function [Result,ColNames,ColUnits] = ephemIntegrateMultiTime1dir(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object by integrating the equation of motion 
            %   taking into account all the major bodies in the Solar
            %   System. Optimized for a single object and multiple times
            %   all larger or smaller relative to the Epoch.
            %
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %
            %   For definitions and formulae, see Explanatory Supplement to the Astronomical
            %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
            % 
            %
            % Input  : - A single element OrbitalEl object.
            %            This object may include multiple orbital elements
            %            in vectors of parameters.
            %          - A vector of JD in the TDT time scale.
            %            Only for a single object (see
            %            ephemIntegrateMultiObj for multiple objects
            %            integration).
            %          * ...,key,val,...
            %            'TimeScale' - Time scale of integration:
            %                   'TDB'|'TT'
            %                   Default is 'TDB'.
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
            %
            %            'IncludeMag' - A logical indicating if to include
            %                   magnitude in output catalog.
            %                   Default is true.
            %            'IncludeAngles' - A logical indicating if to include
            %                   angles in output catalog.
            %                   Default is true.
            %            'IncludeDesignation' - A logical indicating if to
            %                   include asteroid designation.
            %                   Default is true.
            %            'INPOP' - A celestial.INPOP object that will be
            %                   used for the Earth position and perturbarions.
            %                   If empty, then the object will be loaded.
            %                   Default is [].
            %
            %            'Tol' - Tolerance [rad] for solving the Kepler
            %                   equation. Default is 1e-8.
            %            'TolLT' - Tolerance [day] for the light-time
            %                   correction iterations. Default is 1e-6.
            %            'TolInt' - Integration tolerance.
            %                   Default is 1e-8.
            %            'OutUnitsDeg' - A logical indicating if to list
            %                   the RA and Dec in degrees. If false list in
            %                   radians. Default is true.
            %
            %            'Aberration' - A logical indicating if to include
            %                   aberration of light. Default is false.
            %                   Note that for the default (false) the
            %                   output is in an "astrometric" reference
            %                   frame (i.e., relative to the stars).
            %            'EarthEphem' - Earth ephemeris to use:
            %                   'vsop87' - VSOP87
            %                   'inpop' - INPOP (default).
            %            'MaxIterLT' - Maximum numbre of iterations for
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
            %                   (e.g., for quick calculation).
            %
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
            %                   If empty, the function will use EarthEphem and GeoPos.
            %                   In case of size [Nepoch,3], the function assume zero velocity.
            %                   Defauls is [].
            % Output : - Ephemeris (AstroCatalog, table or , matrix).
            %          - A cell array of column names in the ephemeris
            %            matrix.
            %          - A cell array of column units.
            % Author : Eran Ofek (Nov 2023)
            % Example: 
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem([],[9804]);
            %          JD=ceil(OrbEl.Epoch)+0.5;
            %          CatInt = ephemIntegrateMultiTime1dir(OrbEl, JD+(0:100:5000).');
            %          Cat = ephemKeplerMultiTime(OrbEl, JD+(0:100:5000).');
            %          [T,~,U] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','OBSERVER','TimeScale','TT','StartTime',JD,'StopTime',JD+5000,'StepSize',100);
            %          [(Cat.Catalog.RA-T.RA).*3600, (CatInt.Catalog.RA-T.RA).*3600, (CatInt.Catalog.Dec-T.Dec).*3600]
            
            arguments
                Obj(1,1)
                Time
                
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';
                Args.IncludeMag logical          = true;  % use false to speed up
                Args.IncludeAngles logical       = true;
                Args.IncludeDesignation logical  = true;  % works only for AstroCatalog output
                Args.INPOP                       = [];
                
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.TolInt                      = 1e-8; 
                
                Args.OutUnitsDeg(1,1) logical    = true;
                
                Args.Aberration(1,1) logical     = false;
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
                Args.EarthEphem                  = 'INPOP';  % 'vsop87' | 'inpop'

                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                
            end
            RAD  = 180./pi;
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            Nt      = numel(Time);
            Ntarget = numEl(Obj);
            if ~(Nt==1 || Ntarget==1)
                error('Number of epochs or number of targets must be 1');
            end
            Ncat = max(Nt, Ntarget);
                        
            % populate INPOP if needed
            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateTables('all');
                Args.INPOP.populateTables('all','FileData','vel');
            end
            
            % Get initial X, Y, Z, VX, VY, VZ of target
            % Heliocentric system
            %[Nu0]  = keplerSolve(Obj, Obj.Epoch, 'Tol',Args.Tol);
            %[V0,X0] = trueAnom2rectVel(Obj,Nu0,[],[]);  % ecliptic J2000
            
            
            % comparing to horizons
            % [T,Str] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD0, 'StopTime',JD0+0.5, 'CENTER','500@0'); 
            % Comparing with ecliptic coo. - verified
            [X0, V0, JD0] = elements2pos(Obj, 'JD',[],...
                                         'TimeScale',Args.TimeScale,...
                                         'CooSys','eq',...
                                         'RefFrame','bary',...
                                         'Tol',Args.Tol,...
                                         'INPOP',Args.INPOP);
            
            % must assume that all JD0 are the same
            if numel(unique(JD0))==1
                JD0 = JD0(1);
            else
                error('All epochs must be the same');
            end
            
            if ~issorted(Time) && ~issorted(Time, 'descend')
                error('Requested Time must be sorted');
            end
            if ~(min(Time)>=JD0 || max(Time)<=JD0)
                error('Requested times mus be below or above JD0 %10.2f',JD0);
            end
            
            % Sort Time to increase from JD0
            [~,SortedInd] = sort(abs(Time - JD0));
            Time          = Time(SortedInd);
                
           
            % Integration is done in the barycentric equatorial J2000 system
            
            % Integrate all bodies from JD0 to Time
            
            
            TimeStart = JD0;
            AllU      = zeros(3, Nt);
            AllU_B    = zeros(3, Nt);
            AllE_H    = zeros(3, Nt);
            AllE_dotH = zeros(3, Nt);
            for It=1:1:Nt
                
                LightTime = 0;
                [U_B,Uv_B] = celestial.SolarSys.orbitIntegration([TimeStart, Time(It)],...
                                                             X0,...
                                                             V0,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt,...
                                                             'TimeScale',Args.TimeScale,...
                                                             'INPOP',Args.INPOP);
                X0 = U_B;
                V0 = Uv_B;
                TimeStart = Time(It);

                % Observer position
                [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time(It), 'CooSys','b',...
                                                                            'RefFrame','eq',...
                                                                            'INPOP',Args.INPOP,...
                                                                            'ObserverEphem',Args.ObserverEphem,...
                                                                            'SunLightTime',LightTime(:),...
                                                                            'RefEllipsoid',Args.RefEllipsoid);
                % Topocentric position
                U = U_B - E_H;  % U_B(t-tau)
                % Topocentric distance
                Delta = sqrt(sum(U.^2, 1));
                if Args.MaxIterLT>1
                    
                    LightTime = Delta./Caud;   % scalar (single object)                

                    % second iteration for light time correction
                    [U_B,~] = celestial.SolarSys.orbitIntegration([Time(It), Time(It)-LightTime],...
                                                                             U_B,...
                                                                             Uv_B,...
                                                                             'RelTol',Args.TolInt,...
                                                                             'AbsTol',Args.TolInt,...
                                                                             'TimeScale',Args.TimeScale,...
                                                                             'INPOP',Args.INPOP);

                    [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time(It), 'CooSys','b',...
                                                                        'RefFrame','eq',...
                                                                        'INPOP',Args.INPOP,...
                                                                        'ObserverEphem',Args.ObserverEphem,...
                                                                        'SunLightTime',LightTime(:),...
                                                                        'RefEllipsoid',Args.RefEllipsoid);
                end
                

                AllU(:,It)      = U_B - E_H;  % U_B(t-tau)
                AllU_B(:,It)    = U_B;
                AllE_dotH(:,It) = E_dotH;
                AllE_H(:,It)    = E_H;
            end
            
            [Result, ColNames, ColUnits] = prepEphemOutput(Obj, Time, AllU, AllU_B, AllE_H, AllE_dotH,...
                                                           'OutType',Args.OutType,...
                                                           'Aberration',Args.Aberration,...
                                                           'OutUnitsDeg',Args.OutUnitsDeg,...
                                                           'IncludeMag',Args.IncludeMag,...
                                                           'IncludeAngles',Args.IncludeAngles,...
                                                           'IncludeDesignation',Args.IncludeDesignation);
                                                       
    
        end
        
        % done
        function [Result, ColNames, ColUnits] = ephemIntegrateMultiTime(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object by integrating the equation of motion 
            %   taking into account all the major bodies in the Solar
            %   System. Optimized for a single object and multiple times
            %   all larger or smaller relative to the Epoch.
            %
            %   For each orbital-element or time, return the Geocentric or
            %   topocentric ephemerides of the target.
            %
            %   For definitions and formulae, see Explanatory Supplement to the Astronomical
            %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
            % 
            %
            % Input  : - A single element OrbitalEl object.
            %            This object may include multiple orbital elements
            %            in vectors of parameters.
            %          - A vector of JD in the TDT time scale.
            %            Only for a single object (see
            %            ephemIntegrateMultiObj for multiple objects
            %            integration).
            %          * ...,key,val,...
            %            'TimeScale' - Time scale of integration:
            %                   'TDB'|'TT'
            %                   Default is 'TDB'.
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
            %
            %            'IncludeMag' - A logical indicating if to include
            %                   magnitude in output catalog.
            %                   Default is true.
            %            'IncludeAngles' - A logical indicating if to include
            %                   angles in output catalog.
            %                   Default is true.
            %            'IncludeDesignation' - A logical indicating if to
            %                   include asteroid designation.
            %                   Default is true.
            %            'INPOP' - A celestial.INPOP object that will be
            %                   used for the Earth position and perturbarions.
            %                   If empty, then the object will be loaded.
            %                   Default is [].
            %
            %            'Tol' - Tolerance [rad] for solving the Kepler
            %                   equation. Default is 1e-8.
            %            'TolLT' - Tolerance [day] for the light-time
            %                   correction iterations. Default is 1e-6.
            %            'TolInt' - Integration tolerance.
            %                   Default is 1e-8.
            %            'OutUnitsDeg' - A logical indicating if to list
            %                   the RA and Dec in degrees. If false list in
            %                   radians. Default is true.
            %
            %            'Aberration' - A logical indicating if to include
            %                   aberration of light. Default is false.
            %                   Note that for the default (false) the
            %                   output is in an "astrometric" reference
            %                   frame (i.e., relative to the stars).
            %            'EarthEphem' - Earth ephemeris to use:
            %                   'vsop87' - VSOP87
            %                   'inpop' - INPOP (default).
            %            'MaxIterLT' - Maximum numbre of iterations for
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
            %                   (e.g., for quick calculation).
            %
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
            %                   If empty, the function will use EarthEphem and GeoPos.
            %                   In case of size [Nepoch,3], the function assume zero velocity.
            %                   Defauls is [].
            % Output : - Ephemeris (AstroCatalog, table or , matrix).
            %          - A cell array of column names in the ephemeris
            %            matrix.
            %          - A cell array of column units.
            % Author : Eran Ofek (Nov 2023)
            % Example: 
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem([],[9804]);
            %          JD=ceil(OrbEl.Epoch)+0.5;
            %          CatInt = ephemIntegrateMultiTime(OrbEl, JD+(-5000:500:5000).');
                   
            arguments
                Obj(1,1)
                Time
                
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';
                Args.IncludeMag logical          = true;  % use false to speed up
                Args.IncludeAngles logical       = true;
                Args.IncludeDesignation logical  = true;  % works only for AstroCatalog output
                Args.INPOP                       = [];
                
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.TolInt                      = 1e-8; 
                
                Args.OutUnitsDeg(1,1) logical    = true;
                
                Args.Aberration(1,1) logical     = false;
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
                Args.EarthEphem                  = 'INPOP';  % 'vsop87' | 'inpop'

                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                
            end
            
            JD0 = Obj.Epoch;
            
            % do the integration with increasing time
            Time = sort(Time(:));
            IndTimePos = find((Time - JD0)>=0);
            IndTimeNeg = find((Time - JD0)<0);
            TimePos    = Time(IndTimePos);
            TimeNeg    = flipud(Time(IndTimeNeg));

            ArgsCell = namedargs2cell(Args);
            
            [ResultPos,ColNames,ColUnits] = ephemIntegrateMultiTime1dir(Obj, TimePos, ArgsCell{:});
            [ResultNeg]                   = ephemIntegrateMultiTime1dir(Obj, TimeNeg, ArgsCell{:});
            switch lower(Args.OutType)
                case 'astrocatalog'
                    Result = ResultNeg;
                    Result.Catalog = [Result.Catalog; ResultPos.Catalog];
                otherwise
                    Result = [ResultNeg; ResultPos];
                    % Sort by time
            end
            Result = sortrows(Result, 1);
                
        end
        
        % done
        function [Result, ColNames, ColUnits] = ephemIntegrateMultiObj(Obj, Time, Args)
            % Calculate ephemerides for OrbitalEl object by integrating the equation of motion 
            %   taking into account all the major bodies in the Solar
            %   System.
            %   Optimized for a single epoch and multiple objects.
            %
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
            %            'TimeScale' - Time scale of integration:
            %                   'TDB'|'TT'
            %                   Default is 'TDB'.
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
            %
            %            'IncludeMag' - A logical indicating if to include
            %                   magnitude in output catalog.
            %                   Default is true.
            %            'IncludeAngles' - A logical indicating if to include
            %                   angles in output catalog.
            %                   Default is true.
            %            'IncludeDesignation' - A logical indicating if to
            %                   include asteroid designation.
            %                   Default is true.
            %            'INPOP' - A celestial.INPOP object that will be
            %                   used for the Earth position and perturbarions.
            %                   If empty, then the object will be loaded.
            %                   Default is [].
            %
            %            'Tol' - Tolerance [rad] for solving the Kepler
            %                   equation. Default is 1e-8.
            %            'TolLT' - Tolerance [day] for the light-time
            %                   correction iterations. Default is 1e-6.
            %            'TolInt' - Integration tolerance.
            %                   Default is 1e-8.
            %            'OutUnitsDeg' - A logical indicating if to list
            %                   the RA and Dec in degrees. If false list in
            %                   radians. Default is true.
            %
            %            'Aberration' - A logical indicating if to include
            %                   aberration of light. Default is false.
            %                   Note that for the default (false) the
            %                   output is in an "astrometric" reference
            %                   frame (i.e., relative to the stars).
            %            'EarthEphem' - Earth ephemeris to use:
            %                   'vsop87' - VSOP87
            %                   'inpop' - INPOP (default).
            %            'MaxIterLT' - Maximum numbre of iterations for
            %                   light-time corrections. Default is 2.
            %                   1 will force to no ligh-time correction
            %                   (e.g., for quick calculation).
            %
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
            %                   If empty, the function will use EarthEphem and GeoPos.
            %                   In case of size [Nepoch,3], the function assume zero velocity.
            %                   Defauls is [].
            % Output : - Ephemeris (AstroCatalog, table or , matrix).
            %          - A cell array of column names in the ephemeris
            %            matrix.
            %          - A cell array of column units.
            % Author : Eran Ofek (Nov 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem([],[9804;9807]);
            %          JD = celestial.time.julday([9 9 2021])
            %          Cat = ephemIntegrateMultiObj(OrbEl, JD)
            %
           
            arguments
                Obj(1,1)
                Time
                
                Args.TimeScale                   = 'TDB';
                Args.GeoPos                      = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid                = 'WGS84';
                Args.OutType                     = 'AstroCatalog';
                Args.IncludeMag logical          = true;  % use false to speed up
                Args.IncludeAngles logical       = true;
                Args.IncludeDesignation logical  = true;  % works only for AstroCatalog output
                Args.INPOP                       = [];
                
                Args.Tol                         = 1e-8;   % [rad]
                Args.TolLT                       = 1e-6;   % [day]
                Args.TolInt                      = 1e-8; 
                
                Args.OutUnitsDeg(1,1) logical    = true;
                
                Args.Aberration(1,1) logical     = false;
                Args.ObserverEphem               = []; % Heliocentric coordinate of observer - [x,y,z,vx,vy,vz]
                Args.EarthEphem                  = 'INPOP';  % 'vsop87' | 'inpop'

                Args.MaxIterLT                   = 2;  % use 0 for quick and dirty
                
                
            end
            RAD  = 180./pi;
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            Nt      = numel(Time);
            Ntarget = numEl(Obj);
            if ~(Nt==1 || Ntarget==1)
                error('Number of times or number of targets must be 1');
            end
                        
            % populate INPOP if needed
            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateTables('all');
                Args.INPOP.populateTables('all','FileData','vel');
            end
            
            % Get initial X, Y, Z, VX, VY, VZ of target
            % Heliocentric system
            %[Nu0]  = keplerSolve(Obj, Obj.Epoch, 'Tol',Args.Tol);
            %[V0,X0] = trueAnom2rectVel(Obj,Nu0,[],[]);  % ecliptic J2000
            
            
            % comparing to horizons
            % [T,Str] = celestial.SolarSys.getJPL_ephem('9804;','EPHEM_TYPE','VECTORS','TimeScale','TT', 'StartTime',JD0, 'StopTime',JD0+0.5, 'CENTER','500@0'); 
            % Comparing with ecliptic coo. - verified
            [X0, V0, JD0] = elements2pos(Obj, 'JD',[],...
                                         'TimeScale',Args.TimeScale,...
                                         'CooSys','eq',...
                                         'RefFrame','bary',...
                                         'Tol',Args.Tol,...
                                         'INPOP',Args.INPOP);
            
            % must assume that all JD0 are the same
            if numel(unique(JD0))==1
                JD0 = JD0(1);
            else
                error('All epochs must be the same');
            end
            
            
           
            % Integration is done in the barycentric equatorial J2000 system
            
            % Integrate all bodies from JD0 to Time
            
            LightTime = 0;
            [U_B,Uv_B] = celestial.SolarSys.orbitIntegration([JD0, Time-LightTime],...
                                                         X0,...
                                                         V0,...
                                                         'RelTol',Args.TolInt,...
                                                         'AbsTol',Args.TolInt,...
                                                         'TimeScale',Args.TimeScale,...
                                                         'INPOP',Args.INPOP);
            
            % Observer position
            [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time(:), 'CooSys','b',...
                                                                        'RefFrame','eq',...
                                                                        'INPOP',Args.INPOP,...
                                                                        'ObserverEphem',Args.ObserverEphem,...
                                                                        'SunLightTime',LightTime(:),...
                                                                        'RefEllipsoid',Args.RefEllipsoid);
            % Topocentric position
            U = U_B - E_H;  % U_B(t-tau)
            % Topocentric distance
            Delta = sqrt(sum(U.^2, 1));
            LightTime = Delta./Caud;
            
            Nlt = numel(LightTime);

            if Args.MaxIterLT>1
                % second iteration for light time correction
                for Ilt=1:1:Nlt
                    [U_B(:,Ilt),~] = celestial.SolarSys.orbitIntegration([Time, Time-LightTime(Ilt)],...
                                                                             U_B(:,Ilt),...
                                                                             Uv_B(:,Ilt),...
                                                                             'RelTol',Args.TolInt,...
                                                                             'AbsTol',Args.TolInt,...
                                                                             'TimeScale',Args.TimeScale,...
                                                                             'INPOP',Args.INPOP);
                end
                [E_H, E_dotH]=celestial.SolarSys.earthObserverPos(Time, 'CooSys','b',...
                                                                        'RefFrame','eq',...
                                                                        'INPOP',Args.INPOP,...
                                                                        'ObserverEphem',Args.ObserverEphem,...
                                                                        'SunLightTime',LightTime(:),...
                                                                        'RefEllipsoid',Args.RefEllipsoid);

            end
            
            U = U_B - E_H;  % U_B(t-tau)

            %            
            [Result, ColNames, ColUnits] = prepEphemOutput(Obj, Time, U, U_B, E_H, E_dotH,...
                                                           'OutType',Args.OutType,...
                                                           'Aberration',Args.Aberration,...
                                                           'OutUnitsDeg',Args.OutUnitsDeg,...
                                                           'IncludeMag',Args.IncludeMag,...
                                                           'IncludeAngles',Args.IncludeAngles,...
                                                           'IncludeDesignation',Args.IncludeDesignation);
            
            
            
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
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty then will be generated.
            %                   Default is [].
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
                Args.INPOP               = [];
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
                Cat    = ephem(ObjNew(Iobj), JD, 'GeoPos',[], 'MaxIterLT',1,...
                                                 'IncludeMag',IncludeMag, 'OutUnitsDeg',false,...
                                                 'OutType','mat', 'AddDesignation',false,...
                                                 'INPOP',Args.INPOP);
                
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
                                                  'INPOP',Args.INPOP);
                                              
                                              %    'Integration',Args.Integration);


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
            % populate missing parameters
            Result.populate;
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
            % Output : [JD, JD-Epoch, DeltaRA("), DeltaDec(")]
            % Example: VecJD = ((2460110.5 - 2000):10:(2460110.5 +500))';
            %          R1=celestial.OrbitalEl.compareEphem2JPL('StartJD',VecJD(1),'EndJD',VecJD(end));
            %          plot(VecJD,R1(:,1)); hold on; plot(VecJD,R2(:,1))
            
            arguments
                Args.ObjectInd           = 9804;
                Args.StartJD             = 2460110.5 - 1000;
                Args.StepSize            = 50;
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
            %CatE   = ephem(OrbEl1, VecJD, 'GeoPos',Args.GeodPos, 'OutUnitsDeg',false, 'Integration',Args.Integration);
            CatE   = ephemKeplerMultiTime(OrbEl1, VecJD+69./86400, 'GeoPos',Args.GeodPos, 'OutUnitsDeg',false);
            %CatE1   = ephemIntegrate(OrbEl1, VecJD+69./86400, 'GeoPos',Args.GeodPos, 'OutUnitsDeg',false);
            
            CatJPL = celestial.SolarSys.jpl_horizons('ObjectInd',num2str(Args.ObjectInd),'StartJD',Args.StartJD,'StopJD',Args.EndJD,...
                                                     'StepSize',Args.StepSize, 'StepSizeUnits','d','CENTER','500', 'GeodCoo',GeodPosKM);
            % RA nd Dec diff between JPL and ephem:
            Result = [CatE.Catalog.JD, CatE.Catalog.JD - OrbEl1.Epoch, ...
                     [CatE.Catalog.RA - CatJPL.Catalog(:,2), CatE.Catalog.Dec - CatJPL.Catalog(:,3)].*3600];
                     
            
            
            
        end
        
    end


    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
