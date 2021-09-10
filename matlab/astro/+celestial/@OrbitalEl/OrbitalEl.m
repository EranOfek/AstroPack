
classdef OrbitalEl < handle
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
                Args.MagType      = 'HG';
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
            
            Result = 2.*pi .* (Obj.A).^1.5 ./ Obj.K;  % days
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
            
            V = (sqrt(2).*2.*pi.*Obj.A ./ period(Obj,TimeUnits)).*sqrt( 1./R - 1./(2.*Obj.A) );
        end

        function [Nu, R, E, Vel, M] = keplerSolve(Obj, Time, Args)
            % Solve the Kepler equation for OrbitalEl object.
            %   For elliptic, parabolic, and hyperbolic orbits
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
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem;
            %          [Nu, R, E, Vel, M] = keplerSolve(OrbEl(1), 2451545)
            
            arguments
                Obj(1,1)
                Time
                Args.Tol      = 1e-8;
                Args.K        = [];  % use Obj default
                Args.SubTp(1,1) logical = true;
            end
            
            if ~isempty(Args.K)
                Obj.K = Args.K;
            end
            
            Nel  = numEl(Obj);
            Time = Time(:).*ones(Nel,1);
            Ntime = numel(Time);
            if Args.SubTp
                Time = Time - Obj.Tp;
            end
            Nu   = zeros(Nel, 1);
            R    = zeros(Nel, 1);
            E    = zeros(Nel, 1);
            Vel  = zeros(Nel, 1);
            M    = nan(Nel, 1);
            
            Flag = Obj.Eccen<1;
            Flag = Flag & true(Ntime,1);
            [Nu(Flag),R(Flag),E(Flag),Vel(Flag),M(Flag)] = celestial.Kepler.kepler_elliptic(Time(Flag), Obj.PeriDist(Flag), Obj.Eccen(Flag), Obj.K, Args.Tol);
        
            Flag = Obj.Eccen==1;
            Flag = Flag & true(Ntime,1);
            % E is S
            [Nu(Flag),R(Flag),E(Flag),Vel(Flag)] = celestial.Kepler.kepler_parabolic(Time(Flag), Obj.PeriDist(Flag), Obj.K);
            
            Flag = Obj.Eccen>1;
            Flag = Flag & true(Ntime,1);
            % E is H
            [Nu(Flag),R(Flag),E(Flag),Vel(Flag)] = celestial.Kepler.kepler_hyperbolic(Time(Flag), Obj.PeriDist(Flag), Obj.Eccen(Flag), Obj.K, Args.Tol);
            
            
        end
            
        function varargout=trueAnom2rectPos(Obj, Nu, R, AngUnits)
            % True anomaly and radius vector to rectangular position
            % Description: True anomaly to rectangular position
            % Input  : - OrbitalEl object.
            %          - True anomaly [rad].
            %          - Optional radius vector. If not given will be
            %            calculated from the True anaomaly.
            %          - Angilar units. Default is 'rad'.
            % Output : * Either a single 3 column matrix of [X,Y,Z] or 
            %            X,Y,Z. Units the same as the radius vector units.
            % Example: BodyPos = trueAnom2rectPos(OrbEl(1), 1, 1)
            %          [x,y,z] = trueAnom2rectPos(OrbEl(1), 1, 1)
            
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

    end
    
    methods % ephemerides
        function Cat = ephem(Obj, Time, Args)
            % Calculate epjemerides for OrbitalEl object.
            %   For definitions and formulae, see Explanatory Supplement to the Astronomical
            %   Alamanac (Seidelmann 2006), chapter 3.313, p. 148.
            % Input  : -
            % Output : -
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
            %          JD = celestial.time.julday([9 9 2021])
            %          ephem(OrbEl, JD)
            %
            % JD = celestial.time.julday([1 9 2021]);
            % Coo=[-116.865./RAD 33.3563./RAD 2000]
            % OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
            % CatE = ephem(OrbEl, JD, 'GeoPos',Coo)
            
            % [Cat]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',JD,'StopJD',JD+1,'StepSizeUnits','h','CENTER','675')
            
            arguments
                Obj(1,1)
                Time
                Args.Tol     = 1e-8;
                Args.TolLT   = 1e-6;
                Args.Abberation(1,1) logical = false;
                Args.GeoPos  = [];  % [] - topocentric  ; [rad, rad, m]
                Args.RefEllipsoid   = 'WGS84';
            end
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            Nt      = numel(Time);
            Ntarget = numEl(Obj);
            if ~(Nt==1 || Ntarget==1)
                error('Number of epochs or number of targets must be 1');
            end
            Ncat = max(Nt, Ntarget);
            
            ColNames      = {'Designation','Number','JD', 'RA', 'Dec', 'R', 'Delta'};
            ColUnits      = {'','','day','rad','rad', 'au','au'};
            Col           = cell2struct(num2cell(1:1:numel(ColNames)),ColNames,2);
            CatS          = struct('Designation',cell(Ncat,1),...
                                   'Number',nan(Ncat,1),...
                                   'JD',nan(Ncat,1),...
                                   'RA',nan(Ncat,1),...
                                   'Dec',nan(Ncat,1),...
                                   'R',nan(Ncat,1),...
                                   'Delta',nan(Ncat,1));
                               
            for It=1:1:Nt  <-- remove this loop

                LightTimeNotConverged = true;
                LightTime             = 0;
                Iter                  = 0;
                while LightTimeNotConverged
                    Iter = Iter + 1;
                    [Nu, R, E, Vel, M]          = keplerSolve(Obj, Time(It)-LightTime, 'Tol',Args.Tol);
                    % target ecliptic Heliocentric rect. position
                    [Xtarget, Ytarget, Ztarget] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                    [U_B] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                    U_B   = U_B.';  % a 3 X N matrix

                    % verified
                    %RAD = 180./pi;
                    %atan2(Ytarget, Xtarget).*RAD
                    %atan(Ztarget./sqrt(Xtarget.^2 + Ytarget.^2)).*RAD

                    % rectangular ecliptic coordinates of Earth with equinox of J2000
                    [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time(It), 'Earth', 'a', 'd');

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

                    if all(abs(LightTime - PrevLightTime))<Args.TolLT
                        LightTimeNotConverged = false;
                    end
                end
                R     = sqrt(sum(U_B.^2, 1));

                % ignore light deflection
                if Args.Abberation
                    % Abberation of light
                    % Vel should be in the Barycentric system, but here we
                    % approximate it in the Heliocentric system
                    P       = U./Delta;
                    V       = E_dotH./Caud;
                    AbsV    = sqrt(sum(V.^2, 1));
                    InvBeta = sqrt(1- AbsV.^2);
                    F1      = dot(P, V);
                    F2      = 1 + F1./(1 + InvBeta);

                    % The abberated position of the body in the geocentric inertial
                    % frame that is moving with velocioty V of the Earth relative
                    % to the natural frame:

                    U2 = (InvBeta.*U + F2.*Delta.*V)./(1 + F1);
                else
                    U2 = U;
                end

                % Rotate from Ecliptic to Equatorial reference frame
                RotMat = celestial.coo.rotm_coo('E');
                Equatorial_U2 = RotMat * U2;

                RA  = atan2(Equatorial_U2(2,:), Equatorial_U2(1,:));
                Dec = atan(Equatorial_U2(3,:)./sqrt( Equatorial_U2(1,:).^2 + Equatorial_U2(2,:).^2  ));
                
                  
            end
            %celestial.coo.convertdms(RA,'r','SH')
            %celestial.coo.convertdms(Dec,'R','SD')
            % geocentric  05 39 59.38 +11 02 53.3
            % topocentric 05 39 59.53 +11 02 51.9
            CatE = [RA(:), Dec(:)];
            
            %Cat = AstroCatalog({Cat}, 'ColNames', ColNames, 'ColUnits', ColUnits);
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
    
    methods (Static)   % upload orbital elenments
        function Result = loadSolarSystem(Type, Desig)
            % Load the JPL Solar System orbital elements from local disk
            %   To install the orbotal elements use the Installer class.
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
                            Result(Itype).Tp          = T.Tp(Flag);
                            Result(Itype).Ref         = T.Ref(Flag);
                        end
                    otherwise
                        error('Unknown Type option');
                end
            end
                
        end
    end  


    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
