
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
            if Args.SubTp
                Time = Time - Obj.Tp;
            end
            Nu   = zeros(Nel, 1);
            R    = zeros(Nel, 1);
            E    = zeros(Nel, 1);
            Vel  = zeros(Nel, 1);
            M    = nan(Nel, 1);
            
            Flag = Obj.Eccen<1;
            [Nu(Flag),R(Flag),E(Flag),Vel(Flag),M(Flag)] = celestial.Kepler.kepler_elliptic(Time(Flag), Obj.PeriDist(Flag), Obj.Eccen(Flag), Obj.K, Args.Tol);
        
            Flag = Obj.Eccen==1;
            % E is S
            [Nu(Flag),R(Flag),E(Flag),Vel(Flag)] = celestial.Kepler.kepler_parabolic(Time(Flag), Obj.PeriDist(Flag), Obj.K);
            
            Flag = Obj.Eccen>1;
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
            [varargout{1:nargout}] = celestial.Kepler.trueanom2pos(R, Nu, Obj.Node./RAD, Obj.W./RAD, Obj.Incl./RAD);
        end

    end
    
    methods % ephemerides
        function ephem(Obj, Time, Args)
            %
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem([],9804);
            %          JD = celestial.time.julday([9 9 2021])
            %          ephem(OrbEl, JD)
            %
            arguments
                Obj(1,1)
                Time
                Args.Tol     = 1e-8;
            end
            
            [Nu, R, E, Vel, M]          = keplerSolve(Obj, Time, 'Tol',Args.Tol);
            % target ecliptic Heliocentric rect. position
            [Xtarget, Ytarget, Ztarget] = trueAnom2rectPos(Obj, Nu, R, 'rad');
            
            % verified
            %RAD = 180./pi;
            %atan2(Ytarget, Xtarget).*RAD
            %atan(Ztarget./sqrt(Xtarget.^2 + Ytarget.^2)).*RAD
            
            
            [Coo,Vel]=celestial.SolarSys.calc_vsop87(Time, 'Earth', 'e', 'E');
            Xobs    = Coo(1,:).';
            Yobs    = Coo(2,:).';
            Zobs    = Coo(3,:).';
            XobsDot = Vel(1,:).';
            YobsDot = Vel(2,:).';
            ZobsDot = Vel(3,:).';
            
            X = Xtarget - Xobs;
            Y = Ytarget - Yobs;
            Z = Ztarget - Zobs;
            
            Lon = atan2(Y, X);
            Lat = atan(Z./sqrt(X.^2 + Y.^2));
            RotMat = celestial.coo.rotm_coo('E');
            Eq = RotMat * [X(:).'; Y(:).'; Z(:).'];
            Xeq = Eq(1,:).';
            Yeq = Eq(2,:).';
            Zeq = Eq(3,:).';
            
            R     = sqrt(Xtarget.^2 + Ytarget.^2 + Ztarget.^2);
            Delta = sqrt(X.^2 + Y.^2 + Z.^2);
            
            RA  = atan2(Yeq, Xeq);
            Dec = atan(Zeq./sqrt(Xeq.^2 + Yeq.^2));
            
            [RA, Dec].*180./pi
            R
            Delta
            
            
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
