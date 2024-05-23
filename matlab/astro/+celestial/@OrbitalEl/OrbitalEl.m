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
            %            Default is true.
            % Output : - An OrbitalEl object with the selected orbits.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem('num');
            %          Res   = selectFlag(OrbEl, 1, true);
            
            arguments
                Obj(1,1)
                Flag
                CreateNewObj logical   = true;
            end
            
            if CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Ne    = numEl(Result);
            Prop  = fieldnames(Result);
            Nprop = numel(Prop);
            for Iprop=1:1:Nprop
                Ndata = size(Result.(Prop{Iprop}), 1);
                %if numel(Flag)==Ndata
                if Ndata>1 || Ne==1
                    %if iscell(Result.(Prop{Iprop}))
                        Result.(Prop{Iprop}) = Result.(Prop{Iprop})(Flag,:);
                    %else
                    %    Result.(Prop{Iprop}) = Result.(Prop{Iprop})(Flag,:);
                    %end
                end
            end
            
        end


        function Result = insertElements(Obj, NewObj, Ind, IndNew, Args)
            % Insert orbital elemnts from one object into a second object.
            %   Can concat orbital elements, or replace orbital elements in
            %   specific indices with orbital elemenst from another object
            %   with some specific indices.
            %   Property that are scalar for all the elements are not
            %   treated.
            %   If property in second object is empty, then it is not
            %   copied.
            % Input  : - The first celestial.OrbitalEl object
            %          - The secod celestial.OrbitalEl object
            %          - Indices, or logical flags, in the first object.
            %            If empty, then the orbital elemenst in the second object
            %            will be concat to the orbital elements in the first object.
            %            Default is [].
            %          - Indices, or logical flags, in the second object.
            %            If empty use all []. Default is [].
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object. Default is true.
            % Example: OrbEl1 = celestial.OrbitalEl.loadSolarSystem('num',[9801:9810]);
            %          OrbEl2 = celestial.OrbitalEl.loadSolarSystem('num',[9811:9820]);
            %          R1 = insertElements(OrbEl1, OrbEl2, []); % add 
            %          R2 = insertElements(OrbEl1, OrbEl2, [1:10]); % insert 
            %          R3 = insertElements(OrbEl1, OrbEl2, [1:5],[6:10]); % insert 6:10 in OrbEl2 into positions 1:5 in OrbEl1

            arguments
                Obj(1,1)
                NewObj(1,1)
                Ind                        = [];
                IndNew                     = [];
                Args.CreateNewObj logical  = true;
                Args.SkipProp              = {'K'};
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            if isempty(IndNew)
                Nnew = numEl(NewObj);
                IndNew = (1:1:Nnew);
            end

            FN  = fieldnames(Obj);
            Nfn = numel(FN);
            for Ifn=1:1:Nfn
                if ~strcmp(FN{Ifn}, Args.SkipProp)
                    if isempty(Ind)
                        % concat on end
                        if isempty(Obj.(FN{Ifn})) || size(Obj.(FN{Ifn}), 1)==1 || isempty(NewObj.(FN{Ifn}))
                            % skip
                        else
                            Result.(FN{Ifn}) = [Obj.(FN{Ifn}); NewObj.(FN{Ifn})(IndNew,:)];
                        end
                    else
                        % insert in specific positions
                        %if isempty(Obj.(FN{Ifn})) || size(Obj.(FN{Ifn}), 1)==1 || isempty(NewObj.(FN{Ifn}))
                        if isempty(Obj.(FN{Ifn})) || isempty(NewObj.(FN{Ifn}))
                            % skip
                        else
    %                         if size(NewObj.(FN{Ifn}), 1)==1
    %                             Result.(FN{Ifn})(Ind,:) = NewObj.(FN{Ifn});
    %                         else
    %                             Result.(FN{Ifn})(Ind,:) = NewObj.(FN{Ifn})(IndNew,:);
                            if (numel( NewObj.(FN{Ifn}) )==1 &&  numel(IndNew)>1)
                                Result.(FN{Ifn})(Ind,:) = NewObj.(FN{Ifn})(1,:);
                            else
                                if ischar(NewObj.(FN{Ifn}))
                                    Result.(FN{Ifn}) = NewObj.(FN{Ifn});
                                else
                                    Result.(FN{Ifn})(Ind,:) = NewObj.(FN{Ifn})(IndNew,:);
                                end
                            end
                        end
                    end
                end
            end


        end

        
        function Result = merge(Obj, Args)
            % Merge the orbital-elements in several elements of the OrbitalEl object.
            %   This function is custom made for merging the JPL
            %   epehmerides, and may fail in other cases.
            % Input  : - An OrbitalEl object, with multiple elements.
            %          * ...,key,val,...
            %            'MinEpoch' - Select bodies with Epoch above this
            %                   one. Default is -Inf.
            %            'MaxEccen' - Select bodies with Eccen below this
            %                   one. Default is Inf.
            %            'MaxH' - Select bodies with abs. mah (H) below
            %                   this one. Default is Inf.
            % Output : - A merged OrbitalEl objt with a single element.
            %            This is always a new copy.
            % Example: OrbEl = celestial.OrbitalEl.loadSolarSystem;
            %          O = merge(OrbEl);
            %          E=merge(OrbEl,'MinEpoch',celestial.time.julday([1 1 2015]),'MaxEccen',0.99,'MaxH',26)
            
            arguments
                Obj
                Args.MinEpoch = -Inf;
                Args.MaxEccen = Inf;
                Args.MaxH     = Inf;
            end
            
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
            
            % clean file
            Flag = Result.Epoch>Args.MinEpoch & Result.Eccen<Args.MaxEccen & Result.MagPar(:,1)<Args.MaxH;
            if ~all(Flag)
                Result = selectFlag(Result, Flag, false);
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
            
            NN = max(numel(R), numel(Nu));
            X = [R.*cos(Nu); R.*sin(Nu); zeros(1,NN)];
            V = sqrt(Mu .* Obj.A(:).')./R .* [-sin(E); sqrt(1-Obj.Eccen(:).'.^2).*cos(E); zeros(1,NN)];
            
            
        end
            
        function [X0, V0, JD, S_B, S_Bdot] = elements2pos(Obj, Args)
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
            %          - Sun barycentric position (available only for
            %            RefFrame='bary').
            %          - Sun barycentric velocity (available only for
            %            RefFrame='bary').
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
                    S_B    = [];
                    S_Bdot = [];
                case 'bary'
                    % get sun position
                    if isempty(Args.INPOP)
                        Args.INPOP = celestial.INPOP;
                        Args.INPOP.populateTables('Sun');
                        Args.INPOP.populateTables('Sun','FileData','vel');
                    end
                    
                    S_B  = Args.INPOP.getPos('Sun',Args.JD, 'TimeScale',Args.TimeScale, 'IsEclipticOut',IsEcOut, 'OutUnits','au');
                    S_Bdot = Args.INPOP.getVel('Sun',Args.JD, 'TimeScale',Args.TimeScale, 'IsEclipticOut',IsEcOut, 'OutUnits','au');
                    
                    X0   = X0 + S_B;
                    V0   = V0 + S_Bdot;
                otherwise
                    error('Unknown RefFrame option');
            end
                            
            JD    = Args.JD;
            INPOP = Args.INPOP;
        end
    end
    
    methods % ephemerides / utils
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
            %            barycentric or heliocentric position.
            %            This is used only for the calculation of the
            %            target radius vector.
            %          - Like the U matrix, but for E_H - the topocentric
            %            heliocentric poistion. Default is [].
            %          - Like the U matrix, but for E_dotH - the
            %            topocentrix heliocentric/barycentric velocity
            %            [AU/day]. This is used for abberation of light.
            %            If empty, then ignore.
            %            Default is [].
            %          * ...,key,val,...
            %            'S_B' - Column vector of Sun barycentric position.
            %                   If empty, then assume [0 0 0]'.
            %                   Default is [].
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
                
                Args.S_B                   = []; % if provided then use it to calculate heliocentric R instead of barycentric R

                Args.OutType               = 'AstroCatalog'; % 'mat'|'astrocatalog'|'table'
                                
                Args.Aberration logical    = false;
                Args.OutUnitsDeg logical   = true;
                Args.IncludeMag logical    = true;
                Args.IncludeAngles logical = true;
                Args.IncludeDesignation logical = true;

                %Args.TreatInaccurateDist logical  = true;
            end
            
            if Args.OutUnitsDeg
                AngUnits = 'deg';
            else
                AngUnits = 'rad';
            end
            
            % Topocentric distance
            Delta = sqrt(sum(AllU.^2, 1));

            if isempty(Args.S_B)
                R     = sqrt(sum(AllU_B.^2, 1));
            else
                R     = sqrt(sum((AllU_B-Args.S_B).^2, 1));
            end

            
            % U2 is already in equatorial caretesian coordinates
            [RA, Dec, Delta] = celestial.SolarSys.cart2eqAng(AllU, 'InputSys','eq', 'Delta',Delta, 'Aberration',Args.Aberration, 'E_dotH',AllE_dotH, 'OutUnitsDeg',Args.OutUnitsDeg);

            ColNames = {'JD','RA','Dec','R','Delta'};
            ColUnits = {'day',AngUnits,AngUnits,'au','au'};
            Nra      = numel(RA);
            Cat = [Time(:).*ones(Nra,1), RA(:), Dec(:), R(:), Delta(:)]; % Ang_SOT(:), Ang_STO(:), Mag(:)];
                
            % calculate angles
            if Args.IncludeAngles
                R_obs_sun = sqrt(sum(AllE_H.^2, 1));  % Sun-Earth distance
                % if Args.TreatInaccurateDist
                %     Flag = (R_obs_sun+Delta)<R;
                %     R_obs_sun

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
                if iscell(Designation) || isstring(Designation)
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
                
        function [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(Obj, JD, Args)
            % Target barycentric position.
            %   Calculate the target barycentric position in rectangular
            %   J2000.0 equatorial system.
            %   The calculation can be done by solving the Kepler equation,
            %   or by direct integration of the equation of motion, under
            %   perturbations from all the planets.
            %   The code can work on either a single time and multiple
            %   objects, or a single object and multiple times.
            %   If using the Kepler equation, then the code can run on
            %   different time for each object.
            %   If direct integation is used it is recomnded that the times
            %   will be sorted with increasing distance from the epoch.
            % Input  : - A single element celestialOrbitalEl object.
            %          - JD at which to calculate the position.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of the output
            %                   coordinates
            %               'ec' - J2000.0 ecliptic.
            %               'eq' - J2000.0 equatorial.
            %               Default is 'eq'.
            %            'CooSys0' - Coordinate system of input X0 and V0
            %               rectangular coordinates.
            %               'ec' - J2000.0 ecliptic.
            %               'eq' - J2000.0 equatorial.
            %               Default is 'eq'.
            %            'JD0' - An optional epoch for the input orbital
            %               elements, or X0, V0 (initial position).
            %               If empty, then will be taken from the
            %               celestial.OrbitalEl object (Epoch field).
            %               Default is [].
            %            'X0' - An optional 3 X N matrix of initial
            %               positions [au]. If given, then will be used as
            %               a starting position for the direct integration,
            %               instead of the orbital elements.
            %               Default is [].
            %            'V0' - Like X0, but for the velocity [au/day]
            %            'Integration' - A logical indicating if to use
            %               direct integration (true), or Kepler equation
            %               (false). Default is false.
            %            'TimeScale' - 'TDB'|'TT'. Default is 'TDB'.
            %            'RefFrame' - 'helio' | 'bary' - Default is 'bary'.
            %            'INPOP' - An optional populated celestial.INPOP
            %               object (provided for speed). If empty, then
            %               will be generated.
            %               Default is [].
            %            'LightTime' - Light time correction [days].
            %               If this is a vector, then the elements
            %               corresponds to the different times or different
            %               objects. Default is 0.
            %            'SunLightTime' - Sun light time correction [days].
            %               Default is 0.
            %            'Tol' - Tolerance for Kepler equation. 
            %               Default is 1e-10.
            %            'TolInt' - Tolerance for integration.
            %               Default is 1e-10.
            % 
            % Output : - (U_B) Target barycentric position [au]
            %            Rectangular J2000.0 equatorial.
            %          - (U_Bdot) Target barycentric velocity [au/day].
            %          - (S_B) Sun barycentric position [au].
            %          - (S_Bdot) Sun barycentric velocity [au/day]
            % Author : Eran Ofek (Nov 2023)
            % Example: OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]');
            %          JD = celestial.time.julday([1 1 2023]);
            %          [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl, JD)
            %          [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl, JD, 'Integration',true)
            %
            %          OrbEl1=celestial.OrbitalEl.loadSolarSystem('num',9804);
            %          [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl1, JD+(0:1:100)');
            %          [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl1, JD+(0:1:100)','Integration',true)
            %
            %          % Kepler equation / different time for each object
            %          OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]');
            %          JD = celestial.time.julday([1 1 2023]);
            %          [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(OrbEl, JD+(1:100)')
            
            arguments
                Obj(1,1)
                JD
                Args.CooSys                = 'eq';
                Args.CooSys0               = 'eq';
                
                Args.JD0                   = [];
                Args.X0                    = [];
                Args.V0                    = [];
                Args.Integration logical   = false;
                Args.TimeScale             = 'TDB';
                Args.RefFrame              = 'bary';  % 'bary'|'helio'
                Args.INPOP                 = [];
                Args.LightTime             = 0;
                Args.SunLightTime          = 0;   % must be scalar
                Args.TolInt                = 1e-10;
                Args.Tol                   = 1e-8;
            end
            
            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateAll;
            end
            
            
            
            Nel  = Obj.numEl;
            Njd  = numel(JD);
            Nlt  = numel(Args.LightTime);
            if Nel>1 && Njd>1
                if Args.Integration
                    error('Njd>1 and Nel>1 is not supported');
                else
                    if Nel~=Njd
                        error('For Integration=false, either Njd=1, or Nel=1, or Nel=Njd');
                    end
                end
            end
                        
            if nargout>1
                GetVelocity = true;
            else
                GetVelocity = false;
            end
            
            S_B = [];
            if Args.Integration
                if strcmp(Args.RefFrame, 'helio')
                    error('Integration=true, currently works only with RefFrame=bary');
                end
                % Find target barycentric position using orbital integration
                % Integration is done in:
                % Barycentric system
                % Equatorial J2000 cartesian coordinates
                if isempty(Args.X0) && isempty(Args.V0) && isempty(Args.JD0)
                    % convert to equatorial
                    switch lower(Args.CooSys0)
                        case 'ec'
                            % X0/V0 in ecliptic - convert to equatorial J2000
                            RotM = celestial.coo.rotm_coo('E');
                            Args.X0 = RotM * Args.X0;
                            Args.V0 = RotM * Args.V0;
                        case 'eq'
                            % do nothing - already in equatorial
                        otherwise
                            error('Unknown CooSys0 option');
                    end

                    % get initial conditions from orbital elements
                    [Args.X0, Args.V0, Args.JD0] = elements2pos(Obj, 'JD',[],...
                                         'TimeScale',Args.TimeScale,...
                                         'CooSys','eq',...
                                         'RefFrame','bary',...
                                         'Tol',Args.Tol,...
                                         'INPOP',Args.INPOP);
                %else
                %    S_B    = [];
                %    S_Bdot = [];
                end
                Args.JD0 = unique(Args.JD0);
                if numel(Args.JD0)>1
                    error('For orbital integration all epochs must be the same');
                end
                
                if Njd==1
                    % Integrate all bodies simultanosly
                    if numel(Args.LightTime)>1
                        error('For Njd=1 LightTime must be scalar');
                    end
                    [U_B, U_Bdot] = celestial.SolarSys.orbitIntegration([Args.JD0, JD-Args.LightTime],...
                                                             Args.X0,...
                                                             Args.V0,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt,...
                                                             'TimeScale',Args.TimeScale,...
                                                             'INPOP',Args.INPOP);
                
                else
                    % integrate one body over multiple times
                    % assume all times are either larger or smaller than
                    % epoch and are ordered with increased distance from
                    % epoch
                    
                    JD1    = Args.JD0;
                    U_B    = zeros(3, Njd);
                    U_Bdot = zeros(3, Njd);
                    for Ijd=1:1:Njd
                        Ilt = min(Nlt, Ijd);
                        JD2 = JD(Ijd) - Args.LightTime(Ilt);
                        [U_B(:,Ijd), U_Bdot(:,Ijd)] = celestial.SolarSys.orbitIntegration([JD1, JD2],...
                                                             Args.X0,...
                                                             Args.V0,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt,...
                                                             'TimeScale',Args.TimeScale,...
                                                             'INPOP',Args.INPOP);
                        %
                        Args.X0 = U_B(:,Ijd);
                        Args.V0 = U_Bdot(:,Ijd);
                        JD1     = JD2;
                    end
                end
                
            else
                % Find target Heliocentric position via Kepler equation
                % Heliocentric system
                % Eclitpic J2000 cartesian coordinates
                [Nu, R, E]       = keplerSolve(Obj, JD-Args.LightTime, 'Tol',Args.Tol);
                % Target, Ecliptic Heliocentric rect. position
                if GetVelocity
                    [U_Hdot, U_H] = trueAnom2rectVel(Obj, Nu, R, E, 'rad');
                    %U_H    = U_H.';  % a 3 X N matrix
                    %U_Hdot = U_Hdot.';
                else
                    [U_H] = trueAnom2rectPos(Obj, Nu, R, 'rad');
                    U_H   = U_H.';  % a 3 X N matrix
                end
                % convert to Equatorial J2000, Heliocentric
                RotMatEc2Eq = celestial.coo.rotm_coo('E');
                U_H   = RotMatEc2Eq * U_H;
                if GetVelocity
                    U_Hdot = RotMatEc2Eq * U_Hdot;
                end
                
                % Earth/observer heliocentric position
                switch lower(Args.RefFrame)
                    case 'bary'
                        S_B    = Args.INPOP.getPos('Sun', JD - Args.SunLightTime, 'TimeScale',Args.TimeScale, 'IsEclipticOut',false);
                        S_Bdot = Args.INPOP.getVel('Sun', JD - Args.SunLightTime, 'TimeScale',Args.TimeScale, 'IsEclipticOut',false);
                        
                        U_B     = S_B + U_H;
                        
                        U_Bdot  = S_Bdot + U_Hdot;
                    case 'helio'
                        % already heliocentric
                        U_B    = U_H; 
                        U_Bdot = U_Hdot;
                    otherwise
                        error('Unknown  RefFrame option');
                end
                
            end
           
            % Sun
            if isempty(S_B)
                if nargout>2 
                    S_B = Args.INPOP.getPos('Sun', JD - Args.SunLightTime, 'TimeScale',Args.TimeScale, 'IsEclipticOut',false);
                    if nargout>3
                        S_Bdot = Args.INPOP.getVel('Sun', JD - Args.SunLightTime, 'TimeScale',Args.TimeScale, 'IsEclipticOut',false);
                    end
                end
            end
            
                
            switch lower(Args.CooSys)
                case 'eq'
                    % already in eqotorial system
                case 'ec'
                    % convert to ecliptic
                    RotM    = celestial.coo.rotm_coo('e');
                    U_B     = RotM * U_B;
                    U_Bdot  = RotM * U_Bdot;
                    S_B     = RotM * S_B;
                    S_Bdot  = RotM * S_Bdot;
                otherwise
                    error('Uknown CooSys option');
            end
                            
        end
        
        % Not working - likely a problem here - 
        function [Result, U_B, U_Bdot, S_B, S_Bdot] = integrateElements(Obj, FinalEpoch, Args)
            % Convert OrbitalEl object from one epoch to another
            %   via direct integration of the target, given perturbations
            %   from all major planets.
            %   This can be used to convert multiple target elements that
            %   have a common epoch into a new (scalar) epoch.
            % Input  : - A celestial.OrbitalEl object.
            %          - A scalar Julian day of final epoch to which o convert the
            %            epoch of the elements.
            %          * ...,key,val,...
            %            'TimeScale' - Default is 'TDB'.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   Provide in order to expedite the
            %                   calculations. If [], then will be loaded.
            %                   Default is [].
            %            'Tol' - Tolerance for Kepler equation solution.
            %                   Default is 1e-8.
            %            'TolInt' - Integration tolerance.
            %                   Default is 1e-8.
            % Output : - A new celestial.OrbitalEl object with the elements
            %            refered to the FinalEpoch.
            %          - (U_B) A 3 x N matrix of target barycentric
            %            ecliptic position based on the orbital integration.
            %          - (U_Bdot) target barycentric ecliptic velocity.
            %          - (S_B) Sun barycentric ecliptic position.
            %          - (S_Bdot) Sun barycentric ecliptic position.
            % Author : Eran Ofek (Nov 2023)
            % Example: OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9801:9900]);
            %          JD = 2460300.5;
            %          Result = integrateElements(OrbEl, JD);
            %          % compare with JPL
            %          [T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','ELEMENTS','TimeScale','TDB','StartTime',JD,'StopTime',JD+0.5);
            
            arguments
                Obj(1,1)
                FinalEpoch(1,1)
                Args.TimeScale       = 'TDB';
                Args.INPOP           = [];
                Args.Tol             = 1e-8;
                Args.TolInt          = 1e-8;
            end
            Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
            % check that all initial epochs are the same
            StartEpoch = unique(Obj.Epoch);
            if numel(StartEpoch)>1
                error('All Epoch must be the same');
            end
            
            % Calculate the rectangular ecliptic coordinate of the targets
            % via direct integration
            [U_B, U_Bdot, S_B, S_Bdot] = targetBaryPos(Obj, FinalEpoch, 'X0',[],'V0',[],'JD0',[],...
                                                                        'Integration',true,...
                                                                        'TimeScale',Args.TimeScale,...
                                                                        'CooSys','ec',...
                                                                        'RefFrame','bary',...
                                                                        'INPOP',Args.INPOP,...
                                                                        'Tol',Args.Tol, 'TolInt',Args.TolInt);
            % compare U_B w/ JPL
            %[T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','VECTORS',...
            %                    'TimeScale','TDB','StartTime',FinalEpoch,'StopTime',FinalEpoch+0.5, 'CENTER','500@0');
            %U_B(:,1) - [T.X; T.Y; T.Z]
            % looks good
                                                
            % Convert barycentric to heliocentric (ecliptic)
            U_H    = U_B - S_B;
            U_Hdot = U_Bdot - S_Bdot;
            
            
            % compare with JPL
            %[T] = celestial.SolarSys.getJPL_ephem('9801;','EPHEM_TYPE','VECTORS',...
            %                    'TimeScale','TDB','StartTime',FinalEpoch,'StopTime',FinalEpoch+0.5, 'CENTER','500@10');
            %U_H(:,1) - [T.X; T.Y; T.Z]
            
            
            % Convert rectangular position to orbital elements
            [~,Result] = celestial.Kepler.xyz2elements(U_H, U_Hdot, FinalEpoch, 'CooSys','ec');
            
        end
        
        function Result = propagate2commonEpoch(Obj, CommonEpoch, Args)
            % Propagate all orbital elements to a common epoch
            % Input  : - A single element celestial.OrbitalEl object.
            %          - JD of common epoch in which to integrate all the
            %            orbital elements.
            %            If empty, then will look for the most common epoch
            %            in the OrbitalEl object and use it.
            %            Default is [].
            %          * ...,key,val,...
            %            'TimeScale' - Default is 'TDB'.
            %            'INPOP' - A populated celestial.INPOP object.
            %                   Provide in order to expedite the
            %                   calculations. If [], then will be loaded.
            %                   Default is [].
            %            'Tol' - Tolerance for Kepler equation solution.
            %                   Default is 1e-8.
            %            'TolInt' - Integration tolerance.
            %                   Default is 1e-10.
            %
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is true.
            % Output : - A celestial.OrbitalEl object in which all the
            %            bodies have the same epoch.
            % Author : Eran Ofek (Nov 2023)
            % Example: Result = propagate2commonEpoch(OrbEl, 2460100);
            %          
            %          % full example to the most common epoch
            %          OrbEl = celestial.OrbitalEl.loadSolarSystem;
            %          E=merge(OrbEl,'MinEpoch',celestial.time.julday([1 1 2014]),'MaxEccen',0.9999);
            %          IN = celestial.INPOP;
            %          IN.populateTables('all','TimeSpan',[min(E.Epoch)-100, max(E.Epoch)+100])
            %          IN.populateTables('Sun','FileData','vel');
            %          Result = propagate2commonEpoch(E, [], 'INPOP',IN);
            %          

            arguments
                Obj(1,1)
                CommonEpoch     = [];
                Args.TimeScale       = 'TDB';
                Args.INPOP           = [];
                Args.Tol             = 1e-8;
                Args.TolInt          = 1e-8;
                
                Args.CreateNewObj logical = true;

            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end


            [UniqueEpochs,Ia,Ic] = unique(Obj.Epoch);
            
            % figure out the most frequent epoch - and use it as
            % CommonEpoch
            % count UniqueEpochs
            NunEpoch = numel(UniqueEpochs);
            for IunEpoch=1:1:NunEpoch
                FlagUn{IunEpoch} = UniqueEpochs(IunEpoch)==Obj.Epoch;
                SumUn(IunEpoch)  = sum(FlagUn{IunEpoch});
            end
            if isempty(CommonEpoch)
                [~,Imax] = max(SumUn);
                CommonEpoch = UniqueEpochs(Imax);
            end

            for IunEpoch=1:1:NunEpoch
                [IunEpoch, NunEpoch, SumUn(IunEpoch), UniqueEpochs(IunEpoch)-CommonEpoch]
                ObjUn = selectFlag(Obj, FlagUn{IunEpoch}, true);
                if UniqueEpochs(IunEpoch)==CommonEpoch
                    % skip - no need to integrate
                else
                    ObjUn = integrateElements(ObjUn, CommonEpoch, 'TimeScale',Args.TimeScale,...
                                                          'INPOP',Args.INPOP,...
                                                          'Tol',Args.Tol,...
                                                          'TolInt',Args.TolInt);
                    Result = insertElements(Result, ObjUn, FlagUn{IunEpoch}, 'CreateNewObj',false);
                end
            end

        end
    end

    methods % ephemerides
        function [Result] = searchMinorPlanetsNearPosition(Obj, JD, RA, Dec, SearchRadius, Args)
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
            %            'INPOP' - A populated celestial.INPOP object.
            %                   If empty then will be generated.
            %                   Default is [].
            %            'AddDist' - A logical indicating if to add a
            %                   'Dist' column to the output table, containing the
            %                   distance between the search position and asteroid
            %                   [arcsec].
            %                   Deafult is true.
            %
            %            'GeoPos' - Geodetic position of the observer (on
            %                   Earth). [Lon (rad), Lat (rad), Height (m)].
            %                   If empty, then calculate geocentric
            %                   positions. Default is [].
            %            'RefEllipsoid' - Reference ellipsoid for the
            %                   geodetic positions. Default is 'WGS84'.
            %
            %            'ConeSearch' - A logical indicating if to refine
            %                   the final search and to list only bodies
            %                   within the search radius. Otherwise will
            %                   return all sources found in the intial
            %                   search + buffer.
            %                   This is operational only if 'AddDist' is
            %                   true.
            %                   Default is false.
            %            'coneSearchArgs' - A cell array of additional
            %                   arguments to pass to imProc.match.coneSearch
            %                   Default is {}.
            %            'QuickSearchBuffer' - In the first iteration the
            %                   search radius is increased by this amount.
            %                   Default is 500 (units given by the
            %                   'SearchBufferUnits' key/val).
            %            'SearchBufferUnits' - Units of
            %                   'QuickSearchBuffer'. Default is 'arcsec'.
            %
            %            'Integration' - A logical indicating if to perform
            %                   orbital integration, including major bodies
            %                   perturbations. The integration is done only
            %                   on bodies found within the search radius + buffer
            %                   in the first iteration.
            %                   Default is true.
            %            
            %            'TimeScale' - Time scale of JD. 
            %                   Default is 'TDB'.
            %            'ObserverEphem' - A matrix contain observer position [au] and velocities [au/d] in
            %                   Heliocentric equatorial coordinates for each epoch. The columns are [x,y,z,vx,vy,vz]. 
            %                   If empty, the function will use EarthEphem and GeoPos.
            %                   In case of size [Nepoch,3], the function assume zero velocity.
            %                   Defauls is [].
            %            'Tol' - Tolerance [rad] for solving the Kepler
            %                   equation. Default is 1e-8.
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
            % Output : - An AstroCatalog object with the ephemerides of the
            %            minor planets / comets found near the search
            %            position. The number of elements are equal to the
            %            number of elements in the input OrbitalEl object.
            %            You can merge the results using AstroTable/merge.
            % Author : Eran Ofek (Sep 2021)
            % Example: OrbEl1= celestial.OrbitalEl.loadSolarSystem('num');
            %          OrbEl1.propagate2commonEpoch;
            %          IN = celestial.INPOP; IN.populateAll;
            %          [Result] = searchMinorPlanetsNearPosition(OrbEl1, 2461000, 0, 0, 1000, 'INPOP',IN)
            

            arguments
                Obj
                JD
                RA
                Dec
                SearchRadius               = 1000;
                
                Args.SearchRadiusUnits     = 'arcsec';
                Args.CooUnits              = 'deg';
                Args.AddDist logical       = true;
                
                Args.MagLimit              = Inf;
                Args.INPOP                 = [];
                Args.GeoPos                = [];
                Args.RefEllipsoid          = 'WGS84';

                
                Args.ConeSearch logical    = false;
                Args.coneSearchArgs cell   = {};
                Args.QuickSearchBuffer     = 1000;    % to be added to SearchRadis (same units).
                Args.SearchBufferUnits     = 'arcsec';
                
                Args.Integration logical   = true;
                
                Args.TimeScale             = 'TDB';
                Args.ObserverEphem         = [];
                
                Args.Tol                   = 1e-8;
                Args.TolInt                = 1e-8;
                
                Args.OutType               = 'AstroCatalog';
                Args.OutUnitsDeg logical   = true;
                Args.IncludeMag logical    = true;
                Args.IncludeAngles logical = true;
                Args.IncludeDesignation logical = true;
                
            end
            RAD = 180./pi;
            
            if isempty(Args.INPOP)
                Args.INPOP = celestial.INPOP;
                Args.INPOP.populateAll;
            end
            
            if isinf(Args.MagLimit)
                IncludeMag = false;
            else
                IncludeMag = true;
            end
            
            SearchRadiusRAD      = convert.angular(Args.SearchRadiusUnits, 'rad', SearchRadius);
            QuickSearchBufferRAD = convert.angular(Args.SearchBufferUnits, 'rad', Args.QuickSearchBuffer);
            
            RA  = convert.angular(Args.CooUnits,'rad', RA);
            Dec = convert.angular(Args.CooUnits,'rad', Dec);
                        
            ObjNew = Obj.copy();

            Nobj = numel(ObjNew);
            % quick and dirty
            for Iobj=1:1:Nobj
                [Cat,ColNames] = celestial.ephem.ephemKepler(Obj(Iobj), JD, 'INPOP',Args.INPOP,...
                                                                  'GeoPos',Args.GeoPos,...
                                                                  'RefEllipsoid',Args.RefEllipsoid,...
                                                                  'MaxIterLT',1,...
                                                                  'TimeScale',Args.TimeScale,...
                                                                  'ObserverEphem',Args.ObserverEphem,...
                                                                  'Tol',Args.Tol,...
                                                                  'OutType','mat',...
                                                                  'Aberration',false,...
                                                                  'OutUnitsDeg',false,...
                                                                  'IncludeMag',IncludeMag,...
                                                                  'IncludeAngles',IncludeMag,...
                                                                  'IncludeDesignation',false);
                                                                   
               
                % Column indices
                ColRA  = find(strcmp(ColNames,'RA'));
                ColDec = find(strcmp(ColNames,'Dec'));
                ColMag = find(strcmp(ColNames,'Mag'));
                
                Dist   = celestial.coo.sphere_dist_fast(RA, Dec, Cat(:,ColRA), Cat(:,ColDec));
                
                % within search radius and MagLimit
                % RA - col 2
                % Dec - col 3
                % Mag - col 8
                Flag   = Dist<(SearchRadiusRAD + QuickSearchBufferRAD);
                if IncludeMag
                    Flag = Flag & Cat(:,ColMag)<Args.MagLimit;
                end
            
                selectFlag(ObjNew(Iobj), Flag, false);
            end
            
            % accurate search on selected sample:
            Result = AstroCatalog(size(ObjNew));
            for Iobj=1:1:Nobj
                if  numEl(ObjNew(Iobj))==0
                    Flag = [];
                else
                    if Args.Integration
                        % ise orbital integration
                        Result(Iobj) = celestial.ephem.ephemMultiObj(ObjNew(Iobj), JD, 'INPOP',Args.INPOP,...
                                                       'Integration',true,...
                                                       'IntegrationLT',false,...
                                                       'GeoPos',Args.GeoPos,...
                                                       'RefEllipsoid',Args.RefEllipsoid,...
                                                       'MaxIterLT',2,...
                                                       'TimeScale',Args.TimeScale,...
                                                       'ObserverEphem',Args.ObserverEphem,...
                                                       'Tol',Args.Tol,...
                                                       'TolInt',Args.TolInt,...
                                                       'OutType',Args.OutType,...
                                                       'Aberration',false,...
                                                       'OutUnitsDeg',Args.OutUnitsDeg,...
                                                       'IncludeMag',Args.IncludeMag,...
                                                       'IncludeAngles',Args.IncludeAngles,...
                                                       'IncludeDesignation',Args.IncludeDesignation);
                    else
                        % use kepler equation
                        [Result(Iobj)] = celestial.ephem.ephemKepler(ObjNew(Iobj), JD, 'INPOP',Args.INPOP,...
                                                                  'GeoPos',Args.GeoPos,...
                                                                  'RefEllipsoid',Args.RefEllipsoid,...
                                                                  'MaxIterLT',2,...
                                                                  'TimeScale',Args.TimeScale,...
                                                                  'ObserverEphem',Args.ObserverEphem,...
                                                                  'Tol',Args.Tol,...
                                                                  'OutType',Args.OutType,...
                                                                  'Aberration',false,...
                                                                  'OutUnitsDeg',Args.OutUnitsDeg,...
                                                                  'IncludeMag',Args.IncludeMag,...
                                                                  'IncludeAngles',Args.IncludeAngles,...
                                                                  'IncludeDesignation',Args.IncludeDesignation);
                                                                   
                        
                    end                           
               
                    % add Dist
                    if Args.AddDist
                        AstLonLat = getLonLat(Result(Iobj), 'rad');
                        Dist = celestial.coo.sphere_dist_fast(RA, Dec, AstLonLat(:,1), AstLonLat(:,2));
                        
                        Result(Iobj).insertCol(Dist.*RAD.*3600, Inf, {'Dist'}, {'arcsec'});
                    end

                    if Args.ConeSearch && Args.AddDist
                        SearchRadiusAS = convert.angular('rad','arcsec', SearchRadiusRAD);
                        Flag = Result(Iobj).getCol('Dist')<SearchRadiusAS;
                        Result(Iobj).selectRows(Flag, 'CreateNewObj',false);



                        % [Result(Iobj), Flag] = imProc.match.coneSearch(Result(Iobj), [RA, Dec], 'CooType','sphere',...
                        %                               'Radius',SearchRadiusRAD,...
                        %                               'RadiusUnits','rad',...
                        %                               'CooUnits','rad',...
                        %                               'CreateNewObj',false,...
                        %                               Args.coneSearchArgs{:});
                    end
                    
                    
                end
               
            end
                        
                        
        end
    end
    
    methods % conversion
        function Number = desig2number(Obj, Desig)
            % Convert asteroid designation to number
            % Input  : - A single element celestial.OrbitalEl object.
            %          - Asteroid designation, or a cell array of asteroids
            %            designation.
            % Output : - Asteroid number.
            %            If 0, then the designation was not found.
            %            If NaN, then the designation was found but the
            %            number is NaN.
            % Author : Eran Ofek (Dec 2023)
            % Example: OrbEl.desig2number('1998 ST50')
            %          OrbEl.desig2number({'1998 ST50','Ceres'})

            if ischar(Desig)
                Desig = {Desig};
            end

            Ndesig = numel(Desig);
            Number = zeros(Ndesig,1);
            for Idesig=1:1:Ndesig
                
                Ind = find(strcmp(Obj.Designation, Desig{Idesig}));
                if isempty(Ind)
                    Number(Idesig) = 0;
                else
                    Number(Idesig) = Obj.Number(Ind);
                end
            end
    
        end

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
        function Result = loadSolarSystem(Type, Desig, Args)
            % Load the JPL Solar System orbital elements from local disk
            %   To install the orbital elements use the Installer class.
            % Input  : - Type: [] - read all | 'num' | 'unnum' | 'comet' |
            %            'merge'.
            %            'merge' is stored in
            %            ~/matlab/data/SolarSystem/MinorPlanetsCT and
            %            contains a merged catalog with a common epoch.
            %            Default is [].
            %          - Minor planets designation (string) or number.
            %            If empty, return all. Default is [].
            %          * ...,key,name,...
            %            'MergedFile' - Default is
            %            {'MergedEpoch_2460400.mat','MergedEpoch_2460200.mat'}
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
                Args.MergedFile = {'MergedEpoch_2460400.mat','MergedEpoch_2460200.mat'};
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
                    case 'merge'
                        % load the merged common epoch orbital elements
                        % file
                        I = Installer; 
                        Nfile = numel(Args.MergedFile);
                        TryLoading = true;
                        Ifile = 0;
                        while TryLoading 
                            Ifile = Ifile + 1;
                            try
                                DataFile = strcat(I.getDataDir(I.Items.MinorPlanetsCT),filesep,Args.MergedFile{Ifile});
                                Result   = io.files.load2(DataFile);
                                TryLoading = false;
                            catch
                                warning('File %s does not exit - trying next file',Args.MergedFile{Ifile});
                            end
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
            %                   Default is 2451545.
            % Output : - An OrbitalEl object with random elements.
            % Author : Eran Ofek (Apr 2022)
            % Example: R = celestial.OrbitalEl.randomElements;
            
            arguments
                N             = 1e6;
                Args.A        = 1;
                Args.RangeE   = [0, 0.9];
                Args.Epoch    = 2451545;
                Args.RangeIncl = [-90, 90];
            end
            
            Result = celestial.OrbitalEl;
            Result.Number    = (1:1:N).';
            Result.A         = ones(N,1).*Args.A;
            Result.Node      = rand(N,1).*360;
            Result.W         = rand(N,1).*360;
            %Result.Incl      = rand(N,1).*180 - 90;
            % Inc        = (-90:1:90).';
            if min(Args.RangeIncl) ~= max(Args.RangeIncl)
                Inc         = (min(Args.RangeIncl):1:max(Args.RangeIncl)).';
                Result.Incl      = tools.math.stat.randgen([Inc, cosd(Inc)],N);
            else
                Result.Incl = min(Args.RangeIncl);
            end

            Result.Eccen     = rand(N,1).*range(Args.RangeE) + min(Args.RangeE);
            Result.Epoch     = ones(N,1).*Args.Epoch;
            Result.Mepoch    = rand(N,1).*360;
            Result.PeriDist  = Result.A.*(1 - Result.Eccen);

            % M = n*(t-T)
            Result.Tp = Args.Epoch - Result.Mepoch./Result.meanMotion('deg');
            
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
