% CelCoo class (matlab/astro/@CelCoo)
%       A class for spherical coordinates
% Description:
%       CelCoo is a lightweight container for celestial coordinates with
%       convenience getters and basic coordinate operations.
%       Main capabilities:
%         - Store RA/Dec vectors or matrices with metadata (Units, Equinox,
%           Epoch, and optional PM/parallax/radial-velocity fields).
%         - Convert coordinate representation on demand via dependent
%           properties:
%             * Rad / Deg  -> numeric [RA, Dec] in radians/degrees
%             * Sex        -> sexagesimal string output
%             * CosDir     -> direction cosines [X,Y,Z] and scalar X/Y/Z.
%         - Populate coordinates from numeric angles, sexagesimal strings,
%           or Cartesian direction cosines.
%         - Change displayed angular units while keeping coordinates
%           consistent.
%         - Precess coordinates between equinoxes using rotation matrices
%           from celestial.coo utilities.
%
%       Notes:
%         - Several methods are marked "not ready" and are currently
%           placeholders (e.g., full epoch/proper-motion propagation,
%           general system conversion, plotting helpers).
%         - Equinox and Epoch are intended to be Julian-system values.
%
% Examples:
% 


classdef CelCoo < matlab.mixin.Copyable
    properties 
        RA                   = [];
        Dec                  = [];
        Units                = 'rad';   % 'rad'|'deg'
        System               = 'eq';
        Equinox(1,1)         = 2000;    % always in J system
        IsTrue(1,1) logical  = false;   % mean equinox of date.
        Epoch     = 2000;    % always in J system
        EpochType = 'J';     % 'J'|'B'|'JD','MJD'
        PM_RA     = 0;      % [mas/yr]
        PM_Dec    = 0;      % [mas/yr]
        RadVel    = 0;      % [mas/yr]
        Plx       = 1e-6;   % [mas/yr]

        GeoCoo    = [35 30 415];  % [deg deg m]
    end

    properties (SetAccess=private)
        SortBy  = [];  % 'Dec';
    end

    properties (Hidden,Dependent)
        Rad
        Deg
        Sex
        CosDir
        X
        Y
        Z
    end

    properties (Hidden)
        KDTree = []; % KD-Tree
    end


    properties (Hidden)
        DoNotCall = false;  % do not call internal setter
    end
    

    methods % setters/getters
        function set.Units(Obj, NewUnits)
            % Set Units

            Conv = convert.angular(Obj.Units, NewUnits);
            Obj.RA    = Obj.RA.*Conv;
            Obj.Dec   = Obj.Dec.*Conv;
            Obj.Units = NewUnits;

        end

        % not ready
        function Obj=set.Equinox(Obj, NewEquinox)
            % Set the Equinox and precess coordinates accordingly

            if Obj.DoNotCall
                Obj.Equinox = NewEquinox;
                Obj.DoNotCall = false;
            else
                Obj.precess(NewEquinox);
            end
            
        end

        % not ready
        function Obj=set.Epoch(Obj, NewEpoch)
            % Set the Epoch and apply proper motion and parallax

            if Obj.DoNotCall
                Obj.Epoch = NewEpoch;
                Obj.DoNotCall = false;
            else
                Obj.properMotion(NewEpoch);
            end
        end

        function Val=get.Rad(Obj)
            % get [RA, Dec] in radians
            % Output is a two column matrix [RA, Dec]

            RAD = 180./pi;

            Val = [Obj.RA(:), Obj.Dec(:)];
            if strcmp(Obj.Units, 'deg')
                Val = Val./RAD;
            end

        end

        function Val=get.Deg(Obj)
            % get [RA, Dec] in degrees
            % Output is a two column matrix [RA, Dec]
            
            RAD = 180./pi;

            Val = [Obj.RA(:), Obj.Dec(:)];
            if strcmp(Obj.Units, 'rad')
                Val = Val.*RAD;
            end

        end

        function Val=get.Sex(Obj)
            % get {RA, Dec} in sexagesimal
            % Output is a two column string array [RA, Dec]
            
            RAD = 180./pi;

            SexRA  = celestial.coo.convertdms(Obj.RA(:), Obj.Units, 'SH');
            SexDec = celestial.coo.convertdms(Obj.Dec(:), Obj.Units, 'SD');

            Val = [string(SexRA), string(SexDec)];

        end

        function CD=get.CosDir(Obj)
            % get cosine direction X

            switch Obj.Units
                case 'rad'
                    X = cos(Obj.RA).*cos(Obj.Dec);
                    Y = sin(Obj.RA).*cos(Obj.Dec);
                    Z = sin(Obj.Dec);
                case 'deg'
                    X = cosd(Obj.RA).*cosd(Obj.Dec);
                    Y = sind(Obj.RA).*cosd(Obj.Dec);
                    Z = sind(Obj.Dec);
                otherwise
                    error('Unknown Units option');
            end
            CD = [X(:), Y(:), Z(:)];
        end

        function X=get.X(Obj)
            % get X cosine dir

            switch Obj.Units
                case 'rad'
                    X = cos(Obj.RA).*cos(Obj.Dec);
                case 'deg'
                    X = cosd(Obj.RA).*cosd(Obj.Dec);
                otherwise
                    error('Unknown Units option');
            end
        end

        function Y=get.Y(Obj)
            % get Y cosine dir

            switch Obj.Units
                case 'rad'
                    Y = sin(Obj.RA).*cos(Obj.Dec);
                case 'deg'
                    Y = sind(Obj.RA).*cosd(Obj.Dec);
                otherwise
                    error('Unknown Units option');
            end
        end

        function Z=get.Z(Obj)
            % get Z cosine dir

            switch Obj.Units
                case 'rad'
                    Z = sin(Obj.Dec);
                case 'deg'
                    Z = sind(Obj.Dec);
                otherwise
                    error('Unknown Units option');
            end
        end


    end
 
    
    methods % populate
        function Obj=populateEpoch(Obj, Epoch)
            % Convert Epoch to Julian Epoch and populate
            % Input  : - self.
            %          - Epoch. E.g., 'J2000.0','B1950', 2000
            %            If numeric, then assume in Julian years.
            % Output : - A CelCoo object in which the Epoch is populated in
            %            Julian years.
            % Author : Eran Ofek (Mar 2026)
            % Example: C.populateEpoch('B1950')

            arguments
                Obj
                Epoch
            end

            if ischar(Epoch) || isstring(Epoch)
                EpochType = Epoch(1);
                Epoch     = str2double(Epoch(2:end));

                % convert epoch to J
                Epoch = convert.time(Epoch, EpochType, 'J');

            else
                EpochType = 'J';
            end
            Obj.Epoch = Epocj;

        end

        function Obj=populateEquinox(Obj, Epoch)
            % Convert Equinox to Julian Equinox and populate
            % Input  : - self.
            %          - Equinox. E.g., 'J2000.0','B1950', 2000
            %            If numeric, then assume in Julian years.
            % Output : - A CelCoo object in which the Equinox is populated in
            %            Julian years.
            % Author : Eran Ofek (Mar 2026)
            % Example: C.populateEquinox('B1950')

            arguments
                Obj
                Epoch
            end

            if ischar(Epoch) || isstring(Epoch)
                EpochType = Epoch(1);
                Epoch     = str2double(Epoch(2:end));

                % convert epoch to J
                Epoch = convert.time(Epoch, EpochType, 'J');

            else
                EpochType = 'J';
            end
            Obj.Epoch = Epocj;

        end

        function Obj=populate(Obj, RA, Dec, InType)
            % Populate the RA and Dec properties in CelCoo object
            % Input  : - self.
            %          - RA ['sexagesimal'|'deg'|'rad']
            %            Alternativel, if a 3 column matrix, ad Dec is empty,
            %            then this is a [X, Y, Z] cosine dir.  
            %          - Dec ['sexagesimal'|'deg'|'rad']
            %            Default is [].
            %          - Input type: 'deg'|'rad'. If string will use
            %            sexagesimal. Default is 'deg'.
            % Output : - A CelCoo objcect with the populated coordinates.
            % Author : Eran Ofek (Mar 2026)
            % Example: C=CelCoo;
            %          C.populate('10:28:12.12','-19:10:12.1');

            arguments
                Obj
                RA
                Dec       = [];
                InType    = 'deg';
            end
            RAD = 180./pi;

            if isempty(Dec) && size(RA,2)==3
                % assume RA is cosine dir
                [Obj.RA, Obj.Dec] = celestial.coo.cosined2coo(RA(:,1), RA(:,2), RA(:,3), true);
            else
                if iscellstr(RA) || ischar(RA) || isstring(RA) 
                    Obj.RA = celestial.coo.convertdms(RA, 'gH', Obj.Units);
                else
                    Obj.RA = convert.angular(InType, Obj.Units, RA);
                end
    
                if iscellstr(Dec) || ischar(Dec) || isstring(Dec) 
                    Obj.Dec = celestial.coo.convertdms(Dec, 'gD', Obj.Units);
                else
                    Obj.Dec = convert.angular(InType, Obj.Units, Dec);
                end
            end
        
        end

    end

    
    methods % conversion 
        function Result=raRange(Obj, Type, Args)
            % Normalize RA values to a requested angular range.
            % Input  : - CelCoo object (scalar or array).
            %          - Requested RA range mode (case-insensitive):
            %            '2pi'|'pos'|'0-360'|'0-2pi'  : RA in [0,360) deg or [0,2*pi) rad.
            %            'pi'|'pm' |'+-180'|'+-pi'   : RA in [-180,180) deg or [-pi,pi) rad.
            %            Default is '2pi'.
            %          * ...,key,val,...
            %            'CreateNewObj' - If true, normalize in a copy.
            %                   Otherwise modify in place. Default is false.
            % Output : - CelCoo object with normalized RA.
            % Notes  : - Dec is unchanged.
            %          - Normalization is performed in Obj.Units.
            % Author : Eran Ofek (Apr 2026)
            % Example: C = C.raRange('pos');
            %          C = C.raRange('pm');

            arguments
                Obj
                Type = '2pi';
                Args.CreateNewObj  = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Nobj = numel(Result);
            for Iobj=1:1:Nobj
                if isempty(Result(Iobj).RA)
                    continue;
                end

                switch lower(Type)
                    case {'2pi', 'pos', '0-360', '0-2pi'}
                        if strcmp(Result(Iobj).Units, 'deg')
                            Result(Iobj).RA = mod(Result(Iobj).RA, 360);
                        else
                            Result(Iobj).RA = mod(Result(Iobj).RA, 2.*pi);
                        end

                    case {'pi', 'pm','+-180','+-pi','-180:180','-pi:pi'}
                        if strcmp(Result(Iobj).Units, 'deg')
                            Result(Iobj).RA = mod(Result(Iobj).RA + 180, 360) - 180;
                        else
                            Result(Iobj).RA = mod(Result(Iobj).RA + pi, 2.*pi) - pi;
                        end

                    otherwise
                        error('Unknown Type option: %s', string(Type));
                end
            end

        end

        function Result = decRange(Obj, Args)
            % Fold declination into the physical range [-90,+90] deg
            % (or [-pi/2,+pi/2] rad), while preserving spherical position.
            % Input  : - CelCoo object (scalar or array).
            %          * ...,key,val,...
            %            'CreateNewObj' - If true, normalize in a copy.
            %                   Otherwise modify in place. Default is false.
            % Output : - CelCoo object with Dec folded to physical range.
            % Notes  : - When Dec crosses a pole, RA is shifted by pi
            %            (or 180 deg), as required by spherical geometry.
            %          - Final RA is wrapped back to the same RA convention
            %            used in raRange('2pi') i.e., [0,360) or [0,2*pi).
            % Author : Eran Ofek (Apr 2026)
            % Example: C = C.decRange();

            arguments
                Obj
                Args.CreateNewObj  = false;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Nobj = numel(Result);
            for Iobj=1:1:Nobj
                if isempty(Result(Iobj).Dec)
                    continue;
                end

                if strcmp(Result(Iobj).Units, 'deg')
                    HalfPi = 90;
                    PiVal  = 180;
                    TwoPi  = 360;
                else
                    HalfPi = 0.5.*pi;
                    PiVal  = pi;
                    TwoPi  = 2.*pi;
                end

                % First fold to [-pi,+pi) (or [-180,+180))
                DecF = mod(Result(Iobj).Dec + PiVal, TwoPi) - PiVal;
                RA   = Result(Iobj).RA;

                % Reflect values outside [-pi/2,+pi/2] and shift RA by pi
                I = DecF > HalfPi;
                if any(I(:))
                    DecF(I) = PiVal - DecF(I);
                    RA(I)   = RA(I) + PiVal;
                end

                I = DecF < -HalfPi;
                if any(I(:))
                    DecF(I) = -PiVal - DecF(I);
                    RA(I)   = RA(I) + PiVal;
                end

                % Keep RA in the same default positive range as raRange('2pi')
                RA = mod(RA, TwoPi);

                Result(Iobj).RA  = RA;
                Result(Iobj).Dec = DecF;
            end

        end

        function Result=precess(Obj, NewEquinox, Args)
            % Precess coordinates to new equinox
            % Input  : - self.
            %          - New equinox. Either Julian year or JD (if IsJD is
            %            true).
            %          * ...,key,val,...
            %            'OutIsTrue' - A logical indicating if the output
            %                   coordinates refered to true equinox of date (true)
            %                   or mean equinox of date (false).
            %                   Default is true.
            %            'IsJD' - A logical indicating if the new equinox is
            %                   in JD (true) or Julian year (false).
            %                   Defgault is false.
            %            'CreateNewObj' - create a new copy of the object.
            %                   Default is true.
            % Output : - Updated CelCoo object.
            % Notes  : - Coordinates are precessed as currently stored in
            %            Obj.RA/Obj.Dec (feature; no automatic system check
            %            or conversion is performed).
            %          - Uses static CelCoo.precessCoo for the actual
            %            rotation.
            % Author : Eran Ofek (Mar 2026)
            % Example: C.precess(2023.212)
            %          C.precess(2460310.5,'IsJD',true,'OutIsTrue',false);

            arguments
                Obj
                NewEquinox
                Args.OutIsTrue     = true;
                Args.IsJD          = false; % NewEquinox is in JD
                Args.CreateNewObj  = true;
            end
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            if Args.IsJD
                NewEquinoxJD = NewEquinox;
            else
                NewEquinoxJD = convert.time(NewEquinox, 'J', 'JD');
            end

            Nobj = numel(Result);
            for Iobj=1:1:Nobj
                InEquinoxJD = convert.time(Obj(Iobj).Equinox, 'J', 'JD');
                [Result(Iobj).RA, Result(Iobj).Dec] = CelCoo.precessCoo(Obj(Iobj).RA,...
                                                                         Obj(Iobj).Dec,...
                                                                         InEquinoxJD,...
                                                                         NewEquinoxJD,...
                                                                         'CooUnits',Obj(Iobj).Units,...
                                                                         'InIsTrue',Obj(Iobj).IsTrue,...
                                                                         'OutIsTrue',Args.OutIsTrue);
                Result(Iobj).DoNotCall = true;
                Result(Iobj).Equinox = convert.time(NewEquinoxJD, 'JD', 'J');
                Result(Iobj).IsTrue  = Args.OutIsTrue;
            end

        end

        % not ready
        function Result=properMotion(Obj, NewEpoch, Args)
            % Propagate coordinates to a new epoch using proper motion,
            % optional parallax, and radial velocity.
            % Input  : - self.
            %          - New epoch. If numeric, interpreted as Julian year
            %            unless IsJD=true.
            %            If char/string, expected form like 'J2000.0' or
            %            'B1950', and converted to Julian year.
            %          * ...,key,val,...
            %            'ApplyPlx' - Apply parallax and radial-velocity
            %                   correction. Default is true.
            %            'IsJD' - If true, NewEpoch is in JD. If false,
            %                   NewEpoch is Julian year. Default is false.
            %            'CreateNewObj' - If true, propagate in a new copy;
            %                   otherwise update the current object.
            %                   Default is true.
            % Output : - Updated CelCoo object.
            % Author : Eran Ofek (Mar 2026)
            % Example: C = C.properMotion(2025.5);
            %          C = C.properMotion('J2025.5','ApplyPlx',false);
            %          C2 = C.properMotion(2030.0,'CreateNewObj',true);

            arguments
                Obj
                NewEpoch
                Args.ApplyPlx logical  = true;
                Args.IsJD logical      = false;
                Args.CreateNewObj      = true;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            % Interpret NewEpoch and convert to JD
            if ischar(NewEpoch) || isstring(NewEpoch)
                if Args.IsJD
                    error('When IsJD=true, NewEpoch must be numeric JD');
                end
                EpochType = NewEpoch(1);
                EpochVal  = str2double(NewEpoch(2:end));
                EpochJ    = convert.time(EpochVal, EpochType, 'J');
                EpochOutJD = convert.time(EpochJ, 'J', 'JD');
            else
                if Args.IsJD
                    EpochOutJD = NewEpoch;
                    EpochJ     = convert.time(NewEpoch, 'JD', 'J');
                else
                    EpochJ     = NewEpoch;
                    EpochOutJD = convert.time(EpochJ, 'J', 'JD');
                end
            end
            EpochInJD  = convert.time(Obj.Epoch, 'J', 'JD');

            ShapeCoo = size(Obj.RA);
            RadCoo   = Obj.Rad;
            RA       = RadCoo(:,1);
            Dec      = RadCoo(:,2);

            % Fill missing astrometric parameters with defaults
            PM_RA  = Obj.PM_RA;
            PM_Dec = Obj.PM_Dec;
            Plx    = Obj.Plx;
            RV     = Obj.RadVel;
            if ~Args.ApplyPlx
                Plx = 0;
                RV  = 0;
            end

            [NewRA, NewDec] = celestial.coo.proper_motion_parallax(EpochOutJD,...
                                                                    EpochInJD,...
                                                                    EpochInJD,...
                                                                    RA,...
                                                                    Dec,...
                                                                    PM_RA,...
                                                                    PM_Dec,...
                                                                    Plx,...
                                                                    RV);

            Conv    = convert.angular('rad', Obj.Units);
            Result.RA  = reshape(NewRA(:).*Conv, ShapeCoo);
            Result.Dec = reshape(NewDec(:).*Conv, ShapeCoo);
            Result.DoNotCall = true;
            Result.Epoch = EpochJ;

        end
    
        function Result=convert(Obj, Type, Args)
            % Rotate equatorial coordinates to another system.
            % Input  : - self.
            %          - Output system selector:
            %            'ecl'  | 'ecliptic'      -> ecliptic
            %            'gal'  | 'galactic'      -> galactic
            %            'sgal' | 'supergalactic' -> supergalactic
            %            'cmb'                      -> CMB dipole frame
            %            or a 3x3 numeric rotation matrix.
            %            Default is 'gal'.
            %          * ...,key,val,...
            %            'CreateNewObj' - If true, rotate in a copied
            %                   object. Otherwise update in place.
            %                   Default is true.
            % Output : - Updated CelCoo object.
            % Note   : Uses static CelCoo.convertCoo for the rotation.
            % Author : Eran Ofek (Mar 2026)
            % Example: Cg = C.convert('gal','CreateNewObj',true);
            %          Ce = C.convert('ecl');
            %          C2 = C.convert(RotM);

            arguments
                Obj
                Type = 'gal'
                Args.CreateNewObj logical = true;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Lon, Lat] = CelCoo.convertCoo(Obj(Iobj).RA, Obj(Iobj).Dec, Type, ...
                    'CooUnits', Obj(Iobj).Units, ...
                    'Equinox', Obj(Iobj).Equinox);
                Conv = convert.angular('rad', Obj(Iobj).Units);
                Result(Iobj).RA  = Lon .* Conv;
                Result(Iobj).Dec = Lat .* Conv;
                Result(Iobj).System = Type;
            end

        end

        function [Lon, Lat]=galCoo(Obj, Args)
            % Calculate galactic coordinates without changing the object
            % Input  : - self.
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'deg'.
            % Output : - Gal Lon.
            %          - Gal Lat.
            % Author : Eran Ofek (May 2026)
            % Example: [a,b]=C.galCoo

            arguments
                Obj
                Args.OutUnits   = 'deg';
            end

            [Lon, Lat] = CelCoo.convertCoo(Obj.RA, Obj.Dec, 'gal', 'Equinox',Obj.Equinox, 'CooUnits',Obj.Units);
            Conv = convert.angular(Obj.Units, Args.OutUnits);
            Lon  = Lon.*Conv;
            Lat  = Lat.*Conv;
        end

        function [Lon, Lat]=eclCoo(Obj, Args)
            % Calculate ecliptic coordinates without changing the object
            % Input  : - self.
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'deg'.
            % Output : - Ecl Lon.
            %          - Ecl Lat.
            % Author : Eran Ofek (May 2026)
            % Example: [a,b]=C.eclCoo

            arguments
                Obj
                Args.OutUnits   = 'deg';
            end

            [Lon, Lat] = CelCoo.convertCoo(Obj.RA, Obj.Dec, 'ecl', 'Equinox',Obj.Equinox, 'CooUnits',Obj.Units);
            Conv = convert.angular(Obj.Units, Args.OutUnits);
            Lon  = Lon.*Conv;
            Lat  = Lat.*Conv;
        end

        function [Az, Alt, AM] = azAlt(Obj, JD, Args)
            % Convert RA/Dec to horizontal coordinates (Az/Alt).
            % Input  : - A single-element CelCoo object.
            %          - JD scalar, or JD array with same size as RA/Dec.
            %          * ...,key,val,...
            %            'GeoCoo' - Geodetic coordinates
            %                   [Lon(deg), Lat(deg), Height(m)].
            %                   If empty, then use GeoCoo property.
            %                   Default is [].
            %            'OutUnits' - Output units for Az/Alt.
            %                   Default is Obj.Units.
            %            'LSTType' - 'a' (apparent) | 'm' (mean).
            %                   Default is 'a'.
            % Output : - Azimuth array.
            %          - Altitude array.
            %          - Airmass array.
            % Author : Eran Ofek (Apr 2026)
            % Example: [Az,Alt] = C.azAlt(2451545,'GeoCoo',[35 30 415]);

            arguments
                Obj(1,1)
                JD
                Args.GeoCoo = [];   % [deg deg m]
                Args.OutUnits = [];
                Args.LSTType = 'a';
            end

            if isempty(Args.GeoCoo)
                Args.GeoCoo = Obj.GeoCoo;
            end

            if ~strcmpi(Obj.System, 'eq')
                error('convertHoriz currently expects equatorial coordinates (Obj.System=''eq'')');
            end

            RA  = Obj.RA;
            Dec = Obj.Dec;
            if isempty(RA) || isempty(Dec)
                Az  = [];
                Alt = [];
                return;
            end
            if ~isequal(size(RA), size(Dec))
                error('Obj.RA and Obj.Dec must have the same size');
            end

            if isscalar(JD)
                JDuse = JD + zeros(size(RA));
            else
                if ~isequal(size(JD), size(RA))
                    error('JD must be scalar or have the same size as RA/Dec');
                end
                JDuse = JD;
            end

            if isempty(Args.OutUnits)
                Args.OutUnits = Obj.Units;
            end

            [Az, Alt] = celestial.coo.radec2azalt(JDuse,...
                                                  RA,...
                                                  Dec,...
                                                  'GeoCoo', Args.GeoCoo,...
                                                  'InUnits', Obj.Units,...
                                                  'OutUnits', Args.OutUnits,...
                                                  'LSTType', Args.LSTType);
        end


        function [varargout] = riseSet(Obj, JD, Alt, Args)
            % Calculate next rise/set times for object coordinates.
            % Input  : - A single-element CelCoo object.
            %          - Reference JD (scalar). Rise/Set are returned after JD.
            %          - Altitude threshold (scalar). Default is 0.
            %          * ...,key,val,...
            %            'GeoCoo' - Geodetic coordinates
            %                   [Lon(deg), Lat(deg), Height(m)].
            %                   If empty, use object GeoCoo property.
            %                   Default is [].
            %            'STType' - Sidereal time type: 'a' | 'm'.
            %                   Default is 'a'.
            % Output : - Rise JD array, same size as Obj.RA/Obj.Dec.
            %          - Set JD array, same size as Obj.RA/Obj.Dec.
            %          - Optional rise azimuth array (same angular units as
            %            Obj.Units).
            %          - Optional set azimuth array (same angular units as
            %            Obj.Units).
            % Notes  : - Coordinates are passed as stored in Obj.RA/Obj.Dec.
            %          - NaN is returned for non-rising/non-setting targets.
            % Author : Eran Ofek (Apr 2026)
            % Example: [R,S] = C.riseSet(2460400.5);
            %          [R,S,RAz,SAz] = C.riseSet(2460400.5,-0.5667,...
            %                           'GeoCoo',[35 30 415],'STType','a');

            arguments
                Obj(1,1)
                JD (1,1)
                Alt (1,1)   = 0;
                Args.GeoCoo = [];
                Args.STType = 'a';
            end

            if isempty(Args.GeoCoo)
                Args.GeoCoo = Obj.GeoCoo;
            end

            if ~strcmpi(Obj.System, 'eq')
                error('riseSet currently expects equatorial coordinates (Obj.System=''eq'')');
            end

            RA  = Obj.RA;
            Dec = Obj.Dec;
            if isempty(RA) || isempty(Dec)
                Rise   = [];
                Set    = [];
                RiseAz = [];
                SetAz  = [];
                return;
            end
            if ~isequal(size(RA), size(Dec))
                error('Obj.RA and Obj.Dec must have the same size');
            end

           
            [varargout{1:nargout}] = celestial.time.riseSet(JD, RA, Dec, Alt, ...
                                           'ObsPos', Args.GeoCoo, ...
                                           'InUnits', Obj.Units, ...
                                           'STType', Args.STType);
               
        end

    end

    methods % distance and search
        function [Dist,PA] = dist(Obj, RA, Dec, Args)
            % Angular distance and position angle from object coordinates.
            % Input  : - CelCoo object (single element).
            %          - RA of comparison point(s), or object name(s)
            %            resolvable by celestial.convert.cooResolve.
            %            This can be scalar or array of the same size as
            %            the RA/Dec in the object.
            %          - Dec of comparison point(s). Can be empty when RA
            %            is resolvable object name(s).
            %          * ...,key,val,...
            %            'InUnits' - Units of input RA/Dec ('rad'|'deg').
            %                   Default is 'deg'.
            %            'OutUnits' - Output units for Dist/PA ('rad'|'deg').
            %                   Default is 'deg'.
            %            'Server' - Name resolver server passed to
            %                   celestial.convert.cooResolve.
            %                   Default is [].
            % Output : - Angular distance between Obj coordinates and input
            %            coordinates.
            %          - Position angle (eastward from north) of Obj
            %            coordinates as seen from the input coordinates.
            % Author : Eran Ofek (Apr 2026)
            % Example: [D,PA] = C.dist(180, 30, 'InUnits','deg','OutUnits','deg');
            %          [D,PA] = C.dist('M31',[],'Server','simbad');
            %          D = C.dist(RA2, Dec2, 'InUnits','rad');

            arguments
                Obj(1,1)
                RA
                Dec           = [];
                Args.InUnits  = 'deg';
                Args.OutUnits = 'deg';
                Args.Server   = []; % see celestial.convert.cooResolve
            end

            [RA, Dec] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad','Server',Args.Server);

            Coo = Obj.Rad;

            [Dist] = celestial.coo.sphere_dist_fast(RA(:), Dec(:), Coo(:,1), Coo(:,2));
            ConvFactor = convert.angular('rad',Args.OutUnits);
            Dist       = Dist.*ConvFactor;
            if nargout>1
                PA = celestial.coo.position_angle(RA, Dec, Coo(:,1), Coo(:,2));
                PA = PA.*ConvFactor;
            end
        end

        function [Obj,SI]=sort(Obj, SortBy)
            % Sort coordinate vectors in-place by RA or Dec.
            % Input  : - A single-element CelCoo object.
            %            Obj.RA and Obj.Dec are expected to be vectors (or
            %            arrays with compatible linear indexing).
            %          - Sort key:
            %            'Dec' | 'RA' (case-insensitive).
            %            Default is 'Dec'.
            % Output : - The same CelCoo object, with RA/Dec reordered by
            %            the selected key.
            %            The selected key is also written to Obj.SortBy.
            % Notes  : - This method sorts only RA and Dec arrays.
            %            Associated arrays (e.g., PM_RA/PM_Dec) are not
            %            reordered here.
            %          - The sorted indices vector.
            % Author : Eran Ofek (Apr 2026)
            % Example: C = C.sort('RA');
            %          C = C.sort('Dec');

            arguments
                Obj(1,1)
                SortBy  = 'Dec';
            end

            switch lower(SortBy)
                case 'dec'
                    [~,SI] = sort(Obj.Dec);
                case 'ra'
                    [~,SI] = sort(Obj.RA);
                otherwise
                    error('Uknown SortBy option');
            end
            Obj.RA  = Obj.RA(SI);
            Obj.Dec = Obj.Dec(SI);
            Obj.SortBy = SortBy;

        end
    

        function [Obj]=populateKDTree(Obj, Args)
            % Build/populate a KD-tree index for fast coordinate matching.
            % Input  : - CelCoo object.
            %          * ...,key,val,...
            %            'Type' - KD-tree type/mode passed to
            %                   celestial.KDTreeCoo.populate.
            %                   Default is [] (use KDTreeCoo default).
            % Output : - CelCoo object with Obj.KDTree populated.
            % Notes  : - Uses Obj.RA/Obj.Dec as currently stored.
            %          - Uses Obj.Units as input units for KDTree build.
            % Author : Eran Ofek (Apr 2026)
            % Example: C = C.populateKDTree();
            %          C = C.populateKDTree('Type','Flat');

            arguments
                Obj
                Args.Type   = [];
            end

            Obj.KDTree = celestial.KDTreeCoo;
            Obj.KDTree.populate(Obj.RA, Obj.Dec, 'InUnits',Obj.Units, 'Type',Args.Type);

        end

        function [Ind, Dist, Nmatch, MatchSt]=matchSorted(Obj, RA, Dec, SearchRadius, Args)
            % Match input coordinates against a pre-sorted CelCoo catalog.
            % Input  : - A single-element CelCoo object with sorted
            %            coordinates (Obj.SortBy must be populated).
            %          - RA of query coordinate(s), or resolvable object
            %            name(s).
            %          - Dec of query coordinate(s). Can be empty when RA
            %            is provided as name(s) for coordinate resolution.
            %          - Search radius (scalar or array).
            %          * ...,key,val,...
            %            'SearchRadiusUnits' - Units of search radius.
            %                   Default is 'arcsec'.
            %            'InUnits' - Units of input RA/Dec.
            %                   Default is 'deg'.
            %            'Server' - Name resolver server used by
            %                   celestial.convert.cooResolve.
            %                   Default is [].
            % Output : - Match indices returned by matchTwoCats.
            %            The size is identical to the size of the input
            %            search coordinates.
            %          - Angular distances returned by matchTwoCats
            %            (in radians).
            %          - Number of matches per source returned by
            %            matchTwoCats.
            %          - Match status/flags returned by matchTwoCats.
            % Notes  : - Uses binary-search matcher:
            %            imUtil.match.mex.matchTwoCats.
            %          - Obj coordinates are taken from Obj.Rad
            %            (i.e., radians).
            %          - Input coordinates are resolved/converted to radians
            %            using celestial.convert.cooResolve.
            % Author : Eran Ofek (Apr 2026)
            % Example: [Ind,Dist,Nm,St] = C.matchSorted(180,30,5,...
            %                     'InUnits','deg','SearchRadiusUnits','arcsec');
            %          [Ind,Dist] = C.matchSorted('M31',[],30,'Server','simbad');

            arguments
                Obj(1,1)
                RA
                Dec           = [];
                SearchRadius  = 10;
                Args.SearchRadiusUnits = 'arcsec';
                Args.InUnits  = 'deg';
                Args.Server   = []; % see celestial.convert.cooResolve
            end

            [RA, Dec] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','rad','Server',Args.Server);

            Coo = Obj.Rad;

            if isempty(Obj.SortBy)
                error('Requires sorted coordinates');
            end
               
            % use binary search 
            SearchRadiusRad = convert.angular(Args.SearchRadiusUnits, 'rad', SearchRadius);
            [Ind, Dist, Nmatch, MatchSt] = imUtil.match.mex.matchTwoCats(Coo(:,1), Coo(:,2), RA, Dec, SearchRadiusRad, false, false, false);
              
        end

        function [ID, Dist] = matchKD(Obj, RA, Dec, SearchRadius, Args)
            % Match input coordinates using pre-populated KDTree.
            % Input  : - A single-element CelCoo object with populated
            %            Obj.KDTree.
            %          - RA of query coordinate(s), or resolvable object
            %            name(s).
            %          - Dec of query coordinate(s). Can be empty when RA
            %            is resolvable name(s).
            %          - Search radius (scalar or array).
            %          * ...,key,val,...
            %            'SearchRadiusUnits' - Radius units for search.
            %                   Default is 'arcsec'.
            %            'InUnits' - Units of input RA/Dec.
            %                   Default is 'deg'.
            %            'Server' - Name resolver server used by
            %                   celestial.convert.cooResolve.
            %                   Default is [].
            %            'KDType' - KDTree search type, forwarded to
            %                   KDTreeCoo.coneSearch ('M'|'K'|[]).
            %                   Default is [] (auto).
            % Output : - Cell array of matched indices per query.
            %          - Cell array of distances per query.
            %            Output distances are those returned by
            %            KDTreeCoo.coneSearch.
            % Author : Eran Ofek (Apr 2026)
            % Example: [ID,D] = C.matchKD(180,30,5,'InUnits','deg',...
            %                     'SearchRadiusUnits','arcsec');
            %          [ID,D] = C.matchKD('M31',[],30,'Server','simbad');

            arguments
                Obj(1,1)
                RA
                Dec                 = [];
                SearchRadius        = 10;
                Args.SearchRadiusUnits = 'arcsec';
                Args.InUnits        = 'deg';
                Args.Server         = []; % see celestial.convert.cooResolve
                Args.KDType         = [];
            end

            if isempty(Obj.KDTree)
                error('KDTree is empty. Run populateKDTree first.');
            end

            [RA, Dec] = celestial.convert.cooResolve(RA, Dec, ...
                                                      'InUnits', Args.InUnits, ...
                                                      'OutUnits', 'rad', ...
                                                      'Server', Args.Server);

            [ID, Dist] = Obj.KDTree.coneSearch(RA, Dec, SearchRadius, ...
                                               'InUnits', 'rad', ...
                                               'RadiusUnits', Args.SearchRadiusUnits, ...
                                               'Type', Args.KDType);
        end

    
        function Result=inPolySphere(Obj, Poly, Args)
            % Test if object coordinates are inside a convex spherical polygon region.
            % Input  : - CelCoo object (scalar or array). Coordinates are
            %            taken from Obj.RA/Obj.Dec and evaluated in radians.
            %          - Polygon definition:
            %            Nx2 numeric matrix of [Lon,Lat] vertices.
            %            or a Nx3 cosine directions.
            %            See: celestial.htm.in_polysphere.
            %          * ...,key,val,...
            %            'PolyUnits' - Units of Nx2 polygon vertices
            %                   ('rad'|'deg'). Used only when Poly has
            %                   two columns. Default is 'rad'.
            %            'OutIsInd' - If true, return indices of matching
            %                   points. If false, return logical mask.
            %                   Default is false.
            % Output : - Logical array of points inside polygon (default),
            %            or index vector of inside points if OutIsInd=true.
            % Author : Eran Ofek (May 2026)
            % Example: Flag = C.inPolySphere([10 20; 15 20; 15 25; 10 25],'PolyUnits','deg');
            %          I = C.inPolySphere(Poly,'OutIsInd',true);

            arguments
                Obj
                Poly
                Args.PolyUnits  = 'rad';
                Args.OutIsInd   = false;
            end

            if size(Poly,2)==2
                Poly = convert.angular(Args.PolyUnits, 'rad', Poly);
            end

            Result = celestial.htm.in_polysphere(Obj.Rad, Poly);

            if Args.OutIsInd
                Result = find(Result);
            end

        end
    
    end

    methods % sun and moon
        function [Sun, Dist] = sunDist(Obj, JD, Args)
            % Sun coordinates and distance from object coordinates.
            % Input  : - CelCoo object (scalar or array).
            %          - JD scalar. If empty, uses current JD (UTC0.
            %            Default is celestial.time.julday().
            %          * ...,key,val,...
            %            'OutUnits' - Output angular units for Sun RA/Dec,
            %                   Sun Az/Alt, and Dist. Default is 'deg'.
            %            'GeoCoo' - Geodetic coordinates
            %                   [Lon(deg), Lat(deg), Height(m)] used for
            %                   horizontal coordinates. If empty, use
            %                   Obj.GeoCoo. Default is [].
            % Output : - Structure with Sun fields:
            %              RA, Dec  - Sun apparent equatorial coordinates.
            %              Az, Alt  - Sun horizontal coordinates.
            %              EqOfTime - Equation of time [minutes].
            %          - (Optional) Angular distance between each Obj
            %            coordinate and the Sun, in OutUnits.
            % Author : Eran Ofek (May 2026)
            % Example: Sun = C.sunDist();
            %          [Sun,D] = C.sunDist(2460430.5,'OutUnits','deg');
            %          [Sun,D] = C.sunDist(JD,'GeoCoo',[35 30 415],'OutUnits','rad');

            arguments
                Obj
                JD    = celestial.time.julday();
                Args.OutUnits   = 'deg';
                Args.GeoCoo     = [];
            end
            RAD = 180./pi;

            if isempty(Args.GeoCoo)
                Args.GeoCoo = Obj.GeoCoo;
            end

            % equation of time [min]
            [Sun.RA,Sun.Dec,~,~,Sun.EqOfTime]=celestial.SolarSys.suncoo(JD, 'a');
            [Sun.Az, Sun.Alt] = celestial.coo.radec2azalt(JD, Sun.RA, Sun.Dec,'GeoCoo',Obj.GeoCoo(1:2)./RAD, 'InUnits','rad', 'OutUnits','rad','LSTType','m');
            
            Conv = convert.angular('rad',Args.OutUnits);

            if nargout>1
                
                Coo = Obj.Rad;

                Dist = Conv.*celestial.coo.sphere_dist_fast(Sun.RA, Sun.Dec, Coo(:,1), Coo(:,2));

            end

            Sun.RA  = Sun.RA.*Conv;
            Sun.Dec = Sun.Dec.*Conv;
            Sun.Az  = Sun.Az.*Conv;
            Sun.Alt = Sun.Alt.*Conv;

        end

        function [Moon, Dist] = moonDist(Obj, JD, Args)
            % Moon coordinates and distance from object coordinates.
            % Input  : - CelCoo object (scalar or array).
            %          - JD scalar. If empty, uses current JD (UTC).
            %            Default is celestial.time.julday().
            %          * ...,key,val,...
            %            'OutUnits' - Output angular units for Moon RA/Dec,
            %                   Moon Az/Alt, and Dist. Default is 'deg'.
            %            'GeoCoo' - Geodetic coordinates
            %                   [Lon(deg), Lat(deg), Height(m)] used for
            %                   topocentric Moon position and horizontal
            %                   coordinates. If empty, use Obj.GeoCoo.
            %                   Default is [].
            % Output : - Structure with Moon fields:
            %              RA, Dec  - Moon apparent equatorial coordinates.
            %              Az, Alt  - Moon horizontal coordinates.
            %              Illum    - Moon illuminated fraction (negative
            %                   for wanning moon).
            %          - (Optional) Angular distance between each Obj
            %            coordinate and the Moon, in OutUnits.
            % Notes  : - Dist is computed against Obj coordinates as stored,
            %            internally via radians.
            %          - If Obj contains multiple coordinates, Dist follows
            %            Obj.RA/Obj.Dec linearized shape convention.
            % Author : Eran Ofek (May 2026)
            % Example: Moon = C.moonDist();
            %          [Moon,D] = C.moonDist(2460430.5,'OutUnits','deg');
            %          [Moon,D] = C.moonDist(JD,'GeoCoo',[35 30 415],'OutUnits','rad');

            arguments
                Obj
                JD    = celestial.time.julday();
                Args.OutUnits   = 'deg';
                Args.GeoCoo     = [];
            end
            RAD = 180./pi;

            if isempty(Args.GeoCoo)
                Args.GeoCoo = Obj.GeoCoo;
            end

            [Moon.RA,Moon.Dec]=celestial.SolarSys.mooncool(JD, Args.GeoCoo(1:2)./RAD, 'b');
            [Moon.Az, Moon.Alt] = celestial.coo.radec2azalt(JD, Moon.RA, Moon.Dec,'GeoCoo',Obj.GeoCoo(1:2)./RAD, 'InUnits','rad', 'OutUnits','rad','LSTType','m');
            
            Conv = convert.angular('rad',Args.OutUnits);

            if nargout>1
                
                Coo = Obj.Rad;

                Dist = Conv.*celestial.coo.sphere_dist_fast(Moon.RA, Moon.Dec, Coo(:,1), Coo(:,2));

            end

            Moon.RA  = Moon.RA.*Conv;
            Moon.Dec = Moon.Dec.*Conv;
            Moon.Az  = Moon.Az.*Conv;
            Moon.Alt = Moon.Alt.*Conv;

            % Moon illumination
            Moon.Illum = celestial.SolarSys.moon_illum(JD);

        end

    end

    methods % plots
        % not ready
        function plotAitoff(Obj, Marker, Args)
            % Plot coordinates in Aitoff projection 
            %   Plot coordinates in Aitoff along with optional
            %   galactic/ecliptic plane lines.
            %   Uses MATLAB Mapping Toolbox (axesm/plotm).
            % Input  : - CelCoo object (scalar or array).
            %          - Marker style (char) or cell array of plotting
            %            arguments. Default is {'k.','MarkerSize',5}.
            %          * ...,key,val,...
            %            'Grid' - Backward-compatible grid flag.
            %                   Default is false.
            %            'GridOn' - Explicit grid flag. If empty, use
            %                   'Grid'. Default is [].
            %            'Labels' - Show map meridian/parallel labels.
            %                   Default is true.
            %            'MapProjection' - Mapping Toolbox projection name.
            %                   Default is 'aitoff'.
            %            'CooSys' - Output/display coordinate system:
            %                   'eq' (equatorial), 'gal' (galactic),
            %                   'ec'/'ecl' (ecliptic). Default is 'eq'.
            %            'AddEcliptc' - Overlay ecliptic equator.
            %                   Default is true.
            %            'EclMarker' - Marker/style for ecliptic equator.
            %                   Default is {'r--','LineWidth',1}.
            %            'AddGalactic' - Overlay galactic equator.
            %                   Default is true.
            %            'GalMarker' - Marker/style for galactic equator.
            %                   Default is {'b--','LineWidth',1}.
            % Output : null.
            % Author : Cursor + Eran Ofek (Mar 2026)
            % Example: C.plotAitoff({'k.','MarkerSize',6},'CooSys','eq',...
            %                       'GridOn',true,'Labels',true);
            %          C.plotAitoff('k.','CooSys','gal','AddEcliptc',false);
            %          C.plotAitoff('Labels',false,'AddGalactic',false);
            

            arguments
                Obj
                Marker      = {'k.','MarkerSize',5}; 
                Args.Grid   = false;
                Args.GridOn = [];
                Args.Labels logical = true;
                Args.MapProjection = 'aitoff';
                Args.CooSys = 'eq';  % 'gal'|'ec'
                Args.AddEcliptc  = true;  % add eqcliptic plane
                Args.EclMarker   = {'r--','LineWidth',1};
                Args.AddGalactic = true;  % add galactic plane
                Args.GalMarker   = {'b--','LineWidth',1};
                
            end

            if ischar(Marker)
                Marker = {Marker};
            end
            if ischar(Args.EclMarker)
                Args.EclMarker = {Args.EclMarker};
            end
            if ischar(Args.GalMarker)
                Args.GalMarker = {Args.GalMarker};
            end

            targetSys = localNormSys(Args.CooSys);
            PrevHold  = ishold();
            if exist('axesm','file') ~= 2 || exist('plotm','file') ~= 2
                error('plotAitoff requires MATLAB Mapping Toolbox (axesm/plotm)');
            end
            if isempty(Args.GridOn)
                Args.GridOn = Args.Grid;
            end

            % Plot objects in requested coordinate system.
            Lon = [];
            Lat = [];
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(Obj(Iobj).RA) || isempty(Obj(Iobj).Dec)
                    continue;
                end
                RA_rad  = convert.angular(Obj(Iobj).Units, 'rad', Obj(Iobj).RA);
                Dec_rad = convert.angular(Obj(Iobj).Units, 'rad', Obj(Iobj).Dec);
                EqJD    = convert.time(Obj(Iobj).Equinox, 'J', 'JD');
                srcSys  = localNormSys(Obj(Iobj).System);
                [LonI, LatI] = localRotateCoo(RA_rad, Dec_rad, srcSys, targetSys, EqJD);
                Lon = [Lon; LonI(:)];
                Lat = [Lat; LatI(:)];
            end

            Lon = mod(Lon + pi, 2.*pi) - pi;   % rad in [-pi,pi]
            LonDeg = Lon.*180./pi;
            LatDeg = Lat.*180./pi;

            % Use Mapping Toolbox Aitoff map axes.
            if ~ismap(gca)
                axesm(Args.MapProjection, ...
                      'Frame', 'on', ...                % keep ellipse map boundary
                      'MeridianLabel', 'off', ...
                      'ParallelLabel', 'off', ...
                      'MLabelParallel', 0, ...
                      'MLabelLocation', 60, ...
                      'PLabelMeridian', 0, ...
                      'PLabelLocation', 30);
            else
                setm(gca, 'MapProjection', Args.MapProjection, ...
                          'Frame', 'on', ...
                          'MeridianLabel', 'off', ...
                          'ParallelLabel', 'off', ...
                          'MLabelParallel', 0, ...
                          'MLabelLocation', 60, ...
                          'PLabelMeridian', 0, ...
                          'PLabelLocation', 30);
            end
            if Args.Labels
                setm(gca, 'MeridianLabel', 'on', 'ParallelLabel', 'on');
            else
                setm(gca, 'MeridianLabel', 'off', 'ParallelLabel', 'off');
            end
            % Hide regular Cartesian axes border/ticks; keep map frame ellipse.
            set(gca, 'Box', 'off', 'XColor', 'none', 'YColor', 'none');
            if Args.GridOn
                setm(gca, 'Grid', 'on');
            else
                setm(gca, 'Grid', 'off');
            end

            plotm(LatDeg, LonDeg, Marker{:});
            hold on;

            % Add ecliptic equator.
            if Args.AddEcliptc
                EqJD = convert.time(Obj(1).Equinox, 'J', 'JD');
                Lon0 = linspace(0, 2.*pi, 2000).';
                Lat0 = zeros(size(Lon0));
                [LonE, LatE] = localRotateCoo(Lon0, Lat0, "ecl", targetSys, EqJD);
                [LonE, LatE] = localBreakWrap(LonE, LatE);
                plotm(LatE.*180./pi, LonE.*180./pi, Args.EclMarker{:});
            end

            % Add galactic equator.
            if Args.AddGalactic
                EqJD = convert.time(Obj(1).Equinox, 'J', 'JD');
                Lon0 = linspace(0, 2.*pi, 2000).';
                Lat0 = zeros(size(Lon0));
                [LonG, LatG] = localRotateCoo(Lon0, Lat0, "gal", targetSys, EqJD);
                [LonG, LatG] = localBreakWrap(LonG, LatG);
                plotm(LatG.*180./pi, LonG.*180./pi, Args.GalMarker{:});
            end

            if ~PrevHold
                hold off;
            end

            function Sys = localNormSys(S)
                S = lower(string(S));
                if any(strcmp(S, ["eq","equ","equatorial"]))
                    Sys = "eq";
                elseif any(strcmp(S, ["gal","galactic"]))
                    Sys = "gal";
                elseif any(strcmp(S, ["ec","ecl","ecliptic"]))
                    Sys = "ecl";
                else
                    error('Unknown coordinate system: %s', string(S));
                end
            end

            function R = localRotEq2Sys(Sys, EqJD)
                switch Sys
                    case "eq"
                        R = eye(3);
                    case "gal"
                        R = celestial.coo.rotm_coo('g');
                    case "ecl"
                        R = celestial.coo.rotm_coo('e', EqJD);
                    otherwise
                        error('Unknown system option');
                end
            end

            function [OutLon, OutLat] = localRotateCoo(InLon, InLat, SrcSys, DstSys, EqJD)
                SrcSys = localNormSys(SrcSys);
                DstSys = localNormSys(DstSys);
                if SrcSys == DstSys
                    OutLon = InLon;
                    OutLat = InLat;
                    return;
                end
                [SX,SY,SZ] = celestial.coo.coo2cosined(InLon(:), InLat(:));
                Rsrc = localRotEq2Sys(SrcSys, EqJD);
                Rdst = localRotEq2Sys(DstSys, EqJD);
                Vdst = Rdst * (Rsrc.') * [SX.'; SY.'; SZ.'];
                [OutLon, OutLat] = celestial.coo.cosined2coo(Vdst(1,:).', Vdst(2,:).', Vdst(3,:).');
                OutLon = mod(OutLon + pi, 2.*pi) - pi;
            end

            function [LonW, LatW] = localBreakWrap(LonIn, LatIn)
                LonW = mod(LonIn + pi, 2.*pi) - pi;
                LatW = LatIn;
                J = find(abs(diff(LonW)) > pi*0.95);
                if isempty(J)
                    return;
                end
                K = numel(J);
                OutLon = nan(numel(LonW)+K,1);
                OutLat = nan(numel(LatW)+K,1);
                iIn = 1;
                iOut = 1;
                for i=1:numel(LonW)-1
                    OutLon(iOut) = LonW(iIn);
                    OutLat(iOut) = LatW(iIn);
                    iOut = iOut + 1;
                    if any(J==i)
                        iOut = iOut + 1; % keep NaN separator
                    end
                    iIn = iIn + 1;
                end
                OutLon(iOut) = LonW(end);
                OutLat(iOut) = LatW(end);
                LonW = OutLon;
                LatW = OutLat;
            end

        end

        % not ready
        function plotPM(Obj, Args)
            % Plot positions with proper motion vectors
            % Input  : - CelCoo object.
            %          * ...,key,val,...
            %            'CooUnits' - Plot units ('deg'|'rad').
            %                   Default is Obj.Units.
            %            'TimeBase' - Time baseline in years for vector
            %                   length scaling. Default is 1.
            %            'ApplyCosDec' - Convert PM_RA to dRA by dividing
            %                   by cos(Dec). Default is true.
            %            'PlotMarker' - Marker/cell args for positions.
            %                   Default is {'k.','MarkerSize',8}.
            %            'QuiverArgs' - Additional args for quiver.
            %                   Default is {'Color','r','LineWidth',1}.
            %            'QuiverScale' - Numeric quiver scale argument.
            %                   Default is 0 (no autoscale).
            % Output : null.
            % Example: C.plotPM('CooUnits','deg','TimeBase',10);

            arguments
                Obj(1,1)
                Args.CooUnits = [];
                Args.TimeBase(1,1) double = 1;
                Args.ApplyCosDec(1,1) logical = true;
                Args.PlotMarker = {'k.','MarkerSize',8};
                Args.QuiverArgs cell = {'Color','r','LineWidth',1};
                Args.QuiverScale = [];
            end

            if isempty(Args.CooUnits)
                Args.CooUnits = Obj(1).Units;
            end
            if ischar(Args.PlotMarker)
                Args.PlotMarker = {Args.PlotMarker};
            end

            RA0   = Obj.RA;
            Dec0  = Obj.Dec;
            PM_RA = Obj.PM_RA;
            PM_Dec= Obj.PM_Dec;

            if ~isequal(size(RA0), size(Dec0), size(PM_RA), size(PM_Dec))
                error('In a scalar CelCoo object, RA/Dec/PM_RA/PM_Dec must have identical size');
            end

            % Positions in requested plotting units.
            PosRA  = convert.angular(Obj.Units, Args.CooUnits, RA0);
            PosDec = convert.angular(Obj.Units, Args.CooUnits, Dec0);

            % Proper motion components [mas/yr] -> [plot-units/yr].
            dRA_mas = PM_RA;
            if Args.ApplyCosDec
                DecRad = convert.angular(Obj.Units, 'rad', Dec0);
                CDec = cos(DecRad);
                CDec(abs(CDec) < 1e-12) = NaN; % avoid singularity near poles
                dRA_mas = dRA_mas ./ CDec;
            end
            dRA  = convert.angular('mas', Args.CooUnits, dRA_mas) .* Args.TimeBase;
            dDec = convert.angular('mas', Args.CooUnits, PM_Dec)  .* Args.TimeBase;

            PrevHold = ishold();
            plot(PosRA, PosDec, Args.PlotMarker{:});
            hold on;
            if isempty(Args.QuiverScale)
                % Use quiver autoscaling by default for visibility.
                quiver(PosRA, PosDec, dRA, dDec, Args.QuiverArgs{:});
            else
                quiver(PosRA, PosDec, dRA, dDec, Args.QuiverScale, Args.QuiverArgs{:});
            end
            axis tight;
            box on;
            if ~PrevHold
                hold off;
            end

        end

    end

    methods (Static)  % in other files / unitTest
        Result = unitTest
        
        Result = perfTest
    end 

    methods (Static)
        function [OutRA, OutDec] = precessCoo(RA, Dec, InEpochJD, OutEpochJD, Args)
            % Precess coordinates between two epochs.
            % Input  : - RA.
            %          - Dec.
            %          - Input epoch (JD, scalar).
            %          - Output epoch (JD, scalar).
            %          * ...,key,val,...
            %            'CooUnits' - Coordinate units ('deg'|'rad').
            %                   Default is 'deg'.
            %            'InIsTrue' - If true, input is true equinox of date.
            %                   If false, input is mean equinox of date.
            %                   Default is true.
            %            'OutIsTrue' - If true, output is true equinox of date.
            %                   If false, output is mean equinox of date.
            %                   Default is false.
            % Output : - Precessed RA in requested units.
            %          - Precessed Dec in requested units.
            % Author : Eran Ofek (Mar 2026)
            % Example: [RA2,Dec2] = CelCoo.precessCoo(RA,Dec,2451545,2459396,...
            %                              'CooUnits','deg','InIsTrue',true,'OutIsTrue',false);

            arguments
                RA
                Dec
                InEpochJD
                OutEpochJD
                Args.CooUnits = 'deg';
                Args.InIsTrue logical = true;
                Args.OutIsTrue logical = false;
            end

            RAshape = size(RA);
            Decshape = size(Dec);
            if ~isequal(RAshape, Decshape)
                error('RA and Dec must have the same size');
            end

            RA_rad  = convert.angular(Args.CooUnits, 'rad', RA);
            Dec_rad = convert.angular(Args.CooUnits, 'rad', Dec);

            if InEpochJD == OutEpochJD
                OutRA  = RA;
                OutDec = Dec;
                return;
            end

            if Args.InIsTrue
                RotMatIn = celestial.coo.rotm_coo('pd', InEpochJD);
            else
                RotMatIn = celestial.coo.rotm_coo('p', InEpochJD);
            end
            if Args.OutIsTrue
                RotMatOut = celestial.coo.rotm_coo('Pd', OutEpochJD);
            else
                RotMatOut = celestial.coo.rotm_coo('P', OutEpochJD);
            end
            RotMat = RotMatOut * RotMatIn;
            [X, Y, Z] = celestial.coo.coo2cosined(RA_rad(:), Dec_rad(:));
            NewXYZ = RotMat * [X.'; Y.'; Z.'];
            [NewRA, NewDec] = celestial.coo.cosined2coo(NewXYZ(1,:).', NewXYZ(2,:).', NewXYZ(3,:).');

            Conv = convert.angular('rad', Args.CooUnits);
            OutRA  = reshape(NewRA .* Conv, RAshape);
            OutDec = reshape(NewDec .* Conv, Decshape);
        end

        function [Lon, Lat] = convertCoo(RA, Dec, Type, Args)
            % Convert equatorial RA/Dec to another spherical system.
            % Same Type options as CelCoo/convert (non-precession rotm_coo).
            % Input  : - RA (equatorial), same size as Dec.
            %          - Dec (equatorial).
            %          - Type: 'ecl'|'ecliptic'|'ec', 'gal'|'galactic',
            %            'sgal'|'supergalactic', 'cmb', or a 3x3 rotation
            %            matrix (equatorial -> output frame).
            %          * ...,key,val,...
            %            'CooUnits' - Units of RA/Dec ('rad'|'deg').
            %                   Default is 'rad'.
            %            'Equinox' - Julian year for ecliptic obliquity.
            %                   Default is 2000.
            % Output : - Longitude in radians (same shape as RA).
            %          - Latitude in radians (same shape as RA).
            % Author : Eran Ofek (Mar 2026)
            % Example: [l,b] = CelCoo.convertCoo(RA,Dec,'gal');
            %          [lam,bet] = CelCoo.convertCoo(RA,Dec,'ecl','CooUnits','deg');

            arguments
                RA
                Dec
                Type
                Args.CooUnits = 'rad';
                Args.Equinox = 2000;
            end

            RAshape = size(RA);
            if ~isequal(RAshape, size(Dec))
                error('RA and Dec must have the same size');
            end

            RA_rad  = convert.angular(Args.CooUnits, 'rad', RA);
            Dec_rad = convert.angular(Args.CooUnits, 'rad', Dec);

            if isnumeric(Type)
                if ~isequal(size(Type), [3 3])
                    error('If Type is numeric, it must be a 3x3 rotation matrix');
                end
                RotM = Type;
            else
                EqJD = convert.time(Args.Equinox, 'J', 'JD');
                switch lower(string(Type))
                    case {"ecl","ecliptic","ec"}
                        RotM = celestial.coo.rotm_coo('e', EqJD);
                    case {"gal","galactic"}
                        RotM = celestial.coo.rotm_coo('g');
                    case {"sgal","supergalactic"}
                        RotM = celestial.coo.rotm_coo('gSG') * celestial.coo.rotm_coo('g');
                    case {"cmb"}
                        RotM = celestial.coo.rotm_coo('gCMB') * celestial.coo.rotm_coo('g');
                    otherwise
                        error('Unknown Type option: %s', string(Type));
                end
            end

            [X, Y, Z] = celestial.coo.coo2cosined(RA_rad(:), Dec_rad(:));
            NewXYZ = RotM * [X.'; Y.'; Z.'];
            [LonCol, LatCol] = celestial.coo.cosined2coo(NewXYZ(1,:).', NewXYZ(2,:).', NewXYZ(3,:).');
            Lon = reshape(LonCol, RAshape);
            Lat = reshape(LatCol, RAshape);
        end
    end

end
    
