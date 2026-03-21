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
        PM_RA     = 0;      % [mas/yr]
        PM_Dec    = 0;      % [mas/yr]
        RadVel    = 0;      % [mas/yr]
        Plx       = 1e-6;   % [mas/yr]
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
        % not ready
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
            % Author : Eran Ofek (Mar 2026)
            % Example: C.precess(2023.212)

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


    end


    methods % plots
        % not ready
        function plotAitoff(Obj, Marker, Args)
            % Plot coordinates in Aitoff projection 
            %   Plot coordinates in Aitoff along with optional
            %   galactic/ecliptic plane lines.

            arguments
                Obj
                Marker      = {'k.','MarkerSize',5}; 
                Args.Grid   = false;
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

        end

        % not ready
        function plotPM
            % Plot positions with proper motion vectors

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
            %          - Input epoch (JD).
            %          - Output epoch (JD).
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
    
