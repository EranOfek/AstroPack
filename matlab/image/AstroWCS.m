% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

% TODO next-  TAN-SIP Distortion correction is sky2xy

classdef AstroWCS < Component
    % Component should contain:
    % UserData
    % Config
    
    
    % Add comments
    properties (Access = public)
%        Exist(1,1)   logical = false; % removed
        NAXIS(1,1)   uint8  = 2;
        WCSAXES(1,1) uint8  = 2;        
        CTYPE(1,:)   cell   = {'',''};   % e.g., 'RA---TAN', 'SIP', 'TPV', 'ZPN'
        CUNIT        cell   = {'',''};
        RADESYS      char   = 'ICRS';
        LONPOLE      double = 0; 
        LATPOLE      double = 90;
        EQUINOX      double = 2000.0;
        CRPIX(1,:)   double = [0 0];
        CRVAL(1,:)   double = [1 1];
        CD           double = [1 0;0 1];
        PV           double   = [];  % Changes to double from Cell
%        CDELT(1,:)   double = [1 1];   % removed - within AstroWCS we work only with CD. CD can be cosntructed from CDELT and PC
%        PC           double = [];      % removed - within AstroWCS we work only with CD. CD can be cosntructed from CDELT and PC
%        SIP          cell   = {zeros(0,2),zeros(0,2)}; Removed - absorbed in PV
    end     
    
    properties (GetAccess = public)
        ProjType     char   = 'none';
        ProjClass    char   = 'none';
        CooName(1,:) cell   = {'',''};
        
        AlphaP(1,1)  double = NaN;
        DeltaP(1,1)  double = NaN;
        PhiP(1,1)    double = NaN;
        
        Alpha0(1,1)  double = NaN;
        Delta0(1,1)  double = NaN;
        Phi0(1,1)    double = NaN;
        Theta0(1,1)  double = NaN;
        
        Tran2D(1,1) Tran2D              %= Tran2D;
        
    end
        
    % Future
%         WCS    % a structure with fields specified in Fields
%         gridLon     % a cube: e.g., Long = [X,Y,Color]
%         gridLat     % a cube: e.g., Lat  = [X,Y,Color]
%         gridCoo     % a cell array of {X,Y,...} - column per axes 
%         gridAxes = {'RA','Dec','Color'}
    
  
    
%======================================================================    
    
    methods
   %======== Constructor and general functions =========        
        
        % Constructor
        function Obj = AstroWCS(Headers)
            % Construct AstroWCS object and populate it with headers
            % Input  : - Either a vector of the the size of the empty
            %            AstroWCS object (e.g., [2 2]).
            %            OR a cell array of AstrHeaders.
            % Output : - An AstroWCS object with populated fields.
            % Author : Yossi Shvartzvald (June 2021)
            % Example: 
            
            arguments
                Headers      = 1;   % name or array size
            end
            
            if isnumeric(Headers)
                % create an empty AstroWCS object
                List = cell(Headers);
            elseif iscell(Headers)
                List = Headers;
            end

            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).ProjType = ''; % TODO change
            end            
            Obj = reshape(Obj,size(List));
            
        end
        
        function Obj = populate_projMeta(Obj)
            
             switch lower(Obj.ProjClass)
                case 'none'
                    Obj.Alpha0 = Obj.CRVAL(1);
                    Obj.Delta0 = Obj.CRVAL(2);
                    Obj.AlphaP = Obj.CRVAL(1);
                    Obj.DeltaP = Obj.CRVAL(2);

                    Obj.Phi0   = NaN;
                    Obj.Theta0 = NaN;
                    Obj.PhiP   = NaN;

                case 'zenithal'

                    Obj.Alpha0 = Obj.CRVAL(1);
                    Obj.Delta0 = Obj.CRVAL(2);
                    Obj.AlphaP = Obj.CRVAL(1);
                    Obj.DeltaP = Obj.CRVAL(2);

                    ConvFactor = convert.angular('deg',Obj.CUNIT{1});

                    Obj.Phi0   = 0.*ConvFactor;
                    Obj.Theta0 = 90.*ConvFactor;

                    if Obj.Delta0>=Obj.Theta0
                        Obj.PhiP = 0.*ConvFactor;
                    else
                        Obj.PhiP = 180.*ConvFactor;
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
             end            
        end        
        
   %======== Functions to construct AstroWCS from AstroHeader =========
        
        function Obj = read_ctype(Obj)
            % Use Obj.CTYPE to fill the fields ProjType, ProjClass,
            % CooName, and CUNIT (if empty)
            
            ProjTypeDict = Dictionary('DictName','WCS.ProjType');
            CunitDict = Dictionary('DictName','WCS.CUNIT');            
            
      
            ctype = Obj.CTYPE;
            
            projtype = cell(size(ctype));
            cooname  = cell(size(ctype));
            coounit  = cell(size(ctype));
            
            for I = 1:1:numel(ctype)
                Split    = regexp(ctype{I},'-','split');
                Pair     = Split(~tools.cell.isempty_cell(Split));
                cooname{I}  = Pair{1};
                if (numel(Pair)>1) 
                    projtype{I} = Pair{2};
                    if (numel(Pair)>2)   % For e.g., TAN-SIP. Combine both
                        projtype{I} = [Pair{2} '-' Pair{3}];
                    end    
                end
                coounit{I} = CunitDict.searchAlt(cooname{I});
            end
            
            % remove cells with no projtype, e.g. velocity
            projtype = projtype(~tools.cell.isempty_cell(projtype)); 
            
            if isempty(projtype)                        % No projection given
                Obj.ProjType  = 'none';
                Obj.ProjClass = 'none';
            elseif all(strcmp(projtype{1},projtype))
                Obj.ProjType = projtype{1};
                Obj.ProjClass = ProjTypeDict.searchAlt(Obj.ProjType);
            else 
                error(' Not all Axis are with the same projection');
            end
            
            Obj.CooName = cooname;
            
            
            % if CUNIT is not given in the header, fill from dictionary
            Funit = tools.cell.isnan_cell(Obj.CUNIT);
            Obj.CUNIT(Funit) = coounit(Funit);
            
        end
        
   %======== Functions for related to xy2sky =========
   
        function [Alpha, Delta]  = xy2sky(Obj,PX,PY,OutUnits)
        % Convert X/Y pixel coordinates to celestial coordinates
        % Description: Convert X/Y pixel coordinates to celestial
        %              coordinates.
        % Input  : - A single element AstroWCS object
        %          - A matrix of pixel X coordinates.
        %            If next argument is not provided then this is a
        %            two or more column matrix of [PX,PY,...]
        %          - A matrix of pixel Y coordinates.
        %          -  OutUnits 
        % Output : - A two column matrix of [RA,Dec](e.g. [Alpha, Delta]) or a matrix of RA
        %            coordinates.
        %          - A matrix of Dec coordinates.
        %            If not asked for, then the first output will be a
        %            two column matrix.

            if nargin<4
                OutUnits = 'deg';
                if nargin<3
                    PY = [];
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            % pixel to intermediate (in units of CUNIT); including distortion
            [Xd,Yd] = Obj.pix2interm(PX,PY);
            
            % intermediate to native
            [Phi,Theta] = Obj.interm2native(Xd,Yd,Obj.CUNIT{1},'rad');
            
            % native to celestial 
            [Alpha, Delta] = Obj.native2celestial(Phi,Theta,'rad',OutUnits);
    
        end
       
        
        
        function [X,Y]=pix2interm(Obj,PX,PY)
        % Convert pixel coordinates (P) to intermediate coordinates (X) -
        % ADDED DISTORTION
        % Input  : - A single element AstroWCS object
        %          - A matrix of pixel X coordinate.
        %            If next argument is not provided then this is a
        %            two column matrix of [PX,PY].
        %          - A matrix of pixel Y coordinate.
        % Output : - A matrix of X intermediate coordinate.
        %          - A matrix of Y intermediate coordinate.
        %            The intermediate coordinates units are specified in
        %            CUNIT.
        % Example: [X,Y]=pix2interm(Obj,1,1);

            if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if nargin<3
                PY = [];
            end

            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end
            
            P = [PX(:), PY(:)].';
            relP = (P - Obj.CRPIX(:));
            
            % distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip')
                
                relPX = relP(1,:);
                relPY = relP(2,:);
                
                [relPX,relPY]  = AstroWCS.forwardDistortion(Obj.PV,relPX,relPY);
                
                relP = [relPX ; relPY]; % this is already u+g(u,v) and v+g(u,v)
            end

            
            if Obj.NAXIS~=size(P,2) && Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axes and CD matrix');
            end

            XY = (Obj.CD*relP).';

            X = reshape(XY(:,1),size(PX));
            Y = reshape(XY(:,2),size(PY));
            
            % distorion for TPV
            if strcmpi(Obj.ProjType,'tpv')
                
                R = sqrt(X.^2 + Y.^2);
                [X,Y]  = AstroWCS.forwardDistortion(Obj.PV,X,Y,R);

            end

        end
        
        
        function [Phi,Theta]=interm2native(Obj,X,Y,InUnits,OutUnits)
            % project coordinates: intermediate to native
            % Input  : - A AstroWCS object
            %          - A matrix of intermediate X coordinate.
            %            If next argument (Y) is not orovided then this
            %            is a two column matrix of [X,Y].
            %          - A matrix of intermediate Y coordinate. 
            %          - Input intermediate coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            %          - Output native coordinates units. Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Example: [Phi,Theta]=interm2native(Obj,100,100)

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Y = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % convert to deg
            ConvFactor = convert.angular(InUnits,'deg');
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;


            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case {'tan','tpv','tan-sip'}
                            Rtheta = sqrt(X.^2 + Y.^2);        % deg
                            Theta  = atan(180./(pi.*Rtheta));  % rad
                            Phi    = atan2(X,-Y);              % rad

                        otherwise

                            error('Unsupported projection option (%s)',Obj.ProjType);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end

            ConvFactor = convert.angular('rad',OutUnits);
            Theta = Theta.*ConvFactor;
            Phi   = Phi.*ConvFactor;


        end
        
    
        function [Alpha,Delta]=native2celestial(Obj,Phi,Theta,InUnits,OutUnits)
            % convert native coordinates to celestial coordinates
            % Description: Convert spherical native coordinates
            %              (phi, theta) to spherical celestial
            %              coordinates (alpha, delta).
            % Input  : - A single element AstroWCS object.
            %          - A matrix of phi (native) coordinates.
            %            If the next input argument is empty, then this is
            %            a two column matrix of [phi,theta] coordinates.
            %          - A matrix of Theta (native) coordinates.
            %          - Input native coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            %          - Output celestial coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            % Output : - A matrix of celestial (Alpha) coordinates.
            %          - A matrix of celestial (Delta) coordinates.
            % Example: [Alpha,Delta]=native2celestial(Obj,[1 1],[2 2])

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Theta = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            Units = Obj.CUNIT{1};

            % input/output units
            if ~strcmp(Obj.CUNIT{1},Obj.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Alpha,Delta]=Obj.phitheta2alphadelta(Phi.*ConvFactorIn,Theta.*ConvFactorIn,...
                                Obj.PhiP.*ConvFactorW,Obj.AlphaP.*ConvFactorW,Obj.DeltaP.*ConvFactorW,'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Alpha = Alpha.*ConvFactor;
            Delta = Delta.*ConvFactor;


        end    
        
        
   %======== Functions for related to sky2xy =========   
   
        function [PX,PY]  = sky2xy(Obj,Alpha,Delta,InUnits)
            % convert celestial coordinates to pixel coordinates
            % Description: Convert celestial coordinates that are in the
            %              reference frmae of CTYPE, RADESYS and EQUNOX,
            %              to pixel [X,Y] coordinates.
            % Input  : - A single element wcsCl object
            %          - Either longitude, or a two column matrix of
            %            [Lognitude, Latitude].
            %            This can also be a sexagesimal string or a cell
            %            array of longitude strings.
            %          - Either latitude, or empty. If empty, then assume
            %            that the previous argument contains [Long,Lat].
            %            This can also be a string or cell arraey of
            %            latitude sexagesimals.
            %          - Input coordinates units {'rad'|'deg'}
            %            Default is 'deg'
            % Output : * Pixel coordinates. If one input argument then this
            %            is a two column matrix of pixel coordinates [X,Y].
            %            If two arguments, then these are X and Y,
            %            respectively.
            % Example: [X,Y] = coo2xy(Obj,100,10,'deg')

            if nargin<4
                InUnits = 'deg';
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Alpha)
                Alpha = Delta(:,2);
                Delta = Delta(:,1);
            end

            if (iscell(Delta) || ischar(Delta)) && (iscell(Alpha) || ischar(Alpha))
                Delta = celestial.coo.convertdms(Delta,'gH','r');
                Alpha = celestial.coo.convertdms(Alpha,'gD','R');
                InUnits = 'rad';
            end


            % celestial to native
            [Phi,Theta] = Obj.celestial2native(Alpha(:),Delta(:),InUnits,'rad');
            
            % native to intermediate
            [X,Y] = Obj.native2interm(Phi, Theta,'rad',Obj.CUNIT{1});
            
%             % inverse distorsion for TPV and SIP
%             switch lower(Obj.ProjType)
%                 case 'tpv'
%                     % for TPV, invert numerically, iteratively, hopefully
%                     %  converging
%                     
%                     err_thresh = 1e-6;
%                     max_iters = 10;
%                     
%                     [X,Y]  = AstroWCS.backwardDistortion(Obj.PV,X,Y,err_thresh,max_iters);
%                     
%                 case 'tan-sip'
%                     'not implemented yet'
%                 otherwise
%                     % do nothing
%             end

            % Intermediate to pixel
            [PX,PY] = Obj.interm2pix(X,Y);

            PX = reshape(PX,size(Delta));
            PY = reshape(PY,size(Alpha));
        end      
        
        function [Phi,Theta]=celestial2native(Obj,Alpha,Delta,InUnits,OutUnits)
            % convert celestial coordinates to native coordinates
            % Description: Convert sphericall celestial coordinates
            %              (alpha, delta) or cosine directions to native
            %              spherical coordinates.
            % Input  : - A single element AstroWCS object.
            %          - A matrix of longiudes (Alpha).
            %            If the next input argument (matrix of latitude)
            %            is empty, then this can be a two column matrix of
            %            [alpha,delta] coordinates
            %            or a three column matrix of cosine directions.
            %          - A matrix of latitudes (Delta).
            %          - Input celestial coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            %            If input coordinates are in cosine direction, then
            %            this argument is ignored.
            %          - Output native coordinate units {'rad'|'deg'}
            %            Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Example: [Phi,Theta]=celestial2native(W,[1 1],[2 2])

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Delta = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            Units = Obj.CUNIT{1}; % units of properties in the wcsCl class
            if isempty(Delta)
                if size(Alpha,2)==3
                    % cosine direction
                    [Alpha, Delta] = celestial.coo.cosined2coo(Alpha(:,1),Alpha(:,2),Alpha(:,3));
                    % output is radians - override InUnits
                    InUnits = 'rad';
                elseif size(Alpha,2)==2
                    % do nothing
                    Delta = Alpha(:,2);
                    Alpha = Alpha(:,1);
                else
                    error('Celestial coordinates matrix must have 2 or 3 columns');
                end
            end

            % input/output units
            if ~strcmp(Obj.CUNIT{1},Obj.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            % convert to radians
            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Phi,Theta] = AstroWCS.alphadelta2phitheta(Alpha.*ConvFactorIn,Delta.*ConvFactorIn,...
                                    Obj.PhiP.*ConvFactorW, Obj.AlphaP.*ConvFactorW, Obj.DeltaP.*ConvFactorW, 'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;

        end
   
        function [X,Y]=native2interm(Obj,Phi,Theta,InUnits,OutUnits)
            % project coordinates: native to intermediate
            % Input  : - A AstroWCS object
            %          - A matrix of native Phi coordinate.
            %            If the next input argument (Theta) is not provided
            %            then this is a two column matrix of [Phi,Theta]
            %            native coordinates.
            %          - A matrix of native Theta coordinate.
            %          - Input native coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            % Output : - A matrix of X intermediate pixel coordinate.
            %          - A matrix of Y intermediate pixel coordinate.
            % Example: [X,Y]=native2interm(Obj,100,100)

            
            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Theta = [];
                    end
                end
            end

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            % convert native coordinates to radians
            ConvFactor = convert.angular(InUnits,'rad');
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;


            % phi,theta -> X,Y

            switch lower(Obj.ProjClass)
                case 'zenithal'

                    switch lower(Obj.ProjType)
                        case {'tan','tpv','tan-sip'}
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',Obj.ProjType);
                    end

                otherwise
                    error('Unsupported projection class (%s)',Obj.ProjClass);
            end
            
            
            ConvFactor = convert.angular('deg',OutUnits);
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;
            

        end    
        
        function [PX,PY]=interm2pix(Obj,X,Y)
            % Convert intermediate pixel coordinates to pixel coordinates
            % Input  : - A single element AstroWCS object.
            %          - A matrix of X intermediate pixel coordinate.
            %            If next argument (Y) is not provided then this is
            %            a two column matrix of [X,Y].
            %          - A matrix of Y intermeditae pixel coordinate.
            % Output : - A matrix of X pixel coordinate.
            %          - A matrix of Y pixel coordinate.
            % Example: [P1,P2]=interm2pix(Obj,1,1)

             if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axss and CD matrix');
            end

            if nargin<3
                Y = [];
            end
            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end
            
            % inverse distorion for TPV
            if strcmpi(Obj.ProjType,'tpv')
                
                err_thresh = 1e-6;
                max_iters = 10;
                    
                [X,Y]  = AstroWCS.backwardDistortion(Obj.PV,X,Y,err_thresh,max_iters);
            end
      
            
            XY = [X(:), Y(:)];
            
            relP = (Obj.CD) \ XY.';
            
            % inverse distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip')
                
                err_thresh = 1e-9;
                max_iters = 10;
                
                relPX = relP(1,:);
                relPY = relP(2,:);
                
                [relPX,relPY]  = AstroWCS.backwardDistortion(Obj.PV,relPX,relPY,err_thresh,max_iters);
                
                relP = [relPX ; relPY]; % this is including u+g(u,v) and v+g(u,v)
            end            
            

            
            P = (relP + Obj.CRPIX(:)).';            
            
            

            PX = reshape(P(:,1),size(X));
            PY = reshape(P(:,2),size(Y));

        end

        
    end
    
    
    methods (Static)  % static methods

   %======== Functions to construct AstroWCS from AstroHeader =========
        
        function Obj = header2wcs(Header)
            % Create and populate an AstroWCS object from an AstroHeader object

            Obj = AstroWCS(1);
            AH = Header;
            
            % Read number of axes
            % if WCSAXES is not available use NAXIS as default
            Obj.NAXIS = AH.getVal('NAXIS');
            Obj.WCSAXES = AH.getVal('WCSAXES');
            if (Obj.WCSAXES==0)
                Obj.WCSAXES = Obj.NAXIS;
            end
            
            Naxis = Obj.WCSAXES;

            % prepare mutli axis keys
            KeyCtype = cell(1,Naxis);
            KeyCunit = cell(1,Naxis);
            KeyCrpix = cell(1,Naxis);
            KeyCrval = cell(1,Naxis);
            for Iaxis1 = 1:1:Naxis
                KeyCtype{Iaxis1} = sprintf('CTYPE%d',Iaxis1);
                KeyCunit{Iaxis1} = sprintf('CUNIT%d',Iaxis1); 
                KeyCrpix{Iaxis1} = sprintf('CRPIX%d',Iaxis1);
                KeyCrval{Iaxis1} = sprintf('CRVAL%d',Iaxis1);
            end

            % Get CTYPE and transalte to projection information (ProjType,
            % ProjClass) and CooName and CUNIT
            Obj.CTYPE = AH.getCellKey(KeyCtype);
            Obj.CUNIT = AH.getCellKey(KeyCunit);                
            Obj.read_ctype;
            
            [Obj.RADESYS,Obj.EQUINOX] = Obj.read_radesys_equinox(AH);
            % Get base WCS info

            if AH.isKeyExist('LONPOLE')
                Obj.LONPOLE = AH.getVal('LONPOLE');
            end
            if AH.isKeyExist('LATPOLE')
                Obj.LATPOLE = AH.getVal('LATPOLE');
            end            

            
            Obj.CRPIX = cell2mat(AH.getCellKey(KeyCrpix));
            Obj.CRVAL = cell2mat(AH.getCellKey(KeyCrval));
            
            Obj.CD = Obj.build_CD(AH,Naxis);
            
            % Read distortions   
            
            % look for PV coeficients
            Obj.PV = Obj.build_PV(AH,Obj.ProjType);


             % populate proj Meta
            Obj.populate_projMeta;
        end
        
        function [radesys,equinox] =read_radesys_equinox(Header)
            
            AH = Header;
            
            radesys = AH.getVal({'RADESYS','RADECSYS'});
            equinox = AH.getVal('EQUINOX');
            
            % cases where radesys or equinox are not given
            if any(isnan(radesys)) && ~isnan(equinox) % no radesys
                
                if equinox < 1984.0
                    radesys ='FK4';
                else
                    radesys ='FK5';
                end
                
            elseif ~any(isnan(radesys)) && isnan(equinox) % no equinox
                
                switch radesys
                    case{'FK4','FK4-NO-E'}
                        equinox = 1950.0;
                    case 'FK5'
                        equinox = 2000.0;
                    otherwise
                        equinox = [];
                end
                             
            elseif any(isnan(radesys)) && isnan(equinox) % no radesys nor equinox
                radesys = 'ICRS';
                equinox = 2000.0;
                
            end
               
            
        end

        
        function CD = build_CD(Header,Naxis)
            % Read The CD matrix, or PC+CDELT, or CDELT
            
            AH = Header;
            
            KeysCD = cell(1,Naxis.^2);
            KeyCdelt = cell(1,Naxis);
            KeysPC = cell(1,Naxis.^2);
            K = 0;
            for Iaxis1 = 1:1:Naxis
               KeyCdelt{Iaxis1} = sprintf('CDELT%d',Iaxis1);
               for Iaxis2=1:1:Naxis
                   K = K + 1;
                   KeysCD{K} = sprintf('CD%d_%d',Iaxis1,Iaxis2);
                   KeysPC{K} = sprintf('PC%d_%d',Iaxis1,Iaxis2);
               end 
            end
            
            % try to get CD matrix
            ValCD = AH.getCellKey(KeysCD);
            K = 0;
            CD = nan(Naxis,Naxis);
            for Iaxis1=1:1:Naxis
                for Iaxis2=1:1:Naxis
                    K = K + 1;
                    CD(Iaxis1,Iaxis2) = ValCD{K};
                end
            end

            % treat cases in which not all CD keywords are provided -
            % fill with zeros.
            if (any(isnan(CD(:))) && ~all(isnan(CD(:))))
                CD(isnan(CD)) = 0;
            end
            
            % treat cases in which CD matrix is not provided
            if all(isnan(CD(:)))
                ValCdelt = cell2mat(AH.getCellKey(KeyCdelt));
                ValPC = cell2mat(AH.getCellKey(KeysPC));
                
                if ~all(isnan(ValPC))  && ~any(isnan(ValCdelt))   % Both PC and CDELT are provided
                    
                    ValPC(isnan(ValPC)) = 0; % treat cases in which not all PC keywords are provided - fill with zeros  
                    K = 0;
                    for Iaxis1=1:1:Naxis
                        for Iaxis2=1:1:Naxis
                            K = K + 1;
                            CD(Iaxis1,Iaxis2) = ValPC(K) * ValCdelt(Iaxis1);
                        end
                    end           
                                   
                elseif   ~any(isnan(ValCdelt)) % only CDELT is provided 
                    CD = diag(ValCdelt);
                    
                else                      
                    error('no info for CD matrix');
                end
            end
            
        end        
        
        function PV = build_PV(Header,ProjType)
            % Read PV coefficients, if any Coeff X_power Y_power R_power
            
             switch lower(ProjType)
                case 'none'
                    PV = [];
                case 'tan'
                    PV = [];
                case 'tpv'
                    PV = AstroWCS.build_TPV(Header);
                case 'tan-sip'                    
                    PV = AstroWCS.build_TANSIP(Header);
                case 'zpn'                    
                    error('Need to add ZPN - TODO');  
                otherwise
                    error('Unsupported projection type (%s)',ProjType);
             end 
            
        end
        
        function PV = build_TPV(Header)
            
            AH = Header;
           
            PolyTPVtable = AstroWCS.polyTPVdef();
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV1_\d+','match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV2_\d+','match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if  NPV1 || NPV2
                
                PV = zeros(2,max(NPV1,NPV2),4);
                
                PV1_Names = AH.Data(FlagMatchPV1,1);
                PV1_Vals = cell2mat(AH.Data(FlagMatchPV1,2));
                
                for I1 = 1:1:NPV1
                    currPV = PolyTPVtable(PV1_Names(I1),:);
                    if ~currPV.Axis==1
                        error('wrong axis');
                    end
                    PV(currPV.Axis,I1,1:4) = [PV1_Vals(I1) currPV.xi_power currPV.eta_power currPV.r_power];
                end
                
                PV2_Names = AH.Data(FlagMatchPV2,1);
                PV2_Vals = cell2mat(AH.Data(FlagMatchPV2,2));                
                
                for I2 = 1:1:NPV2
                    currPV = PolyTPVtable(PV2_Names(I2),:);
                    if ~currPV.Axis==2
                        error('wrong axis');
                    end
                    PV(currPV.Axis,I2,1:4) = [PV2_Vals(I2) currPV.xi_power currPV.eta_power currPV.r_power];
                end
                
                % if no r_power poly, trancate last level
                if sum(sum(PV(:,:,4)))==0
                    PV = PV(:,:,1:3);
                end

            else
                PV = [];
            end            
            
        end
        
        function PolyTPVtable=polyTPVdef()
            % return a table TPV polynomial definition
            % Output : - A matrix of [Axis, Term, xi_power, eta_power, r_power]

            ColNames = {'Axis' 'Term' 'xi_power' 'eta_power' 'r_power'};
            PolyNames ={'PV1_0' 'PV1_1' 'PV1_2' 'PV1_3' 'PV1_4' 'PV1_5' ...
                        'PV1_6' 'PV1_7' 'PV1_8' 'PV1_9' 'PV1_10' 'PV1_11'...
                        'PV1_12' 'PV1_13' 'PV1_14' 'PV1_15' 'PV1_16'...
                        'PV1_17' 'PV1_18' 'PV1_19' 'PV1_20' 'PV1_21'...
                        'PV1_22' 'PV1_23' 'PV1_24' 'PV1_25' 'PV1_26'...
                        'PV1_27' 'PV1_28' 'PV1_29' 'PV1_30' 'PV1_31'...
                        'PV1_32' 'PV1_33' 'PV1_34' 'PV1_35' 'PV1_36'...
                        'PV1_37' 'PV1_38' 'PV1_39'...
                        'PV2_0' 'PV2_1' 'PV2_2' 'PV2_3' 'PV2_4' 'PV2_5' ...
                        'PV2_6' 'PV2_7' 'PV2_8' 'PV2_9' 'PV2_10' 'PV2_11'...
                        'PV2_12' 'PV2_13' 'PV2_14' 'PV2_15' 'PV2_16'...
                        'PV2_17' 'PV2_18' 'PV2_19' 'PV2_20' 'PV2_21'...
                        'PV2_22' 'PV2_23' 'PV2_24' 'PV2_25' 'PV2_26'...
                        'PV2_27' 'PV2_28' 'PV2_29' 'PV2_30' 'PV2_31'...
                        'PV2_32' 'PV2_33' 'PV2_34' 'PV2_35' 'PV2_36'...
                        'PV2_37' 'PV2_38' 'PV2_39'};
            
            % polynomial mapping: [Axis, Term, xi_power, eta_power, r_power]
            PolyTPV =  [1   0     0   0  0;...
                %
                        1   1     1   0  0;...
                        1   2     0   1  0;...
                        1   3     0   0  1;...
                %
                        1   4     2   0  0;...
                        1   5     1   1  0;...
                        1   6     0   2  0;...
                %
                        1   7     3   0  0;...
                        1   8     2   1  0;...
                        1   9     1   2  0;...
                        1   10    0   3  0;...
                        1   11    0   0  3;...
                %
                        1   12    4   0  0;...
                        1   13    3   1  0;...
                        1   14    2   2  0;...
                        1   15    1   3  0;...
                        1   16    0   4  0;...
                %
                        1   17    5   0  0;...
                        1   18    4   1  0;...
                        1   19    3   2  0;...
                        1   20    2   3  0;...
                        1   21    1   4  0;...
                        1   22    0   5  0;...
                        1   23    0   0  5;...
                %
                        1   24    6   0  0;...
                        1   25    5   1  0;...
                        1   26    4   2  0;...
                        1   27    3   3  0;...
                        1   28    2   4  0;...
                        1   29    1   5  0;...
                        1   30    0   6  0;...
                %
                        1   31    7   0  0;...
                        1   32    6   1  0;...
                        1   33    5   2  0;...
                        1   34    4   3  0;...
                        1   35    3   4  0;...
                        1   36    2   5  0;...
                        1   37    1   6  0;...
                        1   38    0   7  0;...
                        1   39    0   0  7;...
                    
                        2   0     0   0  0;...
                %
                        2   1     0   1  0;...
                        2   2     1   0  0;...
                        2   3     0   0  1;...
                %
                        2   4     0   2  0;...
                        2   5     1   1  0;...
                        2   6     2   0  0;...
                %
                        2   7     0   3  0;...
                        2   8     1   2  0;...
                        2   9     2   1  0;...
                        2   10    3   0  0;...
                        2   11    0   0  3;...
                %
                        2   12    0   4  0;...
                        2   13    1   3  0;...
                        2   14    2   2  0;...
                        2   15    3   1  0;...
                        2   16    4   0  0;...
                %
                        2   17    0   5  0;...
                        2   18    1   4  0;...
                        2   19    2   3  0;...
                        2   20    3   2  0;...
                        2   21    4   1  0;...
                        2   22    5   0  0;...
                        2   23    0   0  5;...
                %
                        2   24    0   6  0;...
                        2   25    1   5  0;...
                        2   26    2   4  0;...
                        2   27    3   3  0;...
                        2   28    4   2  0;...
                        2   29    5   1  0;...
                        2   30    6   0  0;...
                %
                        2   31    0   7  0;...
                        2   32    1   6  0;...
                        2   33    2   5  0;...
                        2   34    3   4  0;...
                        2   35    4   3  0;...
                        2   36    5   2  0;...
                        2   37    6   1  0;...
                        2   38    7   0  0;...
                        2   39    0   0  7];
                    
                 PolyTPVtable = array2table(PolyTPV,'VariableNames',ColNames,'RowNames',PolyNames);

        end
 
        function PV = build_TANSIP(Header)
            
            AH = Header;
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'A_\d+_\d+','match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'B_\d+_\d+','match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if  NPV1 || NPV2
                
                PV = zeros(2,(1+max(NPV1,NPV2)),3); % to add x and y, as SIP is u+f(u,v), v+g(uv)
                
                PV1_Powers  =regexp(AH.Data(FlagMatchPV1,1), 'A_(?<u_power>\d+)\_(?<v_power>\d+)','names');
                PV1_Vals = cell2mat(AH.Data(FlagMatchPV1,2));
                
                for I1 = 1:1:NPV1
                    PV(1,I1,1:3) = [PV1_Vals(I1) str2double(PV1_Powers{I1}.u_power) str2double(PV1_Powers{I1}.v_power)];
                end
                
                PV(1,(NPV1+1),1:3) = [1, 1, 0]; %  add x as SIP is u+f(u,v)
                
                PV2_Powers  =regexp(AH.Data(FlagMatchPV2,1), 'B_(?<u_power>\d+)\_(?<v_power>\d+)','names');
                PV2_Vals = cell2mat(AH.Data(FlagMatchPV2,2));                
                
                for I2 = 1:1:NPV2
                    PV(2,I2,1:3) = [PV2_Vals(I2) str2double(PV2_Powers{I2}.u_power) str2double(PV2_Powers{I2}.v_power)];
                end
                
                PV(2,(NPV2+1),1:3) = [1, 0, 1]; % add y as SIP is v+g(u,v)
               
            else
                PV = [];
            end            
            
        end
        
   %======== Functions for related to xy2sky =========           
        
        function [Alpha,Delta]=phitheta2alphadelta(Phi,Theta,PhiP,AlphaP,DeltaP,Units)
            % convert natve coordinates (Phi,Theta) to celestila (alpha,delta)
            % Input  : - Native longitude (phi)
            %          - Native latitude (theta)
            %          - native longitude of celestial pole
            %          - Celestial longitude of native pole
            %          - Celestial latitude of native pole (DeltaP=ThetaP)
            %          - Input and output units {'deg'|'rad'}.
            %            Default is 'deg'
            % Output : - Celestial longitude
            %          - Celestial latitude
            % Example: - [Alpha,Delta]=AstroWCS.phitheta2alphadelta(1.1,1.1,0,0,0)


            RAD = 180./pi;
            if nargin<6
                Units = 'deg';
            end

            % convert to radians
            switch lower(Units)
                case 'deg'
                    Phi    = Phi./RAD;
                    Theta  = Theta./RAD;
                    PhiP   = PhiP./RAD;
                    AlphaP = AlphaP./RAD;
                    DeltaP = DeltaP./RAD;
                case 'rad'
                    % do nothing
                otherwise
                    error('Unknown Units option');
            end

            % note that the arg(x,y) function in Calabretta et al. is
            % equivalent to atan2(y,x)
            Alpha = AlphaP + atan2(-cos(Theta).*sin(Phi - PhiP),...
                                   sin(Theta).*cos(DeltaP) - cos(Theta).*sin(DeltaP).*cos(Phi - PhiP));
            Delta = asin(sin(Theta).*sin(DeltaP) + cos(Theta).*cos(DeltaP).*cos(Phi - PhiP));

            % convert radians to units
            switch lower(Units)
                case 'deg'
                    Alpha = Alpha.*RAD;
                    Delta = Delta.*RAD;
            end

        end
        
        function [Xd,Yd]  = forwardDistortion(PV,X,Y,R)
           % use PV matrix to calcualte polynomial distortion
           % PV matrix is [Naxis,n,vals]. NAxis should be 2.
           % Third axis is [Coeff, X_power, Y_Power, R_Power (optional)]
           
            if nargin<4
                R = 1;
            end
            
            if (size(PV,1) < 2) || (size(PV,3)<2)
                error('PV matrix too small');
            end

            CoefX    = PV(1,:,1);
            X_Xpower = PV(1,:,2);
            X_Ypower = PV(1,:,3);
            if (size(PV,3)) > 3
                X_Rpower = PV(1,:,4);
            else
                X_Rpower = 0;
            end

            CoefY    = PV(2,:,1);
            Y_Xpower = PV(2,:,2);
            Y_Ypower = PV(2,:,3);
            if (size(PV,3)) > 3
                Y_Rpower = PV(2,:,4);
            else
                Y_Rpower = 0;
            end                        


            Xd = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^X_Rpower(:)) );
            Yd = sum(CoefY(:) .* ((X(:).').^Y_Xpower(:) ) .* ((Y(:).').^Y_Ypower(:))  .* ((R(:).').^Y_Rpower(:)) );                

            Xd=reshape(Xd,size(X));
            Yd=reshape(Yd,size(Y));

        end
        
        
   %======== Functions for related to sky2xy =========           
        
        function [Phi,Theta]=alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP,Units)
            % Convert celestial coordinates to native coordinates
            % Input  : - Celestial longitude.
            %          - Celestial latitude.
            %          - Native longitude of the celestial pole.
            %          - Celestial longitude of the native pole.
            %          - Celestial latitude of the native pole.
            %          - Units of the input and output coordinates
            %            {'deg'|'rad'}. Default is 'deg'.
            % Output : - Native longitude
            %          - Native latitude
            % Example: [Phi,Theta]=AדארםWCS.alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP)


            RAD = 180./pi;
            if nargin<6
                Units = 'deg';
            end

            % convert to radians
            switch lower(Units)
                case 'deg'
                    Alpha  = Alpha./RAD;
                    Delta  = Delta./RAD;
                    PhiP   = PhiP./RAD;
                    AlphaP = AlphaP./RAD;
                    DeltaP = DeltaP./RAD;
                case 'rad'
                    % do nothing
                otherwise
                    error('Unknown Units option');
            end

            % note that the arg(x,y) function in Calabretta et al. is
            % equivalent to atan2(y,x)
            Phi = PhiP + atan2(-cos(Delta).*sin(Alpha - AlphaP),...
                               sin(Delta).*cos(DeltaP) - cos(Delta).*sin(DeltaP).*cos(Alpha - AlphaP));
            % Note that this is 90-angular distance...
            Theta = asin(sin(Delta).*sin(DeltaP) + cos(Delta).*cos(DeltaP).*cos(Alpha - AlphaP));

            % convert radians to units
            switch lower(Units)
                case 'deg'
                    Phi   = Phi.*RAD;
                    Theta = Theta.*RAD;
            end

        end
        
        function [X,Y]  = backwardDistortion(PV,Xd,Yd,err_thresh,max_iters)
            % iterativly calculate [X,Y} from [Xd,Yd] using PV
    
            if nargin<5
                max_iters = 100;
                if nargin < 4
                    err_thresh = 1e-7;
                end    
            end
            
            niter=0;
            err=Inf;
            X0=Xd;
            Y0=Yd;
            while (niter < max_iters) && (err > err_thresh) % arbitrary iteration stop
                if size(PV,3)>2
                    R = sqrt(X0.^2 + Y0.^2); % TODO - change to arbitrary function f(x,y)
                else
                    R=1;
                end
                    
                
                [X1,Y1] = AstroWCS.forwardDistortion(PV,X0,Y0,R);
                X0= (X0-X1) +Xd;
                Y0= (Y0-Y1) +Yd;
                err=sqrt(sum((X1(:)-Xd(:)).^2 + (Y1(:)-Yd(:)).^2));
                niter=niter+1;
            end
            X=X0;
            Y=Y0;


        end
         
        
    end
    
    
   %==================OLD FUNCTIONS=================================
       
    
    
    methods
        function Obj=fill(Obj,Force)
            % Fill ProjType, ProjClass, Coo, AlphaP, DeltaP in wcsCl object
            % Package: @wcsCl (basic)
            % Input  : - A wcsCl object
            %          - A logical indicating if to re-fill the values even
            %            if exist. Default is true.
            % Output : - A wcsCl object
            % Example: Obj.fill

            if nargin<2
                Force = true;
            end

            Nw = numel(Obj);
            for Iw=1:1:Nw
                if Force || isempty(Obj(Iw).ProjType) || isempty(Obj(Iw).ProjClass) || isnan(Obj(Iw).AlphaP) || isnan(Obj(Iw).DeltaP)
                    % fill missing values

                    N = numel(Obj(Iw).CTYPE);
                    Coo  = cell(1,N);
                    Proj = cell(1,N);
                    for I=1:1:N
                        if isempty(Obj(Iw).CTYPE{I})
                            Proj{I} = 'none';
                            Coo{I}  = 'unknown';
                        else

                            Coo{I} = strrep(Obj(Iw).CTYPE{I}(1:4),'-','');
                            if numel(Obj(Iw).CTYPE{I})<5
                                Proj{I} = 'none';
                            else
                                Proj{I} = strrep(Obj(Iw).CTYPE{I}(5:end),'-','');
                            end

                            if I>1
                                if ~(strcmp(Proj{I},Proj{1}) || strcmp(Proj{I},'none'))
                                    error('Projection of all axes must be the same unless none');
                                end
                            end
                        end
                    end

                    Obj(Iw).ProjType  = Proj{1};
                    Obj(Iw).CooName   = Coo;
                    Obj(Iw).ProjClass = wcsCl.classify_projection(Proj{1});


        %                     if ~(strcmp(Obj(Iw).CTYPE{1}(1:2),'RA') || strcmp(Obj(Iw).CTYPE{1}(3:4),'LON'))
        %                         error('Support only cases in which CTYPE1 contains longitudes');
        %                     end
        %                     if ~(strcmp(Obj(Iw).CTYPE{2}(1:3),'DEC') || strcmp(Obj(Iw).CTYPE{2}(3:4),'LAT'))
        %                         error('Support only cases in which CTYPE2 contains latitude');
        %                     end

                     switch lower(Obj(Iw).ProjClass)
                        case 'none'
                            Obj(Iw).Alpha0 = Obj(Iw).CRVAL(1);
                            Obj(Iw).Delta0 = Obj(Iw).CRVAL(2);
                            Obj(Iw).AlphaP = Obj(Iw).CRVAL(1);
                            Obj(Iw).DeltaP = Obj(Iw).CRVAL(2);

                            Obj(Iw).Phi0   = NaN;
                            Obj(Iw).Theta0 = NaN;
                            Obj(Iw).PhiP   = NaN;

                        case 'zenithal'

                            Obj(Iw).Alpha0 = Obj(Iw).CRVAL(1);
                            Obj(Iw).Delta0 = Obj(Iw).CRVAL(2);
                            Obj(Iw).AlphaP = Obj(Iw).CRVAL(1);
                            Obj(Iw).DeltaP = Obj(Iw).CRVAL(2);

                            Units = strtrim(Obj(Iw).CUNIT{1});
                            ConvFactor = convert.angular('deg',Units);

                            Obj(Iw).Phi0   = 0.*ConvFactor;
                            Obj(Iw).Theta0 = 90.*ConvFactor;

                            if Obj(Iw).Delta0>=Obj(Iw).Theta0
                                Obj(Iw).PhiP = 0.*ConvFactor;
                            else
                                Obj(Iw).PhiP = 180.*ConvFactor;
                            end

                        otherwise
                            error('Unsupported projection class (%s)',Obj(Iw).ProjClass);
                     end

                end

            end

        end

        
        function Obj=OLD_populate_projMeta(Obj)
            % populate projection and pole information in a wcsCl object
            % Package: @wcsCl
            % Description: 
            % Input  : - A wcsCl object
            % Output : - Obj wcsCl object with the projection type and pole data populated.

            Def.LONPOLE = 0;
            Def.LATPOLE = 90;

            N = numel(Obj);
            for I=1:1:N

                ProjAlgo  = Obj(I).CTYPE{1}(6:end);
                ProjClass = wcsCl.classify_projection(ProjAlgo);
                Obj(I).ProjType  = ProjAlgo;
                Obj(I).ProjClass = ProjClass;
                switch lower(ProjClass)
                    case 'zenithal'

                        Alpha0 = Obj(I).CRVAL(1);
                        Delta0 = Obj(I).CRVAL(2);
                        AlphaP = Obj(I).CRVAL(1);
                        DeltaP = Obj(I).CRVAL(2);

                        Phi0   = 0;
                        Theta0 = 90;

                        if Delta0>=Theta0
                            PhiP = 0;
                        else
                            PhiP = 180;
                        end

                        Obj(I).Alpha0 = Alpha0;
                        Obj(I).Delta0 = Delta0;
                        Obj(I).AlphaP = AlphaP;
                        Obj(I).DeltaP = DeltaP;
                        Obj(I).Phi0   = Phi0;
                        Obj(I).Theta0 = Theta0;
                        Obj(I).PhiP   = PhiP;
                    otherwise
                        error('Unsupported projection class (%s)',ProjClass);
                end



                switch lower(ProjAlgo)
                    case {'tan'}

                        % treat LONPOLE
                        if isempty(Obj(I).LONPOLE)
                            % check if LONPOLE is in PV1_3
                            if isempty(Obj(I).PV)
                                % set to default value
                                Obj(I).LONPOLE = Def.LONPOLE;
                            else
                                if numel(Obj(I).PV{1})>=3
                                    Obj(I).LONPOLE = Obj(I).PV{1}{3};
                                else
                                    % set to default value
                                    Obj(I).LONPOLE = Def.LONPOLE;
                                end
                            end
                        end

                        % treat LATPOLE
                        if isempty(Obj(I).LATPOLE)
                            % check if LATPOLE is in PV1_4
                            if isempty(Obj(I).PV)
                                % set to default value
                                Obj(I).LATPOLE = Def.LATPOLE;
                            else
                                if numel(Obj(I).PV{1})>=4
                                    Obj(I).LATPOLE = InPar.PV{1}{4};
                                else
                                    % set to default value
                                    Obj(I).LATPOLE = Def.LATPOLE;
                                end
                            end
                        end

                    case 'tpv'

                    otherwise
                end
            end
        end
        
        function [Xd,Yd] = interm2distortedInterm(Obj,X,Y)

            if numel(Obj)~=1
                error('Input must be a single element wcsCl object');
            end

           
            switch lower(Obj.ProjType)
                case {'tpv','tan-sip'}
                    R = sqrt(X.^2 + Y.^2);
                    
                    CoefX    = Obj.PV(1,:,1);
                    X_Xpower = Obj.PV(1,:,2);
                    X_Ypower = Obj.PV(1,:,3);
                    if (size(Obj.PV,3)) > 3
                        X_Rpower = Obj.PV(1,:,4);
                    else
                        X_Rpower = 0;
                    end
                    
                    CoefY    = Obj.PV(2,:,1);
                    Y_Xpower = Obj.PV(2,:,2);
                    Y_Ypower = Obj.PV(2,:,3);
                    if (size(Obj.PV,3)) > 3
                        Y_Rpower = Obj.PV(2,:,4);
                    else
                        Y_Rpower = 0;
                    end                        
                    
                    
                    Xd = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^X_Rpower(:)) );
                    Yd = sum(CoefY(:) .* ((X(:).').^Y_Xpower(:) ) .* ((Y(:).').^Y_Ypower(:))  .* ((R(:).').^Y_Rpower(:)) );

                case 'zpn'
                    'not implemented yet'
                otherwise % no distortion
                    Xd = X;
                    Yd = Y;
            end
            
            Xd=reshape(Xd,size(X));
            Yd=reshape(Yd,size(Y));
            
        end
        
    end

    %======================================================================
    
    methods (Static)
        

    
        

        


        % change name to: sky2xy
        function varargout=coo2xy(Obj,Lon,Lat,Units)
            % convert celestial coordinates to pixel coordinates
            % Package: @wcsCl (transformation)
            % Description: Convertt celestial coordinates that are in the
            %              reference frmae of CTYPE, RADESYS and EQUNOX,
            %              to pixel [X,Y] coordinates.
            % Input  : - A single element wcsCl object
            %          - Either longitude, or a two column matrix of
            %            [Lognitude, Latitude].
            %            This can also be a sexagesimal string or a cell
            %            array of longitude strings.
            %          - Either latitude, or empty. If empty, then assume
            %            that the previous argument contains [Long,Lat].
            %            This can also be a string or cell arraey of
            %            latitude sexagesimals.
            %          - Input coordinates units {'rad'|'deg'}
            %            Default is 'deg'
            % Output : * Pixel coordinates. If one input argument then this
            %            is a two column matrix of pixel coordinates [X,Y].
            %            If two arguments, then these are X and Y,
            %            respectively.
            % Example: [X,Y] = coo2xy(Obj,100,10,'deg')

            if nargin<4
                Units = 'deg';
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Lat)
                Lat = Lon(:,2);
                Lon = Lon(:,1);
            end

            if (iscell(Lon) || ischar(Lon)) && (iscell(Lat) || ischar(Lat))
                Lon = celestial.coo.convertdms(Lon,'gH','r');
                Lat = celestial.coo.convertdms(Lat,'gD','R');
                Units = 'rad';
            end


            % celestial to native
            [Phi,Theta] = celestial2native(Obj,[Lon(:),Lat(:)],[],Units,'deg');
            % native to intermediate
            [X,Y] = native2interm(Obj,[Phi, Theta],[],'deg');
            % inverse distorsion for TPV and SIP
            switch lower(Obj.ProjType)
                case 'tpv'
                    % for TPV, invert numerically, iteratively, hopefully
                    %  converging
                    niter=0;
                    err=Inf;
                    X0=X;
                    Y0=Y;
                    while niter<10 && err > 1e-6 % arbitrary iteration stop
                       [X1,Y1] = interm2TPVdistortedInterm(Obj,X0,Y0);
                       X0=X0-X1+X;
                       Y0=Y0-Y1+Y;
                       err=sqrt(sum((X1(:)-X(:)).^2 + (Y1(:)-Y(:)).^2));
                       niter=niter+1;
                    end
                    X=X0;
                    Y=Y0;
                case 'tan-sip'
                    'not implemented yet'
                otherwise
                    % do nothing
            end

            % Intermediate to pixel
            [PX,PY] = interm2pix(Obj,[X,Y],[]);

            PX = reshape(PX,size(Lon));
            PY = reshape(PY,size(Lat));

            if nargout>1
                varargout{1} = PX;
                varargout{2} = PY;
            else
                varargout{1} = [PX,PY];
            end

        end      
        
        
        


 
        
    





    end
    
    methods



        % FFU
        function [X,Y]     = grid_coo2xy(gridCoo,gridLon,gridLat,varargin)

            error('bug')

            % convert XY coo to Long/Lat using a matrix grid
            % Package: @wcsCl (Static, basic mapping functions)
            % Description: given a matrix/cube of Lon/Lat grid coordinates
            %              as a function of X,Y and additional; e.g.,
            %              color) coordinaes, convert X,Y,color to Lon,Lat.
            % Input  : - (gridCoo) Either a cell array in which each
            %            element contains the X/Y/color coordinates of a
            %            grid, or a matrix in which each column represent
            %            the grid coordinates.
            %            Alternatively, this can be a row vector in which
            %            is element is the coordinate of the end of the
            %            grid array and assuming the array start at 1.
            %          - A grid of longitudes. This is a matrix, or a cube
            %            in which the number of dimensions is equal to the
            %            number of columns/elements in gridCoo.
            %            This cube contains the value of longitude at the
            %            specified grid points.
            %          - A grid of latitudes (the same as the previous
            %            argument, but for the latitude).
            %          * Argument per axes (e.g., x,y,color) for which to
            %            interpolate the lon/lat.
            %          * Additional parameters to pass to interpn.
            %            (e.g., 'makima').
            %            Default is 'linear'.
            % Output : - Interpolated longitudes.
            %            Return NaN if out of grid.
            %          - Interpolated latitudes.
            % Example: gridCoo = [1024 1024 3]; gridLon=rand(10,10,3);
            %          [X,Y]=wcsCl.grid_xy2coo(gridCoo,gridLon,gridLon,2,2,1)
            %          % in 2D
            %          [gridLon,gridLat] = meshgrid((1:1:10),(1:1:10));
            %          [X,Y]=wcsCl.grid_coo2xy([1024 1024],gridLon,gridLat,2,2)

            if isnumeric(gridCoo)
                Ncol = size(gridCoo,2);
                if size(gridCoo,1)==1
                    % a row vector
                    SizeGrid = size(gridLon);
                    Step = (gridCoo-1)./(SizeGrid-1);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = (1:Step(Icol):gridCoo(Icol)).';
                    end

                else

                    % convert gridCoo to cell array

                    Tmp  = cell(1,Ncol);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = gridCoo(:,Icol);
                    end
                end
                gridCoo = Tmp;
            end

            Naxes = numel(gridCoo);

            if numel(varargin)<Naxes
                error('Number of requested coordinates is smaller than the dimension of the grid');
            else
                if numel(varargin)==Naxes
                    % use default interpolation
                    varargin{end+1} = 'linear'; %'makima';
                end
            end

            [Mat{1:1:Naxes}] = ngrid(gridCoo{:});

            [X] = interpn(gridLon,gridLat,Mat{1},varargin{1:Naxes},varargin{Naxes+1:end});
            [Y] = interpn(gridLon,gridLat,Mat{2},varargin{1:Naxes},varargin{Naxes+1:end});

        end

        % FFU
        function [Lon,Lat] = grid_xy2coo(gridCoo,gridLon,gridLat,varargin)
            % convert XY coo to Long/Lat using a matrix grid
            % Package: @wcsCl (Static, basic mapping functions)
            % Description: given a matrix/cube of Lon/Lat grid coordinates
            %              as a function of X,Y and additional; e.g.,
            %              color) coordinaes, convert X,Y,color to Lon,Lat.
            % Input  : - (gridCoo) Either a cell array in which each
            %            element contains the X/Y/color coordinates of a
            %            grid, or a matrix in which each column represent
            %            the grid coordinates.
            %            Alternatively, this can be a row vector in which
            %            is element is the coordinate of the end of the
            %            grid array and assuming the array start at 1.
            %          - A grid of longitudes. This is a matrix, or a cube
            %            in which the number of dimensions is equal to the
            %            number of columns/elements in gridCoo.
            %            This cube contains the value of longitude at the
            %            specified grid points.
            %          - A grid of latitudes (the same as the previous
            %            argument, but for the latitude).
            %          * Argument per axes (e.g., x,y,color) for which to
            %            interpolate the lon/lat.
            %          * Additional parameters to pass to interpn.
            %            (e.g., 'makima').
            %            Default is 'linear'.
            % Output : - Interpolated longitudes.
            %            Return NaN if out of grid.
            %          - Interpolated latitudes.
            % Example: gridCoo = [1024 1024 3]; gridLon=rand(10,10,3);
            %          [Lon,Lat]=wcsCl.grid_xy2coo(gridCoo,gridLon,gridLon,2,2,1)
            %          % in 2D
            %          [gridLon,gridLat] = meshgrid((1:1:10),(1:1:10));
            %          [Lon,Lat]=wcsCl.grid_xy2coo([1024 1024],gridLon,gridLat,2,2)

            if isnumeric(gridCoo)
                Ncol = size(gridCoo,2);
                if size(gridCoo,1)==1
                    % a row vector
                    SizeGrid = size(gridLon);
                    Step = (gridCoo-1)./(SizeGrid-1);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = (1:Step(Icol):gridCoo(Icol)).';
                    end

                else

                    % convert gridCoo to cell array

                    Tmp  = cell(1,Ncol);
                    for Icol=1:1:Ncol
                        Tmp{Icol} = gridCoo(:,Icol);
                    end
                end
                gridCoo = Tmp;
            end

            Naxes = numel(gridCoo);

            if numel(varargin)<Naxes
                error('Number of requested coordinates is smaller than the dimension of the grid');
            else
                if numel(varargin)==Naxes
                    % use default interpolation
                    varargin{end+1} = 'linear'; %'makima';
                end
            end
            Lon = interpn(gridCoo{:},gridLon,varargin{1:Naxes},varargin{Naxes+1:end});
            Lat = interpn(gridCoo{:},gridLat,varargin{1:Naxes},varargin{Naxes+1:end});

        end









        % delete
        function Ans=isPopulated(Obj)
            % Check if wcsCl object Exist field is false (i.e., no WCS in object)
            % Package: @wcsCl (basic)
            
            Ans = [Obj.Exist];
        end
 
        
        % delete
        function Ans=iswcsCl(Obj)
        % Check if object is a wcsCl object 
        % Package: @wcsCl (basic)
            Ans = isa(Obj,'wcsCl');
        end


        function [Flag,Obj]=iswcsOk(Obj)
            % check minimal content of wcsCl object and update Exist property
            % Package: @wcsCl (Static)
            % Input  : - A wcsCl object.
            % Output : - An array of logical flags for each wcsCl element indicating if
            %            the wcsCl is likely valid.
            %          - A wcsCl object with the Exist property updated.
            % Example: [Flag,Obj]=wcsCl.iswcsOk(Obj)

            N = numel(Obj);
            Flag = false(size(Obj));
            for I=1:1:N
                Flag(I) = ~isempty(Obj(I).NAXIS) && ~isnan(Obj(I).NAXIS);
                Ntype = numel(Obj(I).CTYPE);
                for Itype=1:1:Ntype
                    if numel(Obj(I).CTYPE{Itype})>=5
                        Flag(I) = Flag(I);
                    else
                        Flag(I) = false(I);
                    end
                end

                Flag(I) = Flag(I) && all(~isempty(Obj(I).CRPIX)) && all(~isnan(Obj(I).CRPIX));
                Flag(I) = Flag(I) && all(~isempty(Obj(I).CRVAL)) && all(~isnan(Obj(I).CRVAL));
                Flag(I) = Flag(I) && all(~isempty(Obj(I).CD(:))) && all(~isnan(Obj(I).CD(:)));

                Obj(I).Exist = Flag(I);
            end
        end
        







        % static




        function Obj=populate(varargin)
            % Package: @wcsCl (basic)
            % Input  : * An arbitrary number of key,val, arguments that will
            %            populate the wcsCl object.
            %            Possible keywords are:
            %            'CD'
            %            'PC'
            %            'PV'
            %            'CDELT'
            %            'NAXIS'
            %            'WCSAXES'
            %            'CUNIT'
            %            'CRVAL'
            %            'PRPIX'
            %            'CTYPE'
            %            'RADESYS'
            %            'EQINOX'
            %            'LONPOLE'
            %            'LATPOLE'
            %            'MJD'
            % Output : - A wcsCl object

            Def.LONPOLE = 0;
            Def.LATPOLE = 90;

            InPar = inputParser;

            addOptional(InPar,'CD',[1 0; 0 1],@(x) isnumeric(x));
            addOptional(InPar,'PC',[1 0; 0 1],@(x) isnumeric(x));
            addOptional(InPar,'PV',{zeros(0,2),zeros(0,2)},@(x) iscell(x));
            addOptional(InPar,'CDELT',[1 1].',@(x) isnumeric(x));
            addOptional(InPar,'NAXIS',2,@(x) isnumeric(x));
            addOptional(InPar,'WCSAXES',2,@(x) isnumeric(x));
            addOptional(InPar,'CUNIT',{'deg','deg'},@(x) iscell(x));
            addOptional(InPar,'CRVAL',[0 0],@(x) isnumeric(x));     % alpha0, delta0
            addOptional(InPar,'CRPIX',[1 1],@(x) isnumeric(x));
            addOptional(InPar,'CTYPE',{'RA--TAN','DEC-TAN'},@(x) iscell(x));
            addOptional(InPar,'RADESYS','ICRS',@(x) ischar(x));  % 'ICRS' | 'FK5' | 'FK4' | 'FK4-NO-E' | 'GAPPT'
            addOptional(InPar,'EQUINOX',2000,@(x) isnumeric(x));
            addOptional(InPar,'LONPOLE',0,@(x) isnumeric(x));      % phiP
            addOptional(InPar,'LATPOLE',90,@(x) isnumeric(x));     % thetaP
            addOptional(InPar,'MJD',NaN,@(x) isnumeric(x));

            parse(InPar,varargin{:});
            InPar = InPar.Results;

            if isempty(InPar.CD)
                % try to populate CD using PC and CDELT
                if isempty(InPar.PC) || isempty(InPar.CDELT)
                    error('If CD is not provided, you must provide PC and CDELT');
                end
                InPar.CD = InPar.PC.*InPar.CDELT(:);
            end

            Obj = wcsCl(1);

            if ~strcmp(InPar.CTYPE{1}(5:7),InPar.CTYPE{2}(5:7))
                error('Projection algorithm must be equal to longitude and latitude');
            end

            if ~(strcmp(InPar.CTYPE{1}(1:2),'RA') || strcmp(InPar.CTYPE{1}(3:4),'LON'))
                error('Support only cases in which CTYPE1 contains longitudes');
            end
            if ~(strcmp(InPar.CTYPE{2}(1:3),'DEC') || strcmp(InPar.CTYPE{2}(3:4),'LAT'))
                error('Support only cases in which CTYPE2 contains latitude');
            end

            ProjAlgo  = InPar.CTYPE{1}(5:7);
            ProjClass = wcsCl.classify_projection(ProjAlgo);
            switch lower(ProjClass)
                case 'zenithal'

                    Alpha0 = InPar.CRVAL(1);
                    Delta0 = InPar.CRVAL(2);
                    AlphaP = InPar.CRVAL(1);
                    DeltaP = InPar.CRVAL(2);

                    Phi0   = 0;
                    Theta0 = 90;

                    if Delta0>=Theta0
                        PhiP = 0;
                    else
                        PhiP = 180;
                    end

                otherwise
                    error('Unsupported projection class (%s)',ProjClass);
            end

            switch lower(ProjAlgo)
                case {'tan'}

                    % treat LONPOLE
                    if isempty(InPar.LONPOLE)
                        % check if LONPOLE is in PV1_3
                        if isempty(InPar.PV)
                            % set to default value
                            InPar.LONPOLE = Def.LONPOLE;
                        else
                            if numel(InPar.PV{1})>=3
                                InPar.LONPOLE = InPar.PV{1}{3};
                            else
                                % set to default value
                                InPar.LONPOLE = Def.LONPOLE;
                            end
                        end
                    end

                    % treat LATPOLE
                    if isempty(InPar.LATPOLE)
                        % check if LATPOLE is in PV1_4
                        if isempty(InPar.PV)
                            % set to default value
                            InPar.LATPOLE = Def.LATPOLE;
                        else
                            if numel(InPar.PV{1})>=4
                                InPar.LATPOLE = InPar.PV{1}{4};
                            else
                                % set to default value
                                InPar.LATPOLE = Def.LATPOLE;
                            end
                        end
                    end

                case 'tpv'

                otherwise
            end

            % check validity
            if ~iscell(InPar.CTYPE)
                error('CTYPE must be a cell array');
            end
            if ~iscell(InPar.CUNIT)
                error('CUNIT must be a cell array');
            end
            if ~iscell(InPar.PV)
                error('PV must be a cell array');
            end

            Obj.WCS       = struct('RADESYS',InPar.RADESYS,...
                                 'EQUINOX',InPar.EQUINOX,...
                                 'NAXIS',  InPar.NAXIS,...
                                 'WCSAXES',InPar.WCSAXES,...
                                 'LONPOLE',InPar.LONPOLE,...
                                 'LATPOLE',InPar.LATPOLE,...
                                 'MJD',    InPar.MJD,...
                                 'CTYPE',  {InPar.CTYPE},...
                                 'CUNIT',  {InPar.CUNIT},...
                                 'CRVAL',  InPar.CRVAL,...
                                 'CRPIX',  InPar.CRPIX,...
                                 'CD',     InPar.CD,...
                                 'PV',     {InPar.PV},...
                                 'alpha0', Alpha0,...
                                 'delta0', Delta0,...
                                 'alphaP', AlphaP,...
                                 'deltaP', DeltaP);

        end

  
        
        
        % static
        function [X,Y]=projection_TAN(Long,Lat,R,CenterVec)
            % Package: @wcsCl (Static, projection)

                Long1 = CenterVec(1);
                Lat1  = CenterVec(2);
                % R is really R.*S (R-radius, S-scale factor)
                CosC = sin(Lat1).*sin(Lat) + cos(Lat1).*cos(Lat).*cos(Long-Long1);
                X = R.*cos(Lat).*sin(Long-Long1)./CosC;
                Y = R.*(cos(Lat1).*sin(Lat) - sin(Lat1).*cos(Lat).*cos(Long-Long1))./CosC;

        end


        function [X,Y]=projectTAN_coo2xy(Lon,Lat,CRVAL,Units)
            % project sky coordinates to interm. coordinates using TAN projection
            % Package: @wcsCl (Static, transformation)
            % Input  : - Longitude (Units).
            %          - Latitude (Units).
            %          - A two element vector of reference point relative
            %            to which the X/Y coordinates are measured.
            %          - Units of both the reference point and X/Y
            %            coordinates.
            %            {'rad'|'deg'}. Default is 'deg'.
            % Output : - X intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            X is measured toward the North.
            %          - Y intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            Y is measured toward the North.
            % Example: [Lon,Lat]=wcsCl.projectTAN_xy2coo(1,1,[100 45],'deg')
            %          [X,Y]=wcsCl.projectTAN_coo2xy(Lon,Lat,[100 45],'deg')


            if nargin<4
                Units = 'deg';
            end

            % measure angular distance from CRVAL
            ConvFactor = convert.angular(Units,'rad');
            Lon   = Lon.*ConvFactor;
            Lat   = Lat.*ConvFactor;
            CRVAL = CRVAL.*ConvFactor;

            [Theta,Phi] = celestial.coo.sphere_dist(CRVAL(1),CRVAL(2),Lon,Lat);

            Rtheta = tan(Theta);
            ConvFactor = convert.angular('rad',Units);
            X = Rtheta.*sin(Phi).*ConvFactor;
            Y = Rtheta.*cos(Phi).*ConvFactor;  % no minus sign

        end



        function [Lon,Lat]=projectTAN_xy2coo(X,Y,CRVAL,Units)
            % project interm. coordinates to sky coordinates using TAN projection
            % Package: @wcsCl (Static)
            % Input  : - X intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            X is measured toward the North.
            %          - Y intermediate coordinate in angular units as
            %            measured relative to a reference point.
            %            Y is measured toward the East.
            %          - A two element vector of reference point relative
            %            to which the X/Y coordinates are measured.
            %          - Units of both the reference point and X/Y
            %            coordinates.
            %            {'rad'|'deg'}. Default is 'deg'.
            % Output : - Longitude [Units].
            %          - Latitude [Units].
            % Example: [Lon,Lat]=wcsCl.projectTAN_xy2coo(1,1,[100 45],'deg')

            if nargin<4
                Units = 'deg';
            end

            ConvFactor = convert.angular(Units,'rad');
            CRVAL  = CRVAL.*ConvFactor;
            X      = X.*ConvFactor;
            Y      = Y.*ConvFactor;
            Rtheta = sqrt(X.^2 + Y.^2);
            Phi    = atan2(X,Y);  % X,-Y
            Theta  = atan(Rtheta);

            [Lat,Lon] = reckon(CRVAL(2),CRVAL(1),Theta,Phi,'radians');

            ConvFactor = convert.angular('rad',Units);
            Lon    = Lon.*ConvFactor;
            Lat    = Lat.*ConvFactor;

        end



        
    end          
       
    
    %======================================================================    
    
    methods (Static) % Unit-Test
        function Result = unitTest()

            io.msgStyle(LogLevel.Test, '@start', 'AstroWCS test started')
            
            % Change to data directory
            DataSampleDir = tools.os.getTestDataDir;
            PWD = pwd;
            cd(DataSampleDir);  
            
            % construct an empty AstroWCS
            AW = AstroWCS([2 2]);
            
            % construct a AstroWCS from AstroHeader with full TAN projection
            AH = AstroHeader('FOCx38i0101t_c0f.fits');
            AW = AstroWCS.header2wcs(AH);
            
            % Test for header without some: CD matrix missing, no CD matrix,  partial PC matrix, no PC matrix
            ValCD = cell2mat(AH.getCellKey({'CD1_1','CD1_2','CD2_1','CD2_2'}));
            
            AH.deleteKey({'CD1_2','CD2_2'});
            AW = AstroWCS.header2wcs(AH); % should fill with zeros
            
            AH.deleteKey({'CD1_1','CD2_1'});
            AH.insertKey({'CDELT1',ValCD(1);'CDELT2',ValCD(4)}); 
            AW = AstroWCS.header2wcs(AH); % should give diagonal CD with CDELT
            
            AH.insertKey({'PC1_1',1;'PC2_2',1});
            AW = AstroWCS.header2wcs(AH); % should give diagonal CD using PC and filling with zeros
            
            AH.insertKey({'PC1_2',ValCD(2)/ValCD(1);'PC2_1',ValCD(3)/ValCD(4)});
            AW = AstroWCS.header2wcs(AH); % using CDELT and PC should give identical CD to original

            
            % Test with no projection (fill ProjType=ProjClass='none')
            AH.replaceVal({'CTYPE1','CTYPE2'},{'RA','DEC'});
            AW = AstroWCS.header2wcs(AH);
            
            % Test with no radesys info
            AH.deleteKey('EQUINOX');
            AW = AstroWCS.header2wcs(AH);
            
            % Test with only radesys (no equinox)
            AH.insertKey({'RADESYS','FK5'});
            AW = AstroWCS.header2wcs(AH);
            
            % xy2sky tests
            PX = rand(1,500)*1000;
            PY = rand(1,500)*1000;
            RAD = 180./pi;
            
            % get [alpha, delta] for TAN projection
            AH = AstroHeader('FOCx38i0101t_c0f.fits');
            AW = AstroWCS.header2wcs(AH);
            [Alpha, Delta]  = AW.xy2sky(PX,PY,'deg');
            
            ds9('FOCx38i0101t_c0f.fits');
            [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
            d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
            disp(sprintf('Max distance for TAN projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
            
            % test sky2xy for TAN. 
            % First compare to xy2sky and then compared to ds9
            [PX1,PY1]  = AW.sky2xy(Alpha,Delta,'deg');
            d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
            disp(sprintf('Max distance for TAN projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));
            
            [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
            d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
            disp(sprintf('Max distance for TAN projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));
            
            % construct a AstroWCS from Header with TPV projection and get [alpha, delta]
            AH = AstroHeader('tpv.fits');
            AW = AstroWCS.header2wcs(AH);
            [Alpha, Delta]  = AW.xy2sky(PX,PY,'deg');
            
            ds9('tpv.fits');
            [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
            d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
            disp(sprintf('Max distance for TPV projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
            
            % test sky2xy for TPV. 
            % First compare to xy2sky and then compared to ds9
            [PX1,PY1]  = AW.sky2xy(Alpha,Delta,'deg');
            d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
            disp(sprintf('Max distance for TPV projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));          

            [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
            d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
            disp(sprintf('Max distance for TPV projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));            
            
            
            % construct a AstroWCS from Header with TAN-SIP projection  and get [alpha, delta]
            AH = AstroHeader('SPITZER_I1_70576896_0000_0000_1_bcd.fits');
            AW = AstroWCS.header2wcs(AH); 
            [Alpha, Delta]  = AW.xy2sky(PX,PY,'deg');
            
            ds9('SPITZER_I1_70576896_0000_0000_1_bcd.fits');
            [ds9_alpha,ds9_delta] = ds9.xy2coo(PX,PY,AW.RADESYS);
            d_mas = convert.angular('rad','mas',(celestial.coo.sphere_dist_fast(Alpha'./RAD,Delta'./RAD,ds9_alpha./RAD,ds9_delta./RAD)));
            disp(sprintf('Max distance for TAN-SIP projection (xy2sky vs. ds9) is %.1f [mas]',max(d_mas)));
            
            % test sky2xy for  TAN-SIP. 
            % First compare to xy2sky and then compared to ds9
            [PX1,PY1]  = AW.sky2xy(Alpha,Delta,'deg');
            d_pix = sqrt((PX-PX1).^2 + (PY-PY1).^2);
            disp(sprintf('Max distance for  TAN-SIP projection (xy2sky<->sky2xy) is %.1f [mili-pix]',max(d_pix)*1000));          

            [ds9_PX1,ds9_PY1] = ds9.coo2xy(Alpha, Delta);
            d_pix = sqrt((ds9_PX1'-PX1).^2 + (ds9_PY1'-PY1).^2);
            disp(sprintf('Max distance for  TAN-SIP projection (sky2xy vs. ds9) is %.1f [mili-pix]',max(d_pix)*1000));               
            
            
            % construct a AstroWCS from AstroHeader with Naxis=3, and empty
            % projtype in CTYPE3 and get [alpha, delta]
            AH = AstroHeader('WFPC2u5780205r_c0fx.fits');
            AW = AstroWCS.header2wcs(AH);
            %[Alpha, Delta]  = AW.xy2sky(PX,PY,'deg');

            
            % test other things
            
            
            cd(PWD);   
             
            io.msgStyle(LogLevel.Test, '@passed', 'AstroWCS test passed')
            Result = true;            
        end
end
    
end


