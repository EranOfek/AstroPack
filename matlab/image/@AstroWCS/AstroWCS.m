
% TODO Next - Documentation
% WCSAXES>2 Not supported yet

classdef AstroWCS < Component
    % Component should contain:
    
    % Add comments
    properties (Access = public)
%        Exist(1,1)   logical = false; % removed
        NAXIS(1,1)   uint8  = 2;
        WCSAXES(1,1) uint8  = 2;        
        CTYPE(1,:)   cell   = {'',''};   % e.g., 'RA---TAN', 'SIP', 'TPV', 'ZPN'
        CUNIT(1,:)   cell   = {'',''};
        RADESYS      char   = 'ICRS';
        LONPOLE      double = 180;%0; 
        LATPOLE      double = 0;%90;
        EQUINOX      double = 2000.0;
        CRPIX(1,:)   double = [0 0];
        CRVAL(1,:)   double = [1 1];
        CD           double = [1 0;0 1];
        
        PV                 = AstroWCS.DefPVstruct; 
        RevPV              = AstroWCS.DefPVstruct; 

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
        
        Tran2D(1,1) Tran2D   = [];       
        
    end
    
    properties (Hidden, Constant)
        DefPVstruct         = struct('KeyNamesX',[],'PolyCoefX',[],'PolyX_Xdeg',[],'PolyX_Ydeg',[],'PolyX_Rdeg',[],...
                                     'KeyNamesY',[],'PolyCoefY',[],'PolyY_Xdeg',[],'PolyY_Ydeg',[],'PolyY_Rdeg',[]);         
    end   
        
    % Future
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

   %======== Functions to construct AstroHeader from AstroWCS ========= 
   
       function Header = wcs2head(Obj,Header)
            % Convert AstroWCS object to AstroHeader key/par
            % Description:
            % Input  : - A AstroWCS object.
            %          - Optional AstroHeader in which to update key/par
            % Output : - An AstroHeader object with the WCS keywords.           

            arguments
                Obj
                Header      = AstroHeader(1);
            end            

            KeyCell = Obj.wcs2keyCell;
            Header.replaceVal(KeyCell(:,1),KeyCell(:,2),'Comment',KeyCell(:,3));
       end
   
       function KeyCell = wcs2keyCell(Obj)
           KeyCell = cell(0,3);
           
           %Add field by field
           
           if ~isempty(Obj.NAXIS)
               AddCell = {'NAXIS', Obj.NAXIS, 'Number of axes'};
               KeyCell = [KeyCell; AddCell];
           end
           if ~isempty(Obj.WCSAXES)
               AddCell = {'WCSAXES', Obj.WCSAXES, 'WCS dimensionality'};
               KeyCell = [KeyCell; AddCell];
           end 
           if ~isempty(Obj.RADESYS)
               AddCell = {'RADESYS', Obj.RADESYS, 'Astrometric system'};
               KeyCell = [KeyCell; AddCell];
           end       
           if ~isempty(Obj.EQUINOX)
               AddCell = {'EQUINOX', Obj.EQUINOX, 'Equinox'};
               KeyCell = [KeyCell; AddCell];
           end 
           if ~isempty(Obj.LONPOLE)
               AddCell = {'LONPOLE', Obj.LONPOLE, 'Native Longitude of the Celestial Pole'};
               KeyCell = [KeyCell; AddCell];
           end       
           if ~isempty(Obj.LATPOLE)
               AddCell = {'LATPOLE', Obj.LATPOLE, 'Native Latitude of the Celestial Pole'};
               KeyCell = [KeyCell; AddCell];
           end       
           
           Naxis = Obj.WCSAXES;
           % verify size
           if (numel(Obj.CTYPE)~=Naxis) || (numel(Obj.CUNIT)~=Naxis)
               error('Wrong size of CTYPE or CUNIT');
           end
           
           for Ix = 1:1:Naxis
               AddCell = {sprintf('CTYPE%d',Ix), Obj.CTYPE{Ix}, 'WCS projection type for this axis'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix = 1:1:Naxis
               AddCell = {sprintf('CUNIT%d',Ix), Obj.CUNIT{Ix}, 'Axis unit '};
               KeyCell = [KeyCell; AddCell];
           end

           % verify size
           if (numel(Obj.CRPIX)~=Naxis) || (numel(Obj.CRVAL)~=Naxis) ||...
              (size(Obj.CD,1)~=Naxis) || (size(Obj.CD,2)~=Naxis)
               error('Wrong size of CRPIX or CRVAL or CD');
           end           
           
           for Ix = 1:Naxis
               AddCell = {sprintf('CRPIX%d',Ix), Obj.CRPIX(Ix), 'Reference pixel on this axis'};
               KeyCell = [KeyCell; AddCell];
           end
              
           for Ix = 1:Naxis
               AddCell = {sprintf('CRVAL%d',Ix), Obj.CRVAL(Ix), 'World coordinate on this axis'};
               KeyCell = [KeyCell; AddCell];
           end
           
           for Ix1 = 1:Naxis
               for Ix2 = 1:Naxis
                   AddCell = {sprintf('CD%d_%d',Ix1,Ix2), Obj.CD(Ix1,Ix2), 'Linear projection matrix'};
                   KeyCell = [KeyCell; AddCell];
               end
           end
           
           for Ix = 1:numel(Obj.PV.KeyNamesX)
               AddCell = {Obj.PV.KeyNamesX{Ix}, Obj.PV.PolyCoefX(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];               
           end
           
           for Ix = 1:numel(Obj.PV.KeyNamesY)
               AddCell = {Obj.PV.KeyNamesY{Ix}, Obj.PV.PolyCoefY(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];               
           end  
           
           for Ix = 1:numel(Obj.RevPV.KeyNamesX)
               AddCell = {Obj.RevPV.KeyNamesX{Ix}, Obj.RevPV.PolyCoefX(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];               
           end
           
           for Ix = 1:numel(Obj.RevPV.KeyNamesY)
               AddCell = {Obj.RevPV.KeyNamesY{Ix}, Obj.RevPV.PolyCoefY(Ix), 'Projection distortion coefficient'};
               KeyCell = [KeyCell; AddCell];               
           end            
           
           switch lower(Obj.ProjType)
               case 'tan-sip'
                   AddCell = {'A_ORDER', numel(Obj.PV.KeyNamesX), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];
                   AddCell = {'B_ORDER', numel(Obj.PV.KeyNamesY), 'Polynomial order, axis 2'};
                   KeyCell = [KeyCell; AddCell];                   
                   AddCell = {'AP_ORDER', numel(Obj.RevPV.KeyNamesX), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];
                   AddCell = {'BP_ORDER', numel(Obj.RevPV.KeyNamesY), 'Polynomial order, axis 1'};
                   KeyCell = [KeyCell; AddCell];                   
           end
          
       end
        
   %======== Functions for related to xy2sky =========
   
        function [Alpha, Delta]  = xy2sky(Obj,PX,PY,Args)         
        % Convert X/Y pixel coordinates to celestial coordinates
        % Description: Convert X/Y pixel coordinates to celestial
        %              coordinates.
        % Input  : - A single element AstroWCS object
        %          - A matrix of pixel X coordinates.
        %            If next argument is not provided then this is a
        %            two or more column matrix of [PX,PY,...]
        %          - A matrix of pixel Y coordinates.
        %          - OutUnits 
        % Output : - A two column matrix of [RA,Dec](e.g. [Alpha, Delta]) or a matrix of RA
        %            coordinates.
        %          - A matrix of Dec coordinates.
        %            If not asked for, then the first output will be a
        %            two column matrix.

            arguments
                Obj
                PX
                PY                       = [];
                Args.OutUnits            = 'deg';
                Args.includeDistortion   = true;
                Args.useTran2D           = false;                
            end
            
            if Args.useTran2D
                error('Currently do not upport direct use of Tran2D');
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end

            if Obj.WCSAXES>2
                error('Currently do not support WCSAXES>2');
            end
            
            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            % pixel to intermediate (in units of CUNIT); including distortion
            [Xd,Yd] = Obj.pix2interm(PX,PY,Args.includeDistortion);
            
            % intermediate to native
            [Phi,Theta] = Obj.interm2native(Xd,Yd,'InUnits',Obj.CUNIT{1},'OutUnits','rad');
            
            % native to celestial 
            [Alpha, Delta] = Obj.native2celestial(Phi,Theta,'InUnits','rad','OutUnits',Args.OutUnits);
    
        end
              
        
        function [X,Y]=pix2interm(Obj,PX,PY,includeDistortion)
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
        
            arguments
                Obj
                PX
                PY                       = [];
                includeDistortion        = true;             
            end        

            if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end


            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end
            
            P = [PX(:), PY(:)].';
            relP = (P - Obj.CRPIX(:));
            
            % distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip') && includeDistortion
                
                u = relP(1,:);
                v = relP(2,:);
                
                [U,V]  = AstroWCS.forwardDistortion(Obj.PV,u,v,'plusXY_bool',true);  % U = u+f(u,v) and V = v+g(u,v)
                
                relP = [U ; V];
            end

            
            if Obj.NAXIS~=size(P,2) && Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axes and CD matrix');
            end

            XY = (Obj.CD*relP).';

            X = reshape(XY(:,1),size(PX));
            Y = reshape(XY(:,2),size(PY));
            
            % distorion for TPV
            if strcmpi(Obj.ProjType,'tpv') && includeDistortion
                
                R = sqrt(X.^2 + Y.^2);

                [X,Y]  = AstroWCS.forwardDistortion(Obj.PV,X,Y,'R',R);

            end

        end
        
        
        function [Phi,Theta]=interm2native(Obj,X,Y,Args)
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

            arguments
                Obj
                X
                Y                       = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';              
            end            
            

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % convert to deg
            ConvFactor = convert.angular(Args.InUnits,'deg');
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

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Theta = Theta.*ConvFactor;
            Phi   = Phi.*ConvFactor;


        end
        
    
        function [Alpha,Delta]=native2celestial(Obj,Phi,Theta,Args)
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

            arguments
                Obj
                Phi
                Theta                   = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';              
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
            ConvFactorIn = convert.angular(Args.InUnits,'rad');

            [Alpha,Delta]=Obj.phitheta2alphadelta(Phi.*ConvFactorIn,Theta.*ConvFactorIn,...
                                Obj.PhiP.*ConvFactorW,Obj.AlphaP.*ConvFactorW,Obj.DeltaP.*ConvFactorW,'rad');

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Alpha = Alpha.*ConvFactor;
            Delta = Delta.*ConvFactor;


        end    
        
        
   %======== Functions for related to sky2xy =========   
   
        function [PX,PY]  = sky2xy(Obj,Alpha,Delta,Args)            
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

            arguments
                Obj
                Alpha
                Delta                    = [];
                Args.InUnits             = 'deg';
                Args.includeDistortion   = true;
                Args.useTran2D           = false;                
            end
            
            if Args.useTran2D
                error('Currently do not upport direct use of Tran2D');
            end

            if numel(Obj)~=1
                error('Works only on a single element wcsCl object');
            end
            
            if Obj.WCSAXES>2
                error('Currently do not support WCSAXES>2');
            end

            if isempty(Delta)
                Delta = Alpha(:,2);
                Alpha = Alpha(:,1);
            end

            if (iscell(Delta) || ischar(Delta)) && (iscell(Alpha) || ischar(Alpha))
                Delta = celestial.coo.convertdms(Delta,'gH','r');
                Alpha = celestial.coo.convertdms(Alpha,'gD','R');
                Args.InUnits = 'rad';
            end


            % celestial to native
            [Phi,Theta] = Obj.celestial2native(Alpha(:),Delta(:),'InUnits',Args.InUnits,'OutUnits','rad');
            
            % native to intermediate
            [X,Y] = Obj.native2interm(Phi, Theta,'InUnits','rad','OutUnits',Obj.CUNIT{1});
            
            % Intermediate to pixel
            [PX,PY] = Obj.interm2pix(X,Y,Args.includeDistortion);

            PX = reshape(PX,size(Delta));
            PY = reshape(PY,size(Alpha));
        end      
        
        function [Phi,Theta]=celestial2native(Obj,Alpha,Delta,Args)            
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

            arguments
                Obj
                Alpha
                Delta                   = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';              
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
                    Args.InUnits = 'rad';
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
            ConvFactorIn = convert.angular(Args.InUnits,'rad');

            [Phi,Theta] = AstroWCS.alphadelta2phitheta(Alpha.*ConvFactorIn,Delta.*ConvFactorIn,...
                                    Obj.PhiP.*ConvFactorW, Obj.AlphaP.*ConvFactorW, Obj.DeltaP.*ConvFactorW, 'rad');

            ConvFactor = convert.angular('rad',Args.OutUnits);
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;

        end
   
        function [X,Y]=native2interm(Obj,Phi,Theta,Args)
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

            arguments
                Obj
                Phi
                Theta                       = [];
                Args.InUnits            = 'deg';
                Args.OutUnits           = 'deg';              
            end               

            if numel(Obj)~=1
                error('Works on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            % convert native coordinates to radians
            ConvFactor = convert.angular(Args.InUnits,'rad');
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
            
            
            ConvFactor = convert.angular('deg',Args.OutUnits);
            X   = X.*ConvFactor;
            Y   = Y.*ConvFactor;
            

        end    
        
        function [PX,PY]=interm2pix(Obj,X,Y,includeDistortion)
            % Convert intermediate pixel coordinates to pixel coordinates
            % Input  : - A single element AstroWCS object.
            %          - A matrix of X intermediate pixel coordinate.
            %            If next argument (Y) is not provided then this is
            %            a two column matrix of [X,Y].
            %          - A matrix of Y intermeditae pixel coordinate.
            % Output : - A matrix of X pixel coordinate.
            %          - A matrix of Y pixel coordinate.
            % Example: [P1,P2]=interm2pix(Obj,1,1)

            arguments
                Obj
                X
                Y                       = [];
                includeDistortion        = true;             
            end     
            
             if numel(Obj)~=1
                error('The wcsCl object input must contain a single element');
            end

            if Obj.NAXIS~=size(Obj.CD,1) && Obj.NAXIS~=size(Obj.CD,2)
                error('Number of coordinates must be consistent with number of axss and CD matrix');
            end
                
            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end
            
            % inverse distorion for TPV
            if strcmpi(Obj.ProjType,'tpv') && includeDistortion
                
                err_thresh = 1e-7;
                max_iters = 20;
                    
                [X,Y]  = AstroWCS.backwardDistortion(Obj.PV,X,Y,'plusXY_bool',false,'Threshold',err_thresh,'MaxIter',max_iters);
            end
      
            
            XY = [X(:), Y(:)];
            
            relP = (Obj.CD) \ XY.';
            
            % inverse distorion for TAN-SIP
            if strcmpi(Obj.ProjType,'tan-sip')  && includeDistortion
                
                U = relP(1,:);
                V = relP(2,:);
                    
                if ~isempty(Obj.RevPV.PolyCoefX)
                    [u,v]  = AstroWCS.forwardDistortion(Obj.RevPV,U,V,'plusXY_bool',true); % u = U + F(U,V), v = V+G(U,V)
                else

                    err_thresh = 1e-6;
                    max_iters = 20;

                    [u,v]  = AstroWCS.backwardDistortion(Obj.PV,U,V,'plusXY_bool',true,'Threshold',err_thresh,'MaxIter',max_iters);

                end
                
                relP = [u ; v]; 
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
            Obj.PV = Obj.build_PV_from_Header(AH,Obj.ProjType);
            
            % For TAN-SIP try to get RevPV (TODO generlize)
            if strcmpi(Obj.ProjType,'tan-sip')
                Obj.RevPV = AstroWCS.build_TANSIP_from_Header(Header,true);
            end


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
       
        function PV = build_PV_from_Header(Header,ProjType)
            
             switch lower(ProjType)
                case 'none'
                    PV = AstroWCS.DefPVstruct;
                case 'tan'
                    PV = AstroWCS.DefPVstruct;
                case 'tpv'
                    PV = AstroWCS.build_TPV_from_Header(Header);
                case 'tan-sip'                    
                    PV = AstroWCS.build_TANSIP_from_Header(Header);
                case 'zpn'                    
                    error('Need to add ZPN - TODO');  
                otherwise
                    error('Unsupported projection type (%s)',ProjType);
             end   
        end
        
        function PV = build_TPV_from_Header(Header)
            
            PV = AstroWCS.DefPVstruct; 
            
            AH = Header;
           
            PolyTPVtable = AstroWCS.polyTPVdef();
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV1_\d+','match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),'PV2_\d+','match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if NPV1
                PV.KeyNamesX = AH.Data(FlagMatchPV1,1)';
                PV.PolyCoefX = cell2mat(AH.Data(FlagMatchPV1,2))';
                PV.PolyX_Xdeg = zeros(1,NPV1);
                PV.PolyX_Ydeg = zeros(1,NPV1);
                PV.PolyX_Rdeg = zeros(1,NPV1);
                
                for I1 = 1:1:NPV1
                   currPV = PolyTPVtable(PV.KeyNamesX(I1),:);
                   if ~currPV.Axis==1
                       error('wrong axis');
                   end
                   PV.PolyX_Xdeg(I1) = currPV.xi_power;
                   PV.PolyX_Ydeg(I1) = currPV.eta_power;
                   PV.PolyX_Rdeg(I1) = currPV.r_power;
                end
                
                if all(PV.PolyX_Rdeg==0)
                    PV.PolyX_Rdeg=[];
                end
            end
            
            if NPV2
                PV.KeyNamesY = AH.Data(FlagMatchPV2,1)';
                PV.PolyCoefY = cell2mat(AH.Data(FlagMatchPV2,2))';
                PV.PolyY_Xdeg = zeros(1,NPV2);
                PV.PolyY_Ydeg = zeros(1,NPV2);
                PV.PolyY_Rdeg = zeros(1,NPV2);
                
                for I2 = 1:1:NPV2
                   currPV = PolyTPVtable(PV.KeyNamesY(I2),:);
                   if ~currPV.Axis==2
                       error('wrong axis');
                   end
                   PV.PolyY_Xdeg(I2) = currPV.xi_power;
                   PV.PolyY_Ydeg(I2) = currPV.eta_power;
                   PV.PolyY_Rdeg(I2) = currPV.r_power;                    
                end
                
                if all(PV.PolyY_Rdeg==0)
                    PV.PolyY_Rdeg=[];
                end                                
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
        
        function PV = build_TANSIP_from_Header(Header,get_inv)
            %used from both PV and invPV
            
            arguments
                Header
                get_inv        = false;             
            end             
            
            PV = AstroWCS.DefPVstruct;             
            
            AH = Header;
            
            BaseX = 'A';
            BaseY = 'B';
            
            if get_inv
                BaseX = 'AP';
                BaseY = 'BP';                
            end
                
            
            FlagMatchPV1 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),[BaseX '_\d+_\d+'],'match'));
            FlagMatchPV2 = ~tools.cell.isempty_cell(regexp(AH.Data(:,1),[BaseY '_\d+_\d+'],'match'));
            
            NPV1 = sum(FlagMatchPV1);
            NPV2 = sum(FlagMatchPV2);
            
            if NPV1
                
                PV.KeyNamesX = AH.Data(FlagMatchPV1,1)';
                PV.PolyCoefX = cell2mat(AH.Data(FlagMatchPV1,2))';
                PV.PolyX_Xdeg = zeros(1,NPV1);
                PV.PolyX_Ydeg = zeros(1,NPV1);
                
                PV1_Powers  =regexp(AH.Data(FlagMatchPV1,1), [BaseX '_(?<u_power>\d+)\_(?<v_power>\d+)'],'names');
                for I1 = 1:1:NPV1
                   PV.PolyX_Xdeg(I1) = str2double(PV1_Powers{I1}.u_power);
                   PV.PolyX_Ydeg(I1) = str2double(PV1_Powers{I1}.v_power);                    
                end               
            end
            
            if NPV2
                PV.KeyNamesY = AH.Data(FlagMatchPV2,1)';
                PV.PolyCoefY = cell2mat(AH.Data(FlagMatchPV2,2))';
                PV.PolyY_Xdeg = zeros(1,NPV2);
                PV.PolyY_Ydeg = zeros(1,NPV2);
                
                PV2_Powers  =regexp(AH.Data(FlagMatchPV2,1), [BaseY '_(?<u_power>\d+)\_(?<v_power>\d+)'],'names');                
                
                for I2 = 1:1:NPV2
                   PV.PolyY_Xdeg(I2) = str2double(PV2_Powers{I2}.u_power);
                   PV.PolyY_Ydeg(I2) = str2double(PV2_Powers{I2}.v_power);                      
                end
            end              
            
       
            
        end   
        
        
   %======== Functions to construct AstroWCS from Tran2D =========
   
        function Obj = tran2wcs(Tran2D, Args)
            % NAXIS,CRPIX,CRVAL,CD,CTYPE,CUNIT,RADESYS,EQUINOX,LONPOLE,LATPOLE,WCSAXES)
            % Create and populate an AstroWCS object from an Trans2D object

            arguments
                Tran2D
                Args.NAXIS(1,1)
                Args.CRPIX
                Args.CRVAL
                Args.CD
                Args.CTYPE
                Args.CUNIT     
                Args.RADESYS   = [];
                Args.EQUINOX   = [];
                Args.LONPOLE   = [];
                Args.LATPOLE   = [];
                Args.WCSAXES   = [];
            end      

            % if WCSAXES is not given use NAXIS as default            
            if isempty(Args.WCSAXES)
                Args.WCSAXES = Args.NAXIS;
            end

            Obj = AstroWCS(1);
            Obj.Tran2D = Tran2D;
            
            % Paste number of axes

            Obj.NAXIS = Args.NAXIS;
            Obj.WCSAXES = Args.WCSAXES;

            
            % paste CTYPE and transalte to projection information (ProjType,
            % ProjClass) and CooName and CUNIT            
            Obj.CTYPE = Args.CTYPE;
            Obj.CUNIT = Args.CUNIT;                
            Obj.read_ctype;
            
            % paste base WCS info           
            if ~isempty(Args.RADESYS)
                Obj.RADESYS = Args.RADESYS;
            end
            if ~isempty(Args.EQUINOX)
                Obj.EQUINOX = Args.EQUINOX;
            end
            if ~isempty(Args.LONPOLE)
                Obj.LONPOLE = Args.LONPOLE;
            end
            if ~isempty(Args.LATPOLE)
                Obj.LATPOLE = Args.LATPOLE;
            end            
         
            Obj.CRPIX = Args.CRPIX;
            Obj.CRVAL = Args.CRVAL;
                       
            Obj.CD = Args.CD;

            Obj.PV = Obj.build_PV_from_Tran2D(Obj.Tran2D, Obj.ProjType);
            
            %consider adding RevPV for TAN-SIP
            
            % populate proj Meta
            Obj.populate_projMeta;
        end
        
        function PV = build_PV_from_Tran2D(Tran2D,ProjType,set_rev)

            arguments
                Tran2D
                ProjType
                set_rev        = false;             
            end                         
            
            if isempty(Tran2D.PolyRep.PolyParX) || isempty(Tran2D.PolyRep.PolyX_Xdeg) || isempty(Tran2D.PolyRep.PolyX_Ydeg) || ...
                    isempty(Tran2D.PolyRep.PolyParY) || isempty(Tran2D.PolyRep.PolyY_Xdeg) || isempty(Tran2D.PolyRep.PolyY_Ydeg)
                Tran2D.polyRep;
            end
            
            PV = AstroWCS.DefPVstruct;
            
            PV.PolyCoefX = Tran2D.PolyRep.PolyParX;
            PV.PolyX_Xdeg = Tran2D.PolyRep.PolyX_Xdeg;            
            PV.PolyX_Ydeg = Tran2D.PolyRep.PolyX_Ydeg;
            PV.PolyCoefY = Tran2D.PolyRep.PolyParY;            
            PV.PolyY_Xdeg = Tran2D.PolyRep.PolyY_Xdeg;            
            PV.PolyY_Ydeg = Tran2D.PolyRep.PolyY_Ydeg;  

             switch lower(ProjType)
                case 'none'
                    % do nothing
                case 'tan'
                    % do nothing
                case 'tpv'
                    PV = AstroWCS.fill_TPV_KeyNames(PV);
                case 'tan-sip'                    
                    PV = AstroWCS.fill_TANSIP_KeyNames(PV,set_rev);
                case 'zpn'                    
                    error('Need to add ZPN - TODO');  
                otherwise
                    error('Unsupported projection type (%s)',ProjType);
             end               
             
        end
        
        function PV = fill_TPV_KeyNames(PV)
                        
            PolyTPVtable = AstroWCS.polyTPVdef();
            
            NPV1 = length(PV.PolyCoefX);
            NPV2 = length(PV.PolyCoefY);
            
            if NPV1
                PV.KeyNamesX = cell(size(PV.PolyCoefX));
                
                for I1 = 1:1:NPV1
                    Axis = 1;
                    xi_power = PV.PolyX_Xdeg(I1);
                    eta_power = PV.PolyX_Ydeg(I1);
                    if isempty(PV.PolyX_Rdeg)
                        r_power = 0;
                    else
                        r_power = PV.PolyX_Rdeg(I1);
                    end
                    PV.KeyNamesX(I1) = PolyTPVtable.Row(PolyTPVtable.Axis==Axis & PolyTPVtable.xi_power==xi_power & PolyTPVtable.eta_power==eta_power & PolyTPVtable.r_power==r_power);                  
                end
            end
            
            if NPV2
                PV.KeyNamesY = cell(size(PV.PolyCoefY));
                
                for I2 = 1:1:NPV2
                    Axis = 2;
                    xi_power = PV.PolyY_Xdeg(I2);
                    eta_power = PV.PolyY_Ydeg(I2);
                    if isempty(PV.PolyY_Rdeg)
                        r_power = 0;
                    else
                        r_power = PV.PolyY_Rdeg(I2);
                    end
                    PV.KeyNamesY(I2) = PolyTPVtable.Row(PolyTPVtable.Axis==Axis & PolyTPVtable.xi_power==xi_power & PolyTPVtable.eta_power==eta_power & PolyTPVtable.r_power==r_power);                  
                end
            end
        
        end
        
        function PV = fill_TANSIP_KeyNames(PV,set_rev)  

            arguments
                PV
                set_rev        = false;             
            end 
            
            BaseX = 'A';
            BaseY = 'B';
            
            if set_rev
                BaseX = 'AP';
                BaseY = 'BP';                
            end

            NPV1 = length(PV.PolyCoefX);
            NPV2 = length(PV.PolyCoefY);           
            
            if ~isempty(PV.PolyX_Rdeg) || ~isempty(PV.PolyY_Rdeg)
                error('TANSIP do not support poly for R');
            end
            
            if NPV1
                 PV.KeyNamesX = cell(size(PV.PolyCoefX));
                
                for I1 = 1:1:NPV1
                    u_power = PV.PolyX_Xdeg(I1);
                    v_power = PV.PolyX_Ydeg(I1);
                    PV.KeyNamesX{I1} = sprintf('%s_%d_%d',BaseX,u_power, v_power);              
                end
                          
            end
            
            if NPV2
                 PV.KeyNamesY = cell(size(PV.PolyCoefY));
                
                for I2 = 1:1:NPV2
                    u_power = PV.PolyY_Xdeg(I2);
                    v_power = PV.PolyY_Ydeg(I2);
                    PV.KeyNamesY{I2} = sprintf('%s_%d_%d',BaseY,u_power, v_power);              
                end
                          
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

            arguments
                Phi
                Theta
                PhiP
                AlphaP
                DeltaP
                Units        = 'deg';             
            end 
            
            RAD = 180./pi;

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

        function [Xd,Yd]  = forwardDistortion(PV,X,Y,Args)
           % use PV structure to calcualte polynomial distortion
           % plusXY_bool - to add X,Y to the poliniomial. (e.g. in TAN-SIP)

            arguments
                PV
                X
                Y
                Args.R            = 1;
                Args.plusXY_bool  = false;             
            end 
            
            CoefX    = PV.PolyCoefX;
            X_Xpower = PV.PolyX_Xdeg;
            X_Ypower = PV.PolyX_Ydeg;
            if ~isempty(PV.PolyX_Rdeg)
                X_Rpower = PV.PolyX_Rdeg;
            else
                X_Rpower = 0;
            end

            CoefY    = PV.PolyCoefY;
            Y_Xpower = PV.PolyY_Xdeg;
            Y_Ypower = PV.PolyY_Ydeg;
            if ~isempty(PV.PolyY_Rdeg)
                Y_Rpower = PV.PolyY_Rdeg;
            else
                Y_Rpower = 0;
            end                        


            Xd = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((Args.R(:).').^X_Rpower(:)) );
            Yd = sum(CoefY(:) .* ((X(:).').^Y_Xpower(:) ) .* ((Y(:).').^Y_Ypower(:))  .* ((Args.R(:).').^Y_Rpower(:)) );                

            Xd=reshape(Xd,size(X));
            Yd=reshape(Yd,size(Y));
            
            if Args.plusXY_bool
                Xd = Xd+X;
                Yd = Yd+Y;
            end

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

            arguments
                Alpha
                Delta
                PhiP
                AlphaP
                DeltaP
                Units        = 'deg';             
            end 
            
            RAD = 180./pi;

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
        

        function [X,Y]  = backwardDistortion(PV,Xd,Yd,Args)  
            
            arguments
                PV
                Xd
                Yd
                Args.plusXY_bool  = false;     
                Args.Threshold    = 1e-7; 
                Args.MaxIter      = 100; 
                Args.Step         = 1e-5; 
            end 
            
            Xf = Xd;
            Yf = Yd;
            
            Xi  = Xf;
            Yi  = Yf;
            
            % The ouput from forward should be the input Xf, Yf
            NotConverged = true;
            Iter = 0;
            while NotConverged
                %
                Iter = Iter + 1;
                
                if isempty(PV.PolyX_Rdeg) && isempty(PV.PolyY_Rdeg)
                    R = 1;
                else
                    R = sqrt(Xi.^2 + Yi.^2); % TODO - change to arbitrary function f(x,y)
                end                
                
                [Xi1,Yi1] = AstroWCS.forwardDistortion(PV,Xi,Yi,'R',R,'plusXY_bool',Args.plusXY_bool);
                [Xi2,Yi2] = AstroWCS.forwardDistortion(PV,(Xi + Args.Step),(Yi + Args.Step),'R',R,'plusXY_bool',Args.plusXY_bool);
                
                
                DeltaX = (Xi1 - Xi2);
                DeltaY = (Yi1 - Yi2);
                
                IncX = (Xi1 - Xf)./DeltaX .* Args.Step;
                IncY = (Yi1 - Yf)./DeltaY .* Args.Step;
                Xi = Xi + IncX;
                Yi = Yi + IncY; 
                
                [Xi1,Yi1] = AstroWCS.forwardDistortion(PV,Xi,Yi,'R',R,'plusXY_bool',Args.plusXY_bool);
                
                DiffX = Xi1 - Xf;
                DiffY = Yi1 - Yf;
                
                if (max(abs(DiffX)) < Args.Threshold) && (max(abs(DiffY)) < Args.Threshold)
                    NotConverged = false;
                end
                if Iter > Args.MaxIter
                    NotConverged = false;
                    %error('backwardDistortion didnot converge after %d iterations',Iter);
                end
                
            end
            
            X = Xi;
            Y = Yi;
        end
        
       
    end

    
   %======================================================================    
   %==================OLD FUNCTIONS=======================================
   %======================================================================    
    
    
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
       
    end

    %======================================================================
    
    methods (Static)

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
        
    end          
       
    
    %========================Unit-Test======================================    
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % Unit-Test
                        
    end
    
end
