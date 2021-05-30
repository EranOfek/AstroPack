% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroWCS < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
        Image 
        Mask 
        Back 
        Var
        Cat
    end
    
    properties (SetAccess = public)
        Header AstroHeader
        PSF AstroPSF
        CatData AstroCatalog
        WCS AstroWCS
        
        % Data
        ImageData(1,1) BaseImage
        MaskData(1,1) BaseImage
        BackData(1,1) BaseImage
        VarData(1,1) BaseImage
    end
    
    
    %======================================================================
    
    properties (Access = public)
        Exist(1,1)   logical = false;
        NAXIS(1,1)   uint8  = 2;
        WCSAXES(1,1) uint8  = 2
        CTYPE(1,:)   cell   = {'',''};
        CUNIT        cell   = {'deg','deg'};
        RADESYS      char   = 'ICRS';
        LONPOLE      double = 0; 
        LATPOLE      double = 90;
        EQUINOX             = 2000;
        CRPIX(1,:)   double = [0 0];
        CRVAL(1,:)   double = [1 1];
        CDELT(1,:)   double = [1 1];
        CD           double = [1 0;0 1];
        PC           double = [];
        PV           cell   = {zeros(0,2),zeros(0,2)};
        SIP          cell   = {zeros(0,2),zeros(0,2)};
    end     
    properties (GetAccess = public)
        ProjType     char   = '';
        ProjClass    char   = '';
        CooName(1,:) cell   = {'',''};
        AlphaP(1,1)  double = NaN;
        DeltaP(1,1)  double = NaN;
        Alpha0(1,1)  double = NaN;
        Delta0(1,1)  double = NaN;
        Phi0(1,1)    double = NaN;
        Theta0(1,1)  double = NaN;
        PhiP(1,1)    double = NaN;
        
    end
        
%         WCS    % a structure with fields specified in Fields
%         gridLon     % a cube: e.g., Long = [X,Y,Color]
%         gridLat     % a cube: e.g., Lat  = [X,Y,Color]
%         gridCoo     % a cell array of {X,Y,...} - column per axes 
%         gridAxes = {'RA','Dec','Color'}
    
    properties (Hidden)
        UserData
    end
    
%======================================================================    
    
    methods
       
        function W=wcsCl(varargin)
            % wcsCl class constructor
            % Package: @wcsCl
            % Input  : * Arbitrary number of number of elements in each
            %            dimension. Deafult is 1,1
            % Output : - An wcsCl object of the requested size.

            if (numel(varargin)==0)
                varargin{1} = 1;
                varargin{2} = 1;
            elseif (numel(varargin)==1)
                
                if numel(varargin{1})>1
                    varargin = num2cell(varargin{1});
                else
                    varargin{2} = 1;
                end
            else
                % do nothing
            end
            Dim = cell2mat(varargin);
            
            Nel = prod(Dim);
            
            for I=1:1:Nel
                W(I).UserData = [];
            end
            
            W=reshape(W,varargin{:});
            
        end

    end
    
    
    % setters/getters
    methods
        
        function set.CTYPE(W,Val)
            % setter for wcsCl CTYPE property         
            W.CTYPE    = Val;
        end
               
    end
 
    
    %======================================================================    
    
    
    methods % Constructor
       
        function Obj = AstroWCS
            
            
        end

    end
 

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            Obj.ImageData.Data = Data;
        end
        
        function Data = get.Image(Obj)
            % getter for Image - get image from ImageData property
            Data = Obj.ImageData.Data;
        end        
    end
    
    methods (Static)  % static methods
       
    end
    
    
    
    methods % functionality
        function Result = fun_unary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
            %
           
            Nobj = numel(Obj)
            
            
        end
        
    end
    

    %======================================================================
    
    methods
        
        
        function [Phi,Theta]=alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP,Units)
            % Convert celestial coordinates to native coordinates
            % Package: @wcsCl (Static, transformations)
            % Input  : - Celestial longitude.
            %          - Celestial latitude.
            %          - Native longitude of the celestial pole.
            %          - Celestial longitude of the native pole.
            %          - Celestial latitude of the native pole.
            %          - Units of the input and output coordinates
            %            {'deg'|'rad'}. Default is 'deg'.
            % Output : - Native longitude
            %          - Native latitude
            % Example: [Phi,Theta]=wcsCl.alphadelta2phitheta(Alpha,Delta,PhiP,AlphaP,DeltaP)


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



        function [Phi,Theta]=celestial2native(W,Alpha,Delta,InUnits,OutUnits)
            % convert celestial coordinates to native coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert sphericall celestial coordinates
            %              (alpha, delta) or cosine directions to native
            %              spherical coordinates.
            % Input  : - A single element wcsCl object.
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
            %          - Output celestial coordinate units {'rad'|'deg'}
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

            if numel(W)~=1
                error('Works only on a single element wcsCl object');
            end

            Units = W.CUNIT{1}; % units of properties in the wcsCl class
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
            if ~strcmp(W.CUNIT{1},W.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            % convert to radians
            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Phi,Theta] = wcsCl.alphadelta2phitheta(Alpha.*ConvFactorIn,Delta.*ConvFactorIn,...
                                    W.PhiP.*ConvFactorW, W.AlphaP.*ConvFactorW, W.DeltaP.*ConvFactorW, 'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Phi   = Phi.*ConvFactor;
            Theta = Theta.*ConvFactor;

        end



        function ProjClass=classify_projection(ProjType)
        % classify projection type
        % Package: @wcsCl (Static, utilities)
        % Example: ProjClass=wcsCl.classify_projection('TAN')

            switch lower(ProjType)
                case {'none','',char(zeros(1,0))}
                    ProjClass = 'none';
                case {'tan','tpv','tan-sip','sin','ncp','azp','szp','stg','arc','zpn','zea','air'}
                    ProjClass = 'zenithal';
                case {'cyp','cea','car','mer'}
                    ProjClass = 'cylindrical';
                case {'sfl','par','mol','ait'}
                    ProjClass = 'pseudocylindrical';
                case {'cop','coe','cod','coo'}
                    ProjClass = 'conic';
                case {'bon','pco'}
                    ProjClass = 'polyconic';  % and pseudoconic
                case {'tsc','csc','qsc'}
                    ProjClass = 'quadcube';
                otherwise
                    error('Unknown ProjType option (%s)',ProjType);
            end

        end



        function varargout=coo2xy(W,Lon,Lat,Units)
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
            % Example: [X,Y] = coo2xy(W,100,10,'deg')

            if nargin<4
                Units = 'deg';
            end

            if numel(W)~=1
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
            [Phi,Theta] = celestial2native(W,[Lon(:),Lat(:)],[],Units,'deg');
            % native to intermediate
            [X,Y] = native2interm(W,[Phi, Theta],[],'deg');
            % inverse distorsion for TPV and SIP
            switch lower(W.ProjType)
                case 'tpv'
                    % for TPV, invert numerically, iteratively, hopefully
                    %  converging
                    niter=0;
                    err=Inf;
                    X0=X;
                    Y0=Y;
                    while niter<10 && err > 1e-6 % arbitrary iteration stop
                       [X1,Y1] = interm2TPVdistortedInterm(W,X0,Y0);
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
            [PX,PY] = interm2pix(W,[X,Y],[]);

            PX = reshape(PX,size(Lon));
            PY = reshape(PY,size(Lat));

            if nargout>1
                varargout{1} = PX;
                varargout{2} = PY;
            else
                varargout{1} = [PX,PY];
            end

        end


        function W=fill(W,Force)
            % Fill ProjType, ProjClass, Coo, AlphaP, DeltaP in wcsCl object
            % Package: @wcsCl (basic)
            % Input  : - A wcsCl object
            %          - A logical indicating if to re-fill the values even
            %            if exist. Default is true.
            % Output : - A wcsCl object
            % Example: W.fill

            if nargin<2
                Force = true;
            end

            Nw = numel(W);
            for Iw=1:1:Nw
                if Force || isempty(W(Iw).ProjType) || isempty(W(Iw).ProjClass) || isnan(W(Iw).AlphaP) || isnan(W(Iw).DeltaP)
                    % fill missing values

                    N = numel(W(Iw).CTYPE);
                    Coo  = cell(1,N);
                    Proj = cell(1,N);
                    for I=1:1:N
                        if isempty(W(Iw).CTYPE{I})
                            Proj{I} = 'none';
                            Coo{I}  = 'unknown';
                        else

                            Coo{I} = strrep(W(Iw).CTYPE{I}(1:4),'-','');
                            if numel(W(Iw).CTYPE{I})<5
                                Proj{I} = 'none';
                            else
                                Proj{I} = strrep(W(Iw).CTYPE{I}(5:end),'-','');
                            end

                            if I>1
                                if ~(strcmp(Proj{I},Proj{1}) || strcmp(Proj{I},'none'))
                                    error('Projection of all axes must be the same unless none');
                                end
                            end
                        end
                    end

                    W(Iw).ProjType  = Proj{1};
                    W(Iw).CooName   = Coo;
                    W(Iw).ProjClass = wcsCl.classify_projection(Proj{1});


        %                     if ~(strcmp(W(Iw).CTYPE{1}(1:2),'RA') || strcmp(W(Iw).CTYPE{1}(3:4),'LON'))
        %                         error('Support only cases in which CTYPE1 contains longitudes');
        %                     end
        %                     if ~(strcmp(W(Iw).CTYPE{2}(1:3),'DEC') || strcmp(W(Iw).CTYPE{2}(3:4),'LAT'))
        %                         error('Support only cases in which CTYPE2 contains latitude');
        %                     end

                     switch lower(W(Iw).ProjClass)
                        case 'none'
                            W(Iw).Alpha0 = W(Iw).CRVAL(1);
                            W(Iw).Delta0 = W(Iw).CRVAL(2);
                            W(Iw).AlphaP = W(Iw).CRVAL(1);
                            W(Iw).DeltaP = W(Iw).CRVAL(2);

                            W(Iw).Phi0   = NaN;
                            W(Iw).Theta0 = NaN;
                            W(Iw).PhiP   = NaN;

                        case 'zenithal'

                            W(Iw).Alpha0 = W(Iw).CRVAL(1);
                            W(Iw).Delta0 = W(Iw).CRVAL(2);
                            W(Iw).AlphaP = W(Iw).CRVAL(1);
                            W(Iw).DeltaP = W(Iw).CRVAL(2);

                            Units = strtrim(W(Iw).CUNIT{1});
                            ConvFactor = convert.angular('deg',Units);

                            W(Iw).Phi0   = 0.*ConvFactor;
                            W(Iw).Theta0 = 90.*ConvFactor;

                            if W(Iw).Delta0>=W(Iw).Theta0
                                W(Iw).PhiP = 0.*ConvFactor;
                            else
                                W(Iw).PhiP = 180.*ConvFactor;
                            end

                        otherwise
                            error('Unsupported projection class (%s)',W(Iw).ProjClass);
                     end

                end

            end

        end


        function W=fill_PV(W)
            % fill missing values in the PV matrix with zeros
            % Package: @wcsCl (basic)
            % Input  : - A wcsCl object
            % Output : - A wcsCl object
            % Example: W=wcsCl.pop_exampl; W.fill W.fill_PV;

            Nw = numel(W);
            for Iw=1:1:Nw
                N  = numel(W(Iw).PV);
                for I=1:1:N
                    Ind  = W(Iw).PV{I}(:,1);
                    Coef = W(Iw).PV{I}(:,2);

                    FullInd = (0:1:max(Ind))';
                    IsM = ismember(FullInd,Ind);
                    FullCoef = zeros(size(FullInd));
                    FullCoef(IsM) = Coef;
                    W(Iw).PV{I} = [FullInd, FullCoef];
                end
            end
        end


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



        function [Phi,Theta]=interm2native(W,X,Y,InUnits,OutUnits)
            % project coordinates: intermediate to native
            % Package: @wcsCl (transformations)
            % Input  : - A wcsCl object
            %          - A matrix of intermediate X coordinate.
            %            If next argument (Y) is not orovided then this
            %            is a two column matrix of [X,Y].
            %          - A matrix of intermediate Y coordinate. 
            %          - Input intermediate coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            %          - Output native coordinates units. Default is 'deg'.
            % Output : - A matrix of native Phi coordinate.
            %          - A matrix of native Theta coordinate.
            % Example: [Phi,Theta]=interm2native(W,100,100)

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Y = [];
                    end
                end
            end

            if numel(W)~=1
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


            switch lower(W.ProjClass)
                case 'zenithal'

                    switch lower(W.ProjType)
                        case 'tan'
                            Rtheta = sqrt(X.^2 + Y.^2);        % deg
                            Theta  = atan(180./(pi.*Rtheta));  % rad
                            Phi    = atan2(X,-Y);              % rad

                        case 'tpv'
                            Rtheta = sqrt(X.^2 + Y.^2);
                            Theta  = atan(180./(pi.*Rtheta));
                            Phi    = atan2(X,-Y);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',ProjAlgo);
                    end

                otherwise
                    error('Unsupported projection class (%s)',W.ProjClass);
            end

            ConvFactor = convert.angular('rad',OutUnits);
            Theta = Theta.*ConvFactor;
            Phi   = Phi.*ConvFactor;


        end


        function [PX,PY]=interm2pix(W,X,Y)
            % Convert intermediate pixel coordinates to pixel coordinates
            % Package: @wcsCl (transformations)
            % Input  : - A single element wcsCl object.
            %          - A matrix of X intermediate pixel coordinate.
            %            If next argument (Y) is not provided then this is
            %            a two column matrix of [X,Y].
            %          - A matrix of Y intermeditae pixel coordinate.
            % Output : - A matrix of X pixel coordinate.
            %          - A matrix of Y pixel coordinate.
            % Example: [P1,P2]=interm2pix(W,1,1)

             if numel(W)~=1
                error('The wcsCl object input must contain a single element');
            end

            if W.NAXIS~=size(W.CD,1) && W.NAXIS~=size(W.CD,2)
                error('Number of coordinates must be consistent with number of axss and CD matrix');
            end

            if nargin<3
                Y = [];
            end
            if isempty(Y)
                Y = X(:,2);
                X = X(:,1);
            end

            % X = W.WCS.CD*[P - W.WCS.CRPIX(:)];
            % X = CD*P - CD*CRPIX
            % CD^-1 X = I*P - I*CRPIX
            % P = CRPIX + CD^T X

            %P = (W.CRPIX(:) + inv(W.CD) * XY.').';

            XY = [X(:), Y(:)];
            P = [inv(W.CD) * XY.' + W.CRPIX(:)].';

            %[inv(W(Iw).(WCSField).CD) * XY.' + W(Iw).(WCSField).CRPIX(:)].';
            %X = W.WCS.CD.'*XY.'; % - W.WCS.CRPIX(:)];

            PX = reshape(P(:,1),size(X));
            PY = reshape(P(:,2),size(Y));

        end



        function [Xi,Yi]=interm2TPVdistortedInterm(W,X,Y)
        % Apply TPV distortion to intermediate pixel position
        % Package: @wcsCl (transformations)
        % Input  : - A single element wcsCl object
        %          - A matrix of intermediate pixel position X.
        %          - A matrix of intermediate pixel position Y.
        % Output : - A matrix of the distorted intermediate pixel position X.
        %          - A matrix of the distorted intermediate pixel position Y.
        % Example: [Xi,Yi]=interm2TPVdistortedInterm(W,1,1)

            % polynomial mapping: term Xi  Yi r
            PolyPV = wcsCl.polyPVdef;



            if numel(W)~=1
                error('Input must be a single element wcsCl object');
            end

            R = sqrt(X.^2 + Y.^2);

            Np = numel(W.PV);
            if Np~=2
                error('A TPV distortion should contain two columns');
            end

            Nc     = size(W.PV{1},1);

            CoefX  = W.PV{1}(:,2);
            CoefY  = W.PV{2}(:,2);

            PolyPV = PolyPV(1:Nc,:);

            Xi = CoefX.' * ((X(:).'.^PolyPV(:,2)) .* (Y(:).'.^PolyPV(:,3)) .* (R(:).'.^PolyPV(:,4)));
            Yi = CoefY.' * ((Y(:).'.^PolyPV(:,2)) .* (X(:).'.^PolyPV(:,3)) .* (R(:).'.^PolyPV(:,4)));

            Xi=reshape(Xi,size(X));
            Yi=reshape(Yi,size(Y));
        end


        function Ans=isPopulated(W)
            % Check if wcsCl object Exist field is false (i.e., no WCS in object)
            % Package: @wcsCl (basic)
            
            Ans = [W.Exist];
        end
 
        
        function Ans=iswcsCl(W)
        % Check if object is a wcsCl object 
        % Package: @wcsCl (basic)
            Ans = isa(W,'wcsCl');
        end


        function [Flag,W]=iswcsOk(W)
            % check minimal content of wcsCl object and update Exist property
            % Package: @wcsCl (Static)
            % Input  : - A wcsCl object.
            % Output : - An array of logical flags for each wcsCl element indicating if
            %            the wcsCl is likely valid.
            %          - A wcsCl object with the Exist property updated.
            % Example: [Flag,W]=wcsCl.iswcsOk(W)

            N = numel(W);
            Flag = false(size(W));
            for I=1:1:N
                Flag(I) = ~isempty(W(I).NAXIS) && ~isnan(W(I).NAXIS);
                Ntype = numel(W(I).CTYPE);
                for Itype=1:1:Ntype
                    if numel(W(I).CTYPE{Itype})>=5
                        Flag(I) = Flag(I);
                    else
                        Flag(I) = false(I);
                    end
                end

                Flag(I) = Flag(I) && all(~isempty(W(I).CRPIX)) && all(~isnan(W(I).CRPIX));
                Flag(I) = Flag(I) && all(~isempty(W(I).CRVAL)) && all(~isnan(W(I).CRVAL));
                Flag(I) = Flag(I) && all(~isempty(W(I).CD(:))) && all(~isnan(W(I).CD(:)));

                W(I).Exist = Flag(I);
            end
        end
        

        function [Alpha,Delta]=native2celestial(W,Phi,Theta,InUnits,OutUnits)
            % convert native coordinates to celestial coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert spherical native coordinates
            %              (phi, theta) to spherical celestial
            %              coordinates (alpha, delta).
            % Input  : - A single element wcsCl object.
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
            % Example: [Alpha,Delta]=native2celestial(W,[1 1],[2 2])

            if nargin<5
                OutUnits = 'deg';
                if nargin<4
                    InUnits = 'deg';
                    if nargin<3
                        Theta = [];
                    end
                end
            end

            if numel(W)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(Theta)
                Theta = Phi(:,2);
                Phi   = Phi(:,1);
            end

            Units = W.CUNIT{1};

            % input/output units
            if ~strcmp(W.CUNIT{1},W.CUNIT{2})
                error('CUNIT of longitude and latitude must be the same');
            end

            ConvFactorW  = convert.angular(Units,'rad');
            ConvFactorIn = convert.angular(InUnits,'rad');

            [Alpha,Delta]=wcsCl.phitheta2alphadelta(Phi.*ConvFactorIn,Theta.*ConvFactorIn,...
                                W.PhiP.*ConvFactorW,W.AlphaP.*ConvFactorW,W.DeltaP.*ConvFactorW,'rad');

            ConvFactor = convert.angular('rad',OutUnits);
            Alpha = Alpha.*ConvFactor;
            Delta = Delta.*ConvFactor;


        end


        function [X,Y]=native2interm(W,Phi,Theta,InUnits)
            % project coordinates: native to intermediate
            % Package: @wcsCl (transformations)
            % Input  : - A wcsCl object
            %          - A matrix of native Phi coordinate.
            %            If the next input argument (Theta) is not provided
            %            then this is a two column matrix of [Phi,Theta]
            %            native coordinates.
            %          - A matrix of native Theta coordinate.
            %          - Input native coordinates units {'rad'|'deg'}.
            %            Default is 'deg'.
            % Output : - A matrix of X intermediate pixel coordinate.
            %          - A matrix of Y intermediate pixel coordinate.
            % Example: [X,Y]=native2interm(W,100,100)

            if nargin<4
                InUnits = 'deg';
                if nargin<3
                    Theta = [];
                end
            end

            if numel(W)~=1
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

            switch lower(W.ProjClass)
                case 'zenithal'

                    switch lower(W.ProjType)
                        case 'tan'
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        case 'tpv'
                            Rtheta = 180./pi .* cot(Theta);    % deg
                            X      = Rtheta.*sin(Phi);
                            Y      = -Rtheta.*cos(Phi);

                        otherwise

                            %Rtheta = 180./pi .* (Mu + 1).*cos(Theta)./(Mu + sin(Theta);

                            error('Unsupported projection option (%s)',ProjAlgo);
                    end

                otherwise
                    error('Unsupported projection class (%s)',W.ProjClass);
            end

        end



        function [Alpha,Delta]=phitheta2alphadelta(Phi,Theta,PhiP,AlphaP,DeltaP,Units)
            % convert natve coordinates (Phi,Theta) to celestila (alpha,delta)
            % Package: @wcsCl (Static, transformations)
            % Input  : - Native longitude (phi)
            %          - Native latitude (theta)
            %          - native longitude of celestial pole
            %          - Celestial longitude of native pole
            %          - Celestial latitude of native pole (DeltaP=ThetaP)
            %          - Input and output units {'deg'|'rad'}.
            %            Default is 'deg'
            % Output : - Celestial longitude
            %          - Celestial latitude
            % Example: - [Alpha,Delta]=wcsCl.phitheta2alphadelta(1.1,1.1,0,0,0)


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


        function [X,Y]=pix2interm(W,PX,PY)
        % Convert pixel coordinates (P) to intermediate coordinates (X)
        % Package: @wcsCl (transformations)
        % Input  : - A single element wcsCl object
        %          - A matrix of pixel X coordinate.
        %            If next argument is not provided then this is a
        %            two column matrix of [PX,PY].
        %          - A matrix of pixel Y coordinate.
        % Output : - A matrix of X intermediate coordinate.
        %          - A matrix of Y intermediate coordinate.
        %            The intermediate coordinates units are specified in
        %            CUNIT.
        % Example: [X,Y]=pix2interm(W,1,1)

            if numel(W)~=1
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

            if W.NAXIS~=size(P,2) && W.NAXIS~=size(W.CD,1) && W.NAXIS~=size(W.CD,2)
                error('Number of coordinates must be consistent with number of axes and CD matrix');
            end

            XY = (W.CD*(P - W.CRPIX(:))).';

            X = reshape(XY(:,1),size(PX));
            Y = reshape(XY(:,2),size(PY));

        end


        function [Ind,PolyPV]=poly2tpvInd(Poly_Xdeg,Poly_Ydeg,Poly_Rdeg)
            %
            % Example: [Ind,PolyPV]=wcsCl.poly2tpvInd(T.PolyRep.PolyX_Xdeg,T.PolyRep.PolyX_Ydeg)

            if nargout<3
                Poly_Rdeg = zeros(size(Poly_Xdeg));
            end


            PolyPV = wcsCl.polyPVdef;

            N = numel(Poly_Xdeg);
            Ind = nan(N,1);
            for I=1:1:N
                Flag   = PolyPV(:,2)==Poly_Xdeg(I) & PolyPV(:,3)==Poly_Ydeg(I) & PolyPV(:,4)==Poly_Rdeg(I);
                Ind(I) = PolyPV(Flag,1);
            end
        end


        function PolyPV=polyPVdef
            % return the TPV polynomial definition
            % Output : - A matrix of [term, Xi, Yi, r]
            % Example: PolyPV=wcsCl.polyPVdef

            % polynomial mapping: term Xi  Yi r
            PolyPV =   [0     0   0  0;...
                %
                        1     1   0  0;...
                        2     0   1  0;...
                        3     0   0  1;...
                %
                        4     2   0  0;...
                        5     1   1  0;...
                        6     0   2  0;...
                %
                        7     3   0  0;...
                        8     2   1  0;...
                        9     1   2  0;...
                        10    0   3  0;...
                        11    0   0  3;...
                %
                        12    4   0  0;...
                        13    3   1  0;...
                        14    2   2  0;...
                        15    1   3  0;...
                        16    0   4  0;...
                %
                        17    5   0  0;...
                        18    4   1  0;...
                        19    3   2  0;...
                        20    2   3  0;...
                        21    1   4  0;...
                        22    0   5  0;...
                        23    0   0  5;...
                %
                        24    6   0  0;...
                        25    5   1  0;...
                        26    4   2  0;...
                        27    3   3  0;...
                        28    2   4  0;...
                        29    1   5  0;...
                        30    0   6  0;...
                %
                        31    7   0  0;...
                        32    6   1  0;...
                        33    5   2  0;...
                        34    4   3  0;...
                        35    3   4  0;...
                        36    2   5  0;...
                        37    1   6  0;...
                        38    0   7  0;...
                        39    0   0  7];

        end


        function W=pop_example
            % populate an example in a wcsCl object
            % Package: @wcsCl (Static)
            % Example: W=wcsCl.pop_example

            % taken from PTF image:
            % PTF_201211213689_i_p_scie_t085110_u014664936_f02_p100037_c02.fits

            W = wcsCl;
            W.Exist   = true;
            W.NAXIS   = 2;
            W.WCSAXES = 2;
            W.CTYPE   = {'RA--TPV','DEC-TPV'};
            W.CUNIT   = {'deg','deg'};
            W.RADESYS = 'ICRS';
            W.EQUINOX = 2000;
            W.CD      = [0.000281213122191427, 6.60586568794139E-06; 6.75167063981288E-06, -0.000281077673400925];
            W.CRVAL   = [148.750256715202, 69.4980616019507];
            W.CRPIX   = [586.994, 1882.221];
            W.LONPOLE = 180;
            W.LATPOLE = 0;

            PV1 = [0             0
                   1             1
                   2             0
                   4    0.00078642
                   5   -0.00065487
                   6    0.00012021
                   7   -0.00045983
                   8    0.00017232
                   9    -0.0002152
                   10   -0.00016153
                   12    -0.0011808
                   13   -0.00021634
                   14    0.00036701
                   15    0.00028443
                   16    4.0954e-05];

            PV2 = [0             0
                   1             1
                   2             0
                   4   -0.00078711
                   5    0.00036965
                   6    0.00011267
                   7   -0.00022865
                   8    0.00011075
                   9    0.00012075
                   10    -0.0019171
                   12     0.0001629
                   13    1.7579e-06
                   14    0.00029664
                   15    0.00021697
                   16     0.0028449];

            W.PV = {PV1, PV2};

            W.fill;
            W.fill_PV;

        end




        function W=populate(varargin)
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

            W = wcsCl(1);

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

            W.WCS       = struct('RADESYS',InPar.RADESYS,...
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

        function W=populate_projMeta(W)
            % populate projection and pole information in a wcsCl object
            % Package: @wcsCl
            % Description: 
            % Input  : - A wcsCl object
            % Output : - W wcsCl object with the projection type and pole data populated.




            Def.LONPOLE = 0;
            Def.LATPOLE = 90;

            N = numel(W);
            for I=1:1:N

                ProjAlgo  = W(I).CTYPE{1}(6:end);
                ProjClass = wcsCl.classify_projection(ProjAlgo);
                W(I).ProjType  = ProjAlgo;
                W(I).ProjClass = ProjClass;
                switch lower(ProjClass)
                    case 'zenithal'

                        Alpha0 = W(I).CRVAL(1);
                        Delta0 = W(I).CRVAL(2);
                        AlphaP = W(I).CRVAL(1);
                        DeltaP = W(I).CRVAL(2);

                        Phi0   = 0;
                        Theta0 = 90;

                        if Delta0>=Theta0
                            PhiP = 0;
                        else
                            PhiP = 180;
                        end

                        W(I).Alpha0 = Alpha0;
                        W(I).Delta0 = Delta0;
                        W(I).AlphaP = AlphaP;
                        W(I).DeltaP = DeltaP;
                        W(I).Phi0   = Phi0;
                        W(I).Theta0 = Theta0;
                        W(I).PhiP   = PhiP;
                    otherwise
                        error('Unsupported projection class (%s)',ProjClass);
                end



                switch lower(ProjAlgo)
                    case {'tan'}

                        % treat LONPOLE
                        if isempty(W(I).LONPOLE)
                            % check if LONPOLE is in PV1_3
                            if isempty(W(I).PV)
                                % set to default value
                                W(I).LONPOLE = Def.LONPOLE;
                            else
                                if numel(W(I).PV{1})>=3
                                    W(I).LONPOLE = W(I).PV{1}{3};
                                else
                                    % set to default value
                                    W(I).LONPOLE = Def.LONPOLE;
                                end
                            end
                        end

                        % treat LATPOLE
                        if isempty(W(I).LATPOLE)
                            % check if LATPOLE is in PV1_4
                            if isempty(W(I).PV)
                                % set to default value
                                W(I).LATPOLE = Def.LATPOLE;
                            else
                                if numel(W(I).PV{1})>=4
                                    W(I).LATPOLE = InPar.PV{1}{4};
                                else
                                    % set to default value
                                    W(I).LATPOLE = Def.LATPOLE;
                                end
                            end
                        end

                    case 'tpv'

                    otherwise
                end
            end
        end
        
        
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


        function varargout=xy2coo(W,PX,PY,OutUnits)
            % Convert X/Y pixel coordinates to celestial coordinates
            % Package: @wcsCl (transformations)
            % Description: Convert X/Y pixel coordinates to celestial
            %              coordinates.
            % Input  : - A single element wcsCl object
            %          - A matrix of pixel X coordinates.
            %            If next argument is not provided then this is a
            %            two or more column matrix of [PX,PY,...]
            %          - A matrix of pixel Y coordinates.
            % Output : - A two column matrix of [RA,Dec] or a matrix of RA
            %            coordinates.
            %          - A matrix of Dec coordinates.
            %            If not asked for, then the first output will be a
            %            two column matrix.
            % Example: W=wcsCl.pop_example;
            %          [RA,Dec]=xy2coo(W,1,1)

            if nargin<4
                OutUnits = 'deg';
                if nargin<3
                    PY = [];
                end
            end

            if numel(W)~=1
                error('Works only on a single element wcsCl object');
            end

            if isempty(PY)
                PY = PX(:,2);
                PX = PX(:,1);
            end

            % pixel to intermediate (in units of CUNIT)
            [X,Y] = pix2interm(W,PX,PY);
            % interm pixel coordinates TPV distortion
            switch lower(W.ProjType)
                case 'tpv'
                    [X,Y] = interm2TPVdistortedInterm(W,X,Y);
                case 'tan-sip'
                    'not implemented yet'
                otherwise
                    % do nothing
            end

            % intermediate to native
            [Phi,Theta] = interm2native(W,X,Y,W.CUNIT{1},'rad');
            % native to celestial 
            [Alpha, Delta] = native2celestial(W,Phi,Theta,'rad',OutUnits);

            if nargout<2
                varargout{1} = [Alpha, Delta];
            else
                varargout{1} = Alpha;
                varargout{2} = Delta;
            end

        end
        
    end          
       
    
    %======================================================================    
    
    methods % Unit-Test
        function Result = unitTest()

            io.msgStyle(LogLevel.Test, '@start', 'AstroWCS test started')
            
            
            io.msgStyle(LogLevel.Test, '@passed', 'AstroWCS test passed')
            Result = true;            
        end
end
    
end
