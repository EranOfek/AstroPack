function Result = projectionInv(Obj, Lon0, Lat0, Scale, Projection, Args)
    % project X/Y to Lon/Lat using specified projection
    % Input  : - A matrix, AstroImage with catalog, or AstroCatalog object.
    %            This is the input catalog containing some
    %            X/Y data to be projected.
    %          - (Lon0) - The center longitude of projection.
    %          - (Lat0) - The center latitude of projection.
    %          - (Scale) - Scale radius (for e.g., 'gnomonic projection').
    %            Default is 1.
    %          - Projection type - options are:
    %            Gnomonic projection: {'tan','gnomonic','tpv','tan-sip','sip'}
    %            ...
    %          * ...,key,val,...
    %            'Coo0Units' - Units for the Lon0/Lat0 coordinates.
    %                   Default is 'deg'.
    %            'AddNewCols' - A cell array of column names to add with
    %                   the projected X/Y coordinates. If empty, then the
    %                   projected coordinates will be stored in the Lon/Lat
    %                   input coordinates.
    %            'Pos' - Position of new columns. Default is Inf.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    %            'ColLon' - A cell array of column names in which to look
    %                   for the longitude. If input is a matrix, then this
    %                   will be always the first column. Default is
    %                   AstroCatalog.DefNamesRA.
    %            'ColLat' - A cell array of column names in which to look
    %                   for the latitude. If input is a matrix, then this
    %                   will be always the first column. Default is
    %                   AstroCatalog.DefNamesDec.
    %            'OutUnits' 
    %- A cell array of units for the lon/lat
    %                   columns. This is used only in the input is a matrix.
    %                   Default is {'deg','deg'}.
    % Output : - An AstroCatalog or AstroImage object in which the catalog
    %            is updated with the projected coordinates.
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imProc.trans.projectionInv(rand(100,2), 0.5, 0.5, 1, 'tan', 'Coo0Units','rad');
    %          Result = imProc.trans.projectionInv(rand(100,2), 0.5, 0.5, 180./pi, 'tan', 'Coo0Units','deg');
    
    
    arguments
        Obj
        Lon0
        Lat0
        Scale                             = 1;
        Projection                        = 'tan';
        Args.Coo0Units                    = 'deg';
        
        
        Args.AddNewCols                   = {'RA','Dec'};  % if empty, wriote over Lon/Lat columns
        Args.Pos                          = Inf;
        
        Args.CreateNewObj                 = [];
        
        Args.ColX                         = AstroCatalog.DefNamesX;
        Args.ColY                         = AstroCatalog.DefNamesY;
        %Args.OutUnits                     = {'deg','deg'};  % only for matrix input
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = AstroCatalog;
        Obj.Catalog  = Tmp;
        Obj.ColNames = {'X','Y'};
        Obj.ColUnits = {'',''};
    end
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end
    
    % convert Lon0, Lat0 to 'rad'
    ConvFactor = convert.angular(Args.Coo0Units, 'rad');
    Lon0       = Lon0.*ConvFactor;
    Lat0       = Lat0.*ConvFactor;
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        
        % get X/Y from catalog
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            error('Unknown catalog format');
        end
            
        [ColX]   = colnameDict2ind(Cat, Args.ColX);
        [ColY]   = colnameDict2ind(Cat, Args.ColY);
        [X, ~] = getCol(Cat, ColX);
        [Y, ~] = getCol(Cat, ColY);  
        % scale coordinates
        X = X.*Scale;
        Y = Y.*Scale;
        
        switch lower(Projection)
            case {'tan','gnomonic','tpv','tan-sip','sip'}
                % Gnomonic transformation
                
                [Lon, Lat] = celestial.proj.pr_ignomonic(X, Y, [Lon0, Lat0]);
                                
%             case {'sin'}
%                 % sin projection
%                 
%                 [X,Y] = celestial.proj.pr_sin(Lon, Lat, [Lon0, Lat0]);
                
            otherwise
                error('Unsupported projection option');
        end
       
        if isempty(Args.AddNewCols)
            % write X/Y over Lon/Lat
            Cat = replaceCol(Cat, [Lon, Lat], [ColX, ColY]);
        else
            Cat = insertCol(Cat, [Lon, Lat], Args.Pos, Args.AddNewCols, {'',''});
        end
        
        % store Cat in Object
        if isa(Obj, 'AstroCatalog')
            Result(Iobj) = Cat;
        elseif isa(Obj, 'AstroImage')
            Result(Iobj).CatData = Cat;
        else
            error('Unknown catalog format');
        end
            
        
        
    end
end