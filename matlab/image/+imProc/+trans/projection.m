function Result = projection(Obj, Lon0, Lat0, Scale, Projection, Args)
    % project Lon/Lat to X/Y using specified projection
    % Input  : - A matrix, AstroImage with catalog, or AstroCatalog object.
    %            This is the input catalog containing some
    %            Longitude/Latitude data to be projected.
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
    %                   input coordinates. Default is {'X','Y'}.
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
    %            'ColUnits' - A cell array of units for the lon/lat
    %                   columns. This is used only in the input is a matrix.
    %                   Default is {'deg','deg'}.
    % Output : - An AstroCatalog or AstroImage object in which the catalog
    %            is updated with the projected coordinates.
    % Author : Eran Ofek (Jun 2021)
    % Example: Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 1, 'tan', 'Coo0Units','rad', 'ColUnits',{'rad','rad'})
    %          Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 180./pi, 'tan', 'Coo0Units','deg', 'ColUnits',{'deg','deg'})
    
    arguments
        Obj
        Lon0
        Lat0
        Scale                             = 1;
        Projection                        = 'tan';
        Args.Coo0Units                    = 'deg';
        Args.AddNewCols                   = {'X','Y'};  % if empty, wriote over Lon/Lat columns
        Args.Pos                          = Inf;
        
        Args.CreateNewObj                 = [];
        
        Args.ColLon                       = AstroCatalog.DefNamesRA;
        Args.ColLat                       = AstroCatalog.DefNamesDec;
        Args.ColUnits                     = {'deg','deg'};  % only for matrix input
    end
    
    if isnumeric(Obj)
        Tmp = Obj;
        Obj = AstroCatalog;
        Obj.Catalog  = Tmp;
        Obj.ColNames = {'RA','Dec'};
        Obj.ColUnits = Args.ColUnits;
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
        
        % get Lon/Lat from catalog
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            error('Unknown catalog format');
        end
            
        [ColLon]   = colnameDict2ind(Cat, Args.ColLon);
        [ColLat]   = colnameDict2ind(Cat, Args.ColLat);
        [Lon, LonUnits] = getCol(Cat, ColLon);
        [Lat, LatUnits] = getCol(Cat, ColLat);  
        
        % convert Lon/Lat to 'rad'
        Lon = convert.angular(LonUnits{1}, 'rad', Lon);
        Lat = convert.angular(LatUnits{1}, 'rad', Lat);
            
        switch lower(Projection)
            case {'tan','gnomonic','tpv','tan-sip','sip'}
                % Gnomonic transformation
                
                [X,Y] = celestial.proj.pr_gnomonic(Lon, Lat, Scale, [Lon0, Lat0]);
                
            otherwise
                error('Unsupported projection option');
        end
       
        if isempty(Args.AddNewCols)
            % write X/Y over Lon/Lat
            Cat = replaceCol(Cat, [X, Y], [ColLon, ColLat]);
            % in this case, make sure that CooType is not 'sphere'
        else
            Cat = insertCol(Cat, [X, Y], Args.Pos, Args.AddNewCols, {'',''});
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