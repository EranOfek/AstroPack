function Obj = insertImageVal(Obj, ColNameX, ColNameY, varargin)
    % Insert Image values at specific positions into an AstroCatalog
    %   Given an AstroCatalog, insert the values of some images at the
    %   positions specified in the AstroCatalog as new columns in the
    %   AstroCatalog.
    % Input  : - An AstroCatalog object, or an AstroImage with a catalog.
    %          - X column name, or a cell dictionary of X column names.
    %            If empty, then use default.
    %            Default is AstroCatalog.DefNamesX
    %          - Y column name, or a cell dictionary of Y column names.
    %            If empty, then use default.
    %            Default is AstroCatalog.DefNamesY
    %          * Triplets of AstroImage, DataProp, ColName
    %            AstroImage is an AstroImage object containing an image
    %            from which to extract value. If this is a multi-element
    %            object, then each element should corresponds to an
    %            AstroCatalog element.
    % Output : - An AstroCatalog or AstroImage with catalog, with the new
    %            columns with values at the X/Y positions.
    % Author : Eran Ofek (Nov 2021)
    % Example: 
    %          imProc.cat.insertImageVal

    if isempty(ColNameX)
        ColNameX = AstroCatalog.DefNamesX;
    end
    if isempty(ColNameY)
        ColNameY = AstroCatalog.DefNamesY;
    end
    
    Pos = Inf;
    
    Nim1  = numel(varargin{1}); % number of images
    Nvarg = numel(varargin);
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        else
            Cat = Obj(Iobj);
        end

        
            
        if isnumeric(ColNameX) && isnumeric(ColNameY)
            % do nothing
            % X and Y already provided
            DataX = ColNameX;
            DataY = ColNameY;
        else
            % get X and Y from column names
            [DataX, DataY] = getXY(Cat, 'ColX',ColNameX, 'ColY',ColNameY);
        end
        
        Iim1 = min(Nim1, Iobj);
        [SizeY, SizeX] = varargin{1}(Iim1).sizeImage;
        
        if isempty(Y)
            Ind = X;
        else
            Ind = imUtil.image.sub2ind_fast([SizeY, SizeX], X, Y);
        end
            
        for Iarg=1:3:Nvarg
            % Image = varargin{Iarg}
            % DataProp = varargin{Iarg+1}
            % ColName  = varargin{Iarg+2}
            
            Val = getImageVal(Obj, Ind, [], 'DataProp',varargin{Iarg+1});
            Cat = insertCol(Cat, Val, Pos, varargin{Iarg+2}, '');
        end
        if isa(Obj, 'AstroImage')
            Obj(Iobj).CatData = Cat;
        else
            Obj(Iobj) = Cat;
        end
    end
end
            