function Result = addCoordinates2catalog(Obj, Args)
    % Add or update RA/Dec coordinates in catalogs in AstroImage/Astrocatalog
    % Input  : - An AstroImage or AstroCatalog object containing a catalog.
    %          * ...,key,val,...
    %            'WCS' - An AstroWCS object. If not given will taken from
    %                   the AstroImage.WCS data. Default is [].
    %            'UpdateCoo' - A logical indicating if to update the RA/Dec
    %                   columns if already exist. Default is false.
    %            'OutUnits' - Output RA/Dec units. Default is 'rad'.
    %            'includeDistortion' - Include distortions. Default is true.
    %            'useTran2D' - Use Tran2D object. Default is false.
    %            'ColNameRA' - RA column name to insert. Default is 'RA'.
    %            'ColNameDec' - Dec column name to insert. Default is 'Dec'.
    %            'Pos' - Column poistion. Default is Inf.
    %            'CreateNewObj' - Create a new copy of the object.
    %                   Default is false.
    %            'DicNamesX' - Dictionary X column name.
    %                   Default is AstroCatalog.DefNamesX.
    %            'DicNamesY' - Dictionary Y column name.
    %                   Default is AstroCatalog.DefNamesY.
    %            'DicNamesRA' - Dictionary RA column name.
    %                   Default is AstroCatalog.DefNamesRA.
    %            'DicNamesDec' - Dictionary Dec column name.
    %                   Default is AstroCatalog.DefNamesDec.
    % Output : - An AstroImage or AstroCatalog object with the updataed
    %            coordinates.
    % Author : Eran Ofek (Aug 2021)
    % Example: Out = imProc.astrometry.addCoordinates2catalog(AI,'WCS',Result.WCS,'UpdateCoo',true)
    
    arguments
        Obj          % AstroCatalog | AstroImage
        Args.WCS                            = [];
        Args.UpdateCoo(1,1) logical         = false;    % update Coo if already exist
        Args.OutUnits                       = 'rad';
        Args.includeDistortion(1,1) logical = true;
        Args.useTran2D(1,1) logical         = false;
        
        Args.ColNameRA                      = 'RA';  % inserted column name
        Args.ColNameDec                     = 'Dec'; % inserted column name
        Args.Pos                            = Inf;
        
        Args.CreateNewObj logical           = false;
        
        Args.DicNamesX                      = AstroCatalog.DefNamesX;
        Args.DicNamesY                      = AstroCatalog.DefNamesY;
        Args.DicNamesRA                     = AstroCatalog.DefNamesRA;
        Args.DicNamesDec                    = AstroCatalog.DefNamesDec;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
       
        
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            Cat = Result(Iobj);
            WCS = Args.WCS;
        elseif isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
            if isempty(Args.WCS)
                WCS = Result(Iobj).WCS;
            else
                WCS = Args.WCS;
            end
        else
            error('Unknown input object type - first input arg must be AstroCatalog or AstroImage');
        end
        
        if isempty(WCS)
            error('WCS is empty');
        end
        
        % Cat contains an AstroCatalog object
        
        % check if RA/Dec exist in catalog
        [ColIndRA]  = colnameDict2ind(Cat, Args.DicNamesRA);
        [ColIndDec] = colnameDict2ind(Cat, Args.DicNamesDec);
        
        if isempty(ColIndRA) || isempty(ColIndDec)
            UpdateCoo = true;
        else
            % RA/Dec columns already exist in Cat
            UpdateCoo = Args.UpdateCoo;
        end
        
        if UpdateCoo
            [X, Y] = getXY(Cat, 'ColX',Args.DicNamesX, 'ColY',Args.DicNamesY);
            
           [Alpha, Delta]  = xy2sky(WCS, X, Y, 'OutUnits',Args.OutUnits,...
                                               'includeDistortion',Args.includeDistortion,...
                                               'useTran2D',Args.useTran2D);
            % replace or insert coordinates
            Cat = replaceCol(Cat, [Alpha, Delta], {Args.ColNameRA, Args.ColNameDec}, Args.Pos);
            
            % update catalog
            if isa(Obj, 'AstroCatalog')
                Result(Iobj) = Cat;
            elseif isa(Obj, 'AstroImage')
                Result(Iobj).CatData = Cat;
            else
                error('Unknown input object type - first input arg must be AstroCatalog or AstroImage');
            end
            
        end
            
            
        
        
    end
    
    
    
end