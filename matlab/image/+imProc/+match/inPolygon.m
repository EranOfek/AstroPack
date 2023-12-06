function [Result, Flag] = inPolygon(CatObj, Coo, Args)
    % Return sources inside polygon
    % Input  : - An AstroCatalog/AstroImage object. If multiple elements
    %            then will perform the inPolygon search on any one of
    %            the elements.
    %          - A two column matrix of [Long, Lat] or [X,Y] that
    %            defines the verteces of the polygon.
    %          * ...,key,val,...
    %            'CooType' - Which Coo system to use 'pix' (for X/Y),
    %                   or 'sphere' (for RA/Dec).
    %                   If empty, will look for 'sphere', and if not exist
    %                   will use 'pix'. Default is [].
    %            'CooUnits' - Units of (spherical) coordinates
    %                   (second input argument). Default is 'deg'.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroCatalog object with the found sources.
    %          - A vector of logicals with the same length as the
    %            number of rows in the input object. True if object
    %            is in polygon. If the input is an object with
    %            multiple elements, then this vector corresponds to
    %            the last element in the object array.
    % Author : Eran Ofek (Apr 2021)
    % very similar to coneSearch
    % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
    %          [InP, Flag] = imProc.match.inPolygon(AC,[1 1; 1.1 1.1; 0.5 0.1],'CooUnits','rad')

    arguments
        CatObj
        Coo

        Args.CooType                     = [];
        Args.CooUnits char               = 'deg';
        Args.CreateNewObj                = [];
    end

%     if isempty(CatObj(1).CooType)
%         CatObj.getCooTypeAuto;
%     end

    % Convert Coo to radians
    CooRad = convert.angular(Args.CooUnits,'rad',Coo);

    if isempty(Args.CreateNewObj)
        if nargout>0
            Args.CreateNewObj = true;
        else
            Args.CreateNewObj = false;
        end
    end

    % convert AstroImage to AstroCatalog
    if isa(CatObj,'AstroImage')
        CatObj = astroImage2AstroCatalog(CatObj,'CreateNewObj',Args.CreateNewObj);
    elseif isa(CatObj,'AstroCatalog')
        % do nothing
    elseif isnumeric(CatObj)
        error('Input CatObj is of unsupported class');
    else
        error('Input CatObj is of unsupported class');
    end

    if Args.CreateNewObj
        Result = CatObj.copy();
		Result.Catalog = [];
    else 
        Result = CatObj;
    end

    Nobj = numel(CatObj);
    for Iobj=1:1:Nobj
        %if ~CatObj(Iobj).IsSorted
        %    CatObj(Iobj).sortrows(CatObj(Iobj).ColY);
        %end

        if isempty(Args.CooType)
            [CooType] = getCooType(CatObj(Iobj));
            CooType = CooType{1};
        else
            CooType = Args.CooType;
        end
        
        switch lower(CooType)
            case 'sphere'
                Flag = celestial.htm.in_polysphere(getLonLat(CatObj(Iobj),'rad'),CooRad);
            case 'pix'
                Pos = getXY(CatObj(Iobj));
                Flag = inpolygon(Pos(:,1),Pos(:,2), Coo(:,1), Coo(:,2));
            otherwise
                error('Unknown CooType option');
        end
        Result(Iobj).Catalog = CatObj(Iobj).Catalog(Flag,:);

    end


end
