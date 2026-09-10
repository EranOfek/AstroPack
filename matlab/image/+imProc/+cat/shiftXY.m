function Result = shiftXY(Obj, ShiftX, ShiftY, Args)
    % Shift the X/Y coordinate columns of a catalog by a constant offset.
    %   Adds ShiftX/ShiftY to every X/Y column that is present in the
    %   catalog. Intended for moving a catalog measured in the coordinates of
    %   a cropped image into the coordinates of a larger one (e.g. when
    %   stitching crops back together, or when comparing two overlapping
    %   sub-images).
    % Input  : - An AstroCatalog, AstroTable, or an AstroImage (including its
    %            subclasses, e.g. AstroDiff/AstroZOGY), or an array of them.
    %            For an AstroImage the CatData property is shifted.
    %          - Shift to add to the X columns. Either a scalar, applied to
    %            every element of the input, or a vector with one value per
    %            element.
    %          - Shift to add to the Y columns. Same form as the X shift.
    %          * ...,key,val,...
    %            'ColX' - Column name, or a cell array of column names, to
    %                   treat as X coordinates. Names which are not present
    %                   in the catalog are ignored.
    %                   Default is {'XPEAK','X1','X'}.
    %            'ColY' - Like 'ColX', for the Y coordinates.
    %                   Default is {'YPEAK','Y1','Y'}.
    %            'CreateNewObj' - A logical indicating if to copy the input
    %                   object before shifting. If false, the input object is
    %                   modified.
    %                   Default is false.
    % Output : - The input object with the X/Y columns shifted.
    % Author : Alexander Gioffe (Sep 2026)
    % Example: MCat = imProc.cat.shiftXY(MCat, ShiftX, ShiftY);
    %          AI   = imProc.cat.shiftXY(AI, 10, -5, 'ColX','X', 'ColY','Y');
    %          Cat  = imProc.cat.shiftXY(Cat, Sx, Sy, 'CreateNewObj',true);

    arguments
        Obj
        ShiftX
        ShiftY
        Args.ColX                 = {'XPEAK','X1','X'};
        Args.ColY                 = {'YPEAK','Y1','Y'};
        Args.CreateNewObj logical = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    ColX = convertColArg(Args.ColX);
    ColY = convertColArg(Args.ColY);

    Nobj = numel(Result);
    % a scalar shift applies to every element
    if numel(ShiftX)==1
        ShiftX = repmat(ShiftX, 1, Nobj);
    end
    if numel(ShiftY)==1
        ShiftY = repmat(ShiftY, 1, Nobj);
    end
    if numel(ShiftX)~=Nobj || numel(ShiftY)~=Nobj
        error('imProc:cat:shiftXY:BadShiftSize',...
              'The shifts must be scalars or have one value per element of the input (%d)', Nobj);
    end

    for Iobj=1:1:Nobj
        if isa(Result(Iobj), 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            Cat = Result(Iobj);
        end

        % an empty catalog has no column names to resolve
        if isempty(Cat.Catalog) || isempty(Cat.ColNames)
            continue;
        end

        IndX = colname2ind(Cat, ColX);
        IndY = colname2ind(Cat, ColY);
        IndX = IndX(~isnan(IndX));   % names absent from this catalog
        IndY = IndY(~isnan(IndY));

        if ~isempty(IndX)
            Cat.Catalog(:,IndX) = Cat.Catalog(:,IndX) + ShiftX(Iobj);
        end
        if ~isempty(IndY)
            Cat.Catalog(:,IndY) = Cat.Catalog(:,IndY) + ShiftY(Iobj);
        end

        if isa(Result(Iobj), 'AstroImage')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
    end
end

function Col = convertColArg(Col)
    % accept a single name as well as a cell array of names
    if ischar(Col) || isstring(Col)
        Col = cellstr(Col);
    end
end
