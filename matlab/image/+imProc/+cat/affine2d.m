function Result = affine2d(Obj, AffineMatrix,Direction, Args)
    % Apply affine transformation to AstroCatalog, or catalog in AstroImage
    % Input  : - An AstroCatalaog or AstroImage object.
    %          - Either a 2D affine matrix, or a vector of rotation and shift.
    %            If a matrix, this is a 3x3 affine matrix
    %            e.g., [cos(Theta), -sin(Theta), ShiftX
    %                   sin(Theta),  cos(Theta), ShiftY
    %                   0            0           1]
    %            Otherwise a vector of [Theta, Scale, ShiftX, ShiftY]
    %            or [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY]
    %          - Direction of rotation:
    %            '+' (or positive) will rotate the coordinates in respect to the reference
    %            frame, while '-' (or negative) will rotate the reference frame.
    %            Default is 1.
    %          * ...,key,val,...
    %            'ThetaUnits' - Units of rotation (theta) angle.
    %                   Default is 'deg'.
    %            'ColX' - A cell array of X coordinate column names on which to operate
    %                   the transformation.
    %                   This can be either a dictionary list from which the
    %                   forst existing column will be selected, or a list
    %                   of columns.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColX' - Like 'ColX', but for the Y coordinate.
    %                   Default is AstroCatalog.DefNamesY.
    %            'AllCols' - A logical indicating if to treat the 'ColX'
    %                   and 'ColY', as a list of columns on which to
    %                   operate (true), or a dictionary from which only the
    %                   first existing (X/Y) column will be selected
    %                   (false). Default is true.
    %            'CreateNewObj' - Create a new copy of the object.
    %                   Default is false.
    % Author : Eran Ofek (Dec 2022)
    % Example: AC=AstroCatalog({rand(10,2)},'ColNames',{'X','Y'})
    %          AC=imProc.cat.affine2d(AC, [0 1 10 5],1,'AllCols',false);

    arguments
        Obj
        AffineMatrix
        Direction                    = 1;
        Args.ThetaUnits              = 'deg';
        Args.ColX                    = AstroCatalog.DefNamesX;
        Args.ColY                    = AstroCatalog.DefNamesY;
        Args.AllCols logical         = true;
        Args.CreateNewObj logical    = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('Unknown input object type - must be AstroImage or AstroCatalog');
        end

        if Args.AllCols
            IndColX = colname2ind(Cat, Args.ColX);
            IndColY = colname2ind(Cat, Args.ColY);
        else
            [IndColX] = colnameDict2ind(Cat, Args.ColX);
            [IndColY] = colnameDict2ind(Cat, Args.ColY);
        end

        Ncol = numel(IndColX);
        for Icol=1:1:Ncol
            [NewX, NewY] = imUtil.cat.affine2d_transformation(Cat.Catalog(:,[IndColX(Icol), IndColY(Icol)]), AffineMatrix, Direction, 'ThetaUnits',Args.ThetaUnits, 'ColX',1, 'ColY',2);
            Cat.Catalog(:,[IndColX(Icol), IndColY(Icol)]) = [NewX, NewY];
        end

        if isa(Obj, 'AstroImage')
            Result(Iobj).CatData = Cat;
        elseif isa(Obj, 'AstroCatalog')
            Result(Iobj) = Cat;
        else
            error('Unknown input object type - must be AstroImage or AstroCatalog');
        end

    end






end