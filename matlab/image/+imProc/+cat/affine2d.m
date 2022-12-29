function Result = affine2d(Obj, AffineMatrix,Direction, Args)
    %
    % Example: AC=AstroCatalog({rand(10,2)},'ColNames',{'X','Y'})
    %          AC=imProc.cat.affine2d(AC, [0 1 10 5]);

    arguments
        Obj
        AffineMatrix
        Direction                    = 1;
        Args.ThetaUnits              = 'deg';
        Args.ColX                    = AstroCatalog.DefNamesX;
        Args.ColY                    = AstroCatalog.DefNamesY;
        Args.AllCols logical         = true;
        Args.CreateNewObj logical    = true;
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
            [NewX, NewY] = imUtil.cat.affine2d_transformation(Cat.Catalog(:,[IndColX(Icol), IndColY(Icol)]), AffineMatrix, Args.ThetaUnits, 'ColX',1, 'ColY',2);
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