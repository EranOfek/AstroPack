function [Result] = withinPoly(Obj, PolyX, PolyY, Args)
    % Select sources, in AstroImage/AstroCatalog, within a convex polygon.
    %   See also: imProc.cat.catsHM_inImage
    % Input  : - AstroImage or AstroCatalog object
    %          - (PolyX) X/RA coordinates of polygon.
    %          - (PolyY) Y/Dec coordinates of polygon.
    %          * ...,key,val,... 
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input object. Default is false.
    %            'CooType' - Coordinates type: 'sphere'|'pix'.
    %                   Default is 'sphere'.
    %            'PolyUnits' - Units of the coordinates for 'sphere CooType
    %                   Default is 'deg'.
    % Output : - An AstroCatalog/AstroImage object with the selected
    %            sources within the convex polygon.
    % Author : Eran Ofek (2025 Dec) 
    % Example: [Result] = imProc.cat.withinPoly(AI,[1 100 100 1],[100 100 1 1], 'CooTYpe','pix');

    arguments
        Obj
        PolyX                  
        PolyY                  
        Args.CreateNewObj      = false;
        Args.CooType           = 'sphere';
        Args.PolyUnits         = 'deg';
        
        %Args.ColRA             = 'RA';
        %Args.ColDec            = 'Dec';
        %Args.ColX              = 'X';
        %Args.ColY              = 'Y';
    end
    RAD = 180./pi;

    % if Args.CreateNewObj
    %     Result = Obj.copy;
    % else
    %     Result = Obj;
    % end
    Result = Obj;

    if isa(Obj, 'AstroImage') || isa(Obj, 'AstroDiff') || isa(Obj, 'AstroZOGY')
        IsAI = true;
    else
        IsAI = false;
    end

    if strcmp(Args.CooType, 'sphere') && strcmp(Args.PolyUnits, 'deg')
        Conv = RAD;
    else
        Conv = 1;
    end

    Nobj=numel(Obj);
    for Iobj=1:1:Nobj
        if IsAI
            Cat = Result(Iobj).CatData;
        else
            Cat = Result(Iobj);
        end

        switch lower(Args.CooType)
            case 'sphere'
                [RA, Dec] = getLonLat(Cat, 'deg');
                %[CD1,CD2,CD3] = celestial.coo.coo2cosined(PolyX,PolyY);
                Flag      = celestial.htm.in_polysphere([RA, Dec]./RAD, [PolyX(:), PolyY(:)]./Conv);

            case 'pix'
                [X, Y] = getXY(Cat);
                Flag   = inpolygon(X, Y, PolyX, PolyY);

            otherwise
                error('Unknown CooType option');
        end

        % bugs - this lines doesnt select , doesnt work for AI

        Cat = selectRows(Cat, Flag, 'CreateNewObj',Args.CreateNewObj);

        if IsAI
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end

    end

end
