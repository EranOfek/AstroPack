function [XYfull, Result] = addXYfull(Obj, Args)
    % Add X and Y coordinates in the original (full) image.
    %   Given an AstroImage containing an AstroCatalog with X, Y
    %   coordinates. Convert them to X and Y in the full image based on the
    %   appropriate CCDSEC.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'ColX' - The column name in the catalog, containing the X
    %                   coordinates.
    %                   Default is 'X'.
    %            'ColY' - The column name in the catalog, containing the Y
    %                   coordinates.
    %                   Default is 'Y'.
    %            'ColXfull' - The column name in which to store the X
    %                   coordinates in the original full image.
    %                   Default is 'XFULL'.
    %            'ColYfull' - The column name in which to store the Y
    %                   coordinates in the original full image.
    %                   Default is 'YFULL'.
    %            'KeyCCDSEC' - The header keyword containing the CCDSEC
    %                   defining the section of the cropped image in the
    %                   original full image.
    %                   Default is 'ORIGSEC'.
    %            'CreateNewObj' - A logical indicating if to copy the input
    %                   object before inserting the new columns. If false,
    %                   the input object is modified.
    %                   Default is false.
    % Output : - A two column matrix of [X, Y] full corrdinates.
    %            If input is an array, this is kept only for the last
    %            element.
    %          - An updated AstroImage object containing also the X, Y full
    %            coordinates.
    % Author : Eran Ofek (2026 Jun) 
    % Example: AI = imProc.cat.addXYfull(AI);
    %          [XYfull, AI] = imProc.cat.addXYfull(AI, 'ColX','XWIN_IMAGE',...
    %                                      'ColY','YWIN_IMAGE');


    arguments
        Obj
        Args.ColX              = 'X';
        Args.ColY              = 'Y';
        Args.ColXfull          = 'XFULL';
        Args.ColYfull          = 'YFULL';
        Args.KeyCCDSEC         = 'ORIGSEC';
        
        Args.CreateNewObj      = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    NewColName  = {Args.ColXfull, Args.ColYfull};
    NewColUnits = {'',''};

    Nobj = numel(Obj);
    if Nobj < 1
        XYfull = [];
    end
    for Iobj=1:1:Nobj
        CCDSEC = Result(Iobj).HeaderData.getVal(Args.KeyCCDSEC, 'UseDict',false);
        CCDSEC = imUtil.ccdsec.ccdsecStr2num(CCDSEC);

        XY = Result(Iobj).CatData.getColMulti({Args.ColX, Args.ColY});
        [Xfull, Yfull] = imUtil.ccdsec.xy_crop2full(XY(:,1), XY(:,2), CCDSEC);

        XYfull = [Xfull, Yfull];

        if nargout>1
            Result(Iobj).CatData = Result(Iobj).CatData.insertMultiCol(XYfull, NewColName, NewColUnits);
        end

    end

end
