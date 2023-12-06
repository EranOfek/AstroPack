function [AC,RoundX,RoundY,X,Y]=getSrcStamps(Obj, Args)
    % Given an AstroImage, get a cube of stamps around sources.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'HalfSize' - Half radius of output stamps.
    %                   Default is 12 (i.e., stamps of 25x25).
    %            'X' - X or RA coordinates of sources for which to extract
    %                   stamps.
    %                   If empty, then get coordinates from the
    %                   AstroCatalog in the AstroImage.
    %                   Otherwise this can be in RA/Dec or X/Y units.
    %                   Default is [].
    %            'Y' - Like 'X', but for the Y/Dec coordinate.
    %                   Default is [].
    %            'IsLonLat' - A logical indicating if the vectors of X/Y
    %                   are in RA/Dec (true), or X/Y (false).
    %                   Default is false.
    %            'CooUnits' - If X and Y are in RA/Dec, this indicate the
    %                   units. default is 'deg'.
    %            'IsAstroCube' - Store output in AstroCube.
    %                   Default is false.
    %            See code for additional arguments.
    % Output : - A cube of stamps.
    %            If IsAstroCube=true, then this is an AstroCube object.
    %          - Rounded X coordinates of stamps
    %          - Rounded Y coordinates of stamps
    %          - Requested X coordinates of stamps.
    %          - Requested Y coordinates of stamps.
    % Author : Eran Ofek (Oct 2023)
    % Example: [Cube,RoundX,RoundY,X,Y]=imProc.sources.getSrcStamps(AI);
    
    arguments
        Obj
        
        Args.HalfSize            = 12;
        Args.X                   = [];
        Args.Y                   = [];
        Args.IsLonLat logical    = false;
        Args.CooUnits            = 'deg';
        Args.IsAstroCube logical = false;
        
        Args.ColX                = AstroCatalog.DefNamesX;
        Args.ColY                = AstroCatalog.DefNamesY;
        Args.ColLon              = AstroCatalog.DefNamesRA;
        Args.ColLat              = AstroCatalog.DefNamesDec;
        
        Args.mexCutout logical   = true;
        
        Args.PopMask                  = true;
        Args.PopBack                  = true;
        Args.PopVar                   = true;
        Args.CopyHeader               = true;
        Args.CreateNewHeader logical  = false;
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isempty(Args.X) || isempty(Args.Y)
            % get coordinates from AstroCatalog in AstroImage

            if Args.IsLonLat
                XY = Obj(Iobj).CatData.getLonLat('rad', 'ColLon',Args.ColLon, 'ColLat',Args.ColLat);
                X  = XY(:,1);
                Y  = XY(:,1);
                Units = 'rad';
            else
                XY = Obj(Iobj).CatData.getXY('ColX',Args.ColX, 'ColY',Args.ColY);
                X  = XY(:,1);
                Y  = XY(:,2);
            end

        else
            % user supplied coordinates
            X     = Args.X(:);
            Y     = Args.Y(:);
            Units = Args.CooUnits;
        end
        
        if Args.IsLonLat
            % convert RA/Dec to X/Y
            [X, Y] = Obj(Iobj).WCS.sky2xy(X, Y, 'InUnits',Units);
        end
        
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Obj(Iobj).Image, X, Y, Args.HalfSize, 'mexCutout',Args.mexCutout);
        
        if Args.IsAstroCube
            % AstroCube output
            if Iobj==1
                AC = AstroCube;
            end
            AC(Iobj).ImageData = Cube;
            AC(Iobj).HalfSize  = Args.HalfSize;
            AC(Iobj).X         = RoundX;
            AC(Iobj).Y         = RoundY;
            
            if Args.PopMask && ~isempty(Obj(Iobj).Mask)
                AC(Iobj).MaskData = imUtil.cut.image2cutouts(Obj(Iobj).Mask, X, Y, Args.HalfSize, 'mexCutout',Args.mexCutout);
            end
            if Args.PopBack && ~isempty(Obj(Iobj).Back)
                AC(Iobj).BackData = imUtil.cut.image2cutouts(Obj(Iobj).Back, X, Y, Args.HalfSize, 'mexCutout',Args.mexCutout);
            end
            if Args.PopVar && ~isempty(Obj(Iobj).Var)
                AC(Iobj).VarData = imUtil.cut.image2cutouts(Obj(Iobj).Var, X, Y, Args.HalfSize, 'mexCutout',Args.mexCutout);
            end
            if Args.CopyHeader
                if Args.CreateNewHeader
                    AC(Iobj).HeaderData = Obj(Iobj).HeaderData.copy;
                else
                    AC(Iobj).HeaderData = Obj(Iobj).HeaderData;
                end
            end
            
            %error('AstroCube output is not yet implemented');
        else
            AC = Cube;
        end
        
    end
    

end