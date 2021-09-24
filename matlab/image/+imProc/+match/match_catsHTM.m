function [Result, SelObj, ResInd, CatH] = match_catsHTM(Obj, CatName, Args)
    % Match an AstroCatalog object with catsHTM catalog
    % Input  : - An AstroCatalog or an AstroImage object (multi
    %            elements supported). The AStroCatalog object will
    %            be matched against a catsHTM catalog.
    %          - catsHTM catalog name (e.g., 'GAIADR2').
    %            See catsHTM.catalogs for possible options.
    %          * ...,key,val,...
    %            'Coo' - [RA, Dec] of coordinates to search.
    %                   If empty, then will attempt to find this
    %                   from the catalog itself. DEfault is [].
    %            'CooUnits' - Units of coordinates. Object default
    %                   is 'deg'.
    %            'Radius' - Matching radius. Default is 3.
    %            'RadiusUnits' - Matchin radius units.
    %                   Default is 'arcsec'.
    %            'CatRadius' - The search radius of the catsHTM
    %                   catalog. If not given this is taken as the
    %                   bounding circle radius of the inout
    %                   AstroCatalog. Default is [].
    %            'CatRadiusUnits' - CatRadius units.
    %                   Default is 'arcsec'.
    %            'Con' - A cell array of additional
    %                  constraints to apply to output catalog.
    %                  See catsHTM.cone_search for options.
    %                  E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}
    %                  Default is {}.
    %            'catsHTMisRef' - A logical indicating if the
    %                   catsHTM catalog is treated as the reference
    %                   catalog. Default is false.
    %                   If true, then the output is the same but for the catsHTM catalog.
    %            'AddColDist' - Default is true.
    %            'ColDistPos' - Default is Inf.
    %            'ColDistName' - Default is 'Dist'.
    %            'ColDistUnits' - Default is 'arcsec'.
    %            'AddColNmatch' - Default is true.
    %            'ColNmatchPos' - Default is Inf.
    %            'ColNmatchName' - Default is 'Nmatch'.
    % Output : - The input catalog with added columns for the nearest match
    %            in the catsHTM catalog.
    %          - Select lines only from the input catalog. Only sources
    %            with matches are selected.
    % Author : Eran Ofek (Apr 2021)
    % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
    %          M = imProc.cat.Match;
    %          M.coneSearch(AC,[1 1],'Radius',3600);
    %          [MatchedObj, UnMatchedObj, TruelyUnMatched, CatH] = M.match_catsHTM(AC,'GAIADR2')

    arguments
        Obj
        CatName char
        Args.Coo                 = [];
        Args.CooUnits            = 'deg';
        Args.Radius              = 3;
        Args.RadiusUnits         = 'arcsec';
        Args.CatRadius           = [];
        Args.CatRadiusUnits      = 'arcsec';
        Args.Con                 = {};
        Args.catsHTMisRef        = false;
        
        Args.AddColDist logical   = true;
        Args.ColDistPos           = Inf;
        Args.ColDistName          = 'Dist';
        Args.ColDistUnits         = 'arcsec';
        Args.AddColNmatch logical = true;
        Args.ColNmatchPos         = Inf;
        Args.ColNmatchName        = 'Nmatch';
    end

    % convert AstroImage to AstroCatalog
    if isa(Obj,'AstroImage')
        Obj = astroImage2AstroCatalog(Obj,'CreateNewObj',Args.CreateNewObj);
    elseif isa(Obj,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj)
        error('Input Obj is of unsupported class');
    else
        error('Input Obj is of unsupported class');
    end


    if isempty(Args.Coo) || isempty(Args.Radius)
        UseUserCoo = true;
    else
        UseUserCoo = false;
    end


    Nobj = numel(Obj);
    MatchedObj = AstroCatalog(size(Obj));
    
    Result     = Obj.copyObject;
    if nargout>1
        SelObj     = AstroCatalog(size(Obj));
    end
    
    CatH = AstroCatalog(size(Obj));  % output of catsHTM
    for Iobj=1:1:Nobj
        if isempty(Args.Coo) || isempty(Args.CatRadius)
            % get coordinates using boundingCircle
            [CircX, CircY, CircR] = Obj(Iobj).boundingCircle('OutUnits','rad');
            Args.Coo                 = [CircX, CircY];
            Args.CatRadius      = CircR;
            Args.CooUnits       = 'rad';
            Args.CatRadiusUnits = 'rad';
        else
            Args.Coo = convert.angular(Args.CooUnits,'rad',Args.Coo);
        end
        Icoo = 1;
        CatH(Iobj)  = catsHTM.cone_search(CatName, Args.Coo(Icoo,1), Args.Coo(Icoo,2), Args.CatRadius, 'RadiusUnits',Args.CatRadiusUnits, 'Con',Args.Con, 'OutType','astrocatalog');

        if catsHTMisRef
            ResInd = imProc.match.matchReturnIndices(Obj, CatH, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.radiusUnits);
        else                                          
            % default!
            ResInd = imProc.match.matchReturnIndices(CatH, Obj, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.radiusUnits);
        end
        
        if nargout>1
            % catalog of selected sources in Obj that has matches in CatH [size like CatH)]
            FlagNN = ~isnan(ResInd.Obj2_IndInObj1);
            SelObj(Iobj) = selectRows(Obj(Iobj), IndObj);
            % add Dist/Nmatch to SelObj
            if Args.AddColDist
                DistData     = convert.angular('rad', Args.ColDistUnits, ResInd.Obj2_Dist(FlagNN));
                SelObj(Iobj) = insertCol(SelObj(Iobj), DistData, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
            end
            if Args.AddColNmatch
                SelObj(Iobj) = insertCol(SelObj(Iobj), ResInd.Obj2_NmatchObj1(FlagNN) , Args.ColNmatchPos, Args.ColNmatchName, '');
            end
        end
        
        % Obj, but with extra columns indicating if there is a match in CatH
        if Args.AddColDist
            DistData     = convert.angular('rad', Args.ColDistUnits, ResInd.Obj2_Dist(FlagNN));
            Result(Iobj) = insertCol(Result(Iobj), ResInd.Obj2_Dist, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
        end
        if Args.AddColNmatch
            Result(Iobj) = insertCol(Result(Iobj), ResInd.Obj2_NmatchObj1 , Args.ColNmatchPos, Args.ColNmatchName, '');
        end
        
        
%         % match sources
%         if Args.catsHTMisRef
%             [MatchedObj, UnMatchedObj, TruelyUnMatched] = imProc.match.match(Obj, CatH, 'Radius',Args.Radius, 'RadiusUnits',Args.RadiusUnits);
%         else
%             [MatchedObj, UnMatchedObj, TruelyUnMatched] = imProc.match.match(CatH, Obj, 'Radius',Args.Radius, 'RadiusUnits',Args.RadiusUnits);
%         end

    end
end
