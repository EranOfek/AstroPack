function [Result, SelObj, ResInd, CatH] = match_catsHTM(Obj, CatName, Args)
    % Match an AstroCatalog object with catsHTM catalog and add columns to catalog.
    % Input  : - An AstroCatalog or an AstroImage object (multi
    %            elements supported). The AStroCatalog object will
    %            be matched against a catsHTM catalog.
    %          - catsHTM catalog name (e.g., 'GAIADR2').
    %            See catsHTM.catalogs for possible options.
    %          * ...,key,val,...
    %            'Coo' - [RA, Dec] of coordinates to search.
    %                   If empty, then will attempt to find this
    %                   from the catalog itself. Default is [].
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
    
    Result = Obj.copy();
    if nargout>1
        SelObj = AstroCatalog(size(Obj));
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

        if Args.catsHTMisRef
            ResInd = imProc.match.matchReturnIndices(Obj, CatH, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        else                                          
            % default!
            ResInd = imProc.match.matchReturnIndices(CatH, Obj, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        end
        
        [Result(Iobj), SelObj] = imProc.match.insertCol_matchIndices(Result(Iobj), ResInd, 'AddColDist',Args.AddColDist,...
                                                                              'ColDistPos',Args.ColDistPos,...
                                                                              'ColDistName',Args.ColDistName,...
                                                                              'ColDistUnits',Args.ColDistUnits,...
                                                                              'AddColNmatch',Args.AddColNmatch,...
                                                                              'ColNmatchPos',Args.ColNmatchPos,...
                                                                              'ColNmatchName',Args.ColNmatchName);
        
        

    end
end
