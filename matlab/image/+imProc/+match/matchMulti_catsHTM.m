function [ResInd, CatH] = matchMulti_catsHTM(Obj, CatName, Args)
    % Match an AstroCatalog object with catsHTM catalog and return (all) multiple matches.
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
    %            'CreateNewObj' -If input is AstroImage, then indicating if
    %                   to create a new copy of the catalog.
    %                   Default is false.
    %            'CheckIsSorted' - Check that catalog is sorted.
    %                   Default is true.
    %            'SortCol' - If catalog is not sorted then sort by this
    %                   catalog. Default is 'Dec'.
    % Output : - The input catalog with added columns for the nearest match
    %            in the catsHTM catalog.
    %          - Select lines only from the input catalog. Only sources
    %            with matches are selected.
    % Author : Eran Ofek (Apr 2021)
    % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
    %          M = imProc.cat.Match;
    %          M.coneSearch(AC,[1 1],'Radius',3600);
    %          [MatchedObj, UnMatchedO

    arguments
        Obj
        CatName     = 'GAIADR3';
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
        Args.CreateNewObj logical = false;


        Args.CheckIsSorted  = true;
        Args.SortCol        = 'Dec';

    end


    if isempty(Args.Coo) || isempty(Args.Radius)
        EstimateCoo = true;
    else
        EstimateCoo = false;
    end


    Nobj = numel(Obj);
    %MatchedObj = AstroCatalog(size(Obj));
    
    
    if nargout>1
        SelObj = AstroCatalog(size(Obj));
    end
    
    %

    CatH = AstroCatalog(size(Obj));  % output of catsHTM
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            % assume AstroImage or AstroZOGY
            Cat = Obj(Iobj).CatData;
        end

        if Args.CheckIsSorted
            if ~Cat.IsSorted
                % sort catalog
                Cat = Cat.sortrows(Args.SortCol);
            end
        end
        
        if EstimateCoo
            % isempty(Args.Coo) || isempty(Args.CatRadius)
            % get coordinates using boundingCircle
            [CircX, CircY, CircR] = Cat.boundingCircle('OutUnits','rad', 'CooType','sphere');
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
            ResInd(Iobj).Ind = imProc.match.matchReturnIndicesMulti(Obj(Iobj), CatH(Iobj), 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        else                                          
            % default!
            ResInd(Iobj).Ind = imProc.match.matchReturnIndicesMulti(CatH(Iobj), Obj(Iobj), 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        end
        
    

    end



end
