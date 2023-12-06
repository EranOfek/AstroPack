function [Result, ResInd, CatH] = returnMatched_catsHTM(Obj, CatName, Args)
    % Return a catsHTM catalog matched to an AstroCatalog object.
    % Input  : - An AstroCatalog or an AstroImage object (multi
    %            elements supported). The AStroCatalog object will
    %            be matched against a catsHTM catalog.
    %          - catsHTM catalog name (e.g., 'GAIADR3').
    %            See catsHTM.catalogs for possible options.
    %            Alternatively, this can be an Astrocatalog object
    %            containing the catsHTM catalog of the field after applying
    %            proper motion, but before matching.
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
    %            'ApplyPM' - A logical indicating if to correct the
    %                   coordinates in the catsHTM catalog to proper motion and
    %                   parallax. This may work for some catalogs (e.g.,
    %                   'GAIADR3'). Default is true.
    %            'applyProperMotionArgs' - A cell array of arguments to
    %                   pass to imProc.cat.applyProperMotion
    %                   Default is {}.
    %            'ColEpoch' - Column name containing the epoch of the
    %                   catalog. Default is 'Epoch'.
    %            'EpochUnits' - Units of the epoch (in 'ColEpoch').
    %                   See convert.time for option.
    %                   Default is 'J' (i.e., Julian years).
    %
    % Output : - The requested catsHTM catalog matched to the input
    %            catalog. I.e., The number of lines in this catalog are
    %            equal to thre number of lines in the input catalog and the
    %            sources are matched.
    %          - The structure array of matched indices as returned by
    %            imProc.match.matchReturnIndices
    %          - The catsHTM catalog for the field before matching.
    % Author : Eran Ofek (Apr 2021)
    % Example: [Result, ResInd, CatH] = imProc.match.returnMatched_catsHTM(AI, 'GAIADR3');
    %          

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
        
        Args.ApplyPM logical     = true; % will work for GAIA only
        Args.applyProperMotionArgs cell = {};
        Args.ColEpoch                   = 'Epoch';   % epoch col name in catsHTM catalog
        Args.EpochUnits                 = 'J';  % 'J' for Julian years, 'JD' for JD,... see convert.time
        
        %Args.AddColDist logical   = true;
        %Args.ColDistPos           = Inf;
        %Args.ColDistName          = 'Dist';
        %Args.ColDistUnits         = 'arcsec';
        %Args.AddColNmatch logical = true;
        %Args.ColNmatchPos         = Inf;
        %Args.ColNmatchName        = 'Nmatch';
    end

    CatH   = AstroCatalog(size(Obj));  % output of catsHTM
    Result = AstroCatalog(size(Obj));
    Nobj   = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            CatIn = Obj(Iobj).CatData;
            JD    = Obj(Iobj).julday;
        else
            % assuming an AstroCatalog object
            CatIn = Obj(Iobj);
            JD    = Obj(Iobj).JD;
        end
        
        if isempty(Args.Coo) || isempty(Args.CatRadius)
            % get coordinates using boundingCircle
            [CircX, CircY, CircR] = CatIn(Iobj).boundingCircle('OutUnits','rad', 'CooType','sphere');
            Args.Coo                 = [CircX, CircY];
            Args.CatRadius      = CircR;
            Args.CooUnits       = 'rad';
            Args.CatRadiusUnits = 'rad';
        else
            Args.Coo = convert.angular(Args.CooUnits,'rad',Args.Coo);
        end
        
        if isa(CatName, 'AstroCatalog')
            % catalog is already provided
            CatH(Iobj) = CatName(Iobj);
        else
            Icoo = 1;
            CatH(Iobj)  = catsHTM.cone_search(CatName, Args.Coo(Icoo,1), Args.Coo(Icoo,2), Args.CatRadius, 'RadiusUnits',Args.CatRadiusUnits, 'Con',Args.Con, 'OutType','astrocatalog');
        
            % apply PM/plx
            if Args.ApplyPM
                % Get EpochIn from catalog
                EpochIn = CatH(Iobj).getCol(Args.ColEpoch);  % Julian year

                Result = imProc.cat.applyProperMotion(CatH(Iobj), EpochIn(1), JD,    'CreateNewObj',false,...
                                                                                     'EpochInUnits',Args.EpochUnits,...
                                                                                     'EpochOutUnits','jd',...
                                                                                     Args.applyProperMotionArgs{:});
            end
        end
        
        if Args.catsHTMisRef
            ResInd(Iobj) = imProc.match.matchReturnIndices(CatIn, CatH, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        else                                          
            % default!
            ResInd(Iobj) = imProc.match.matchReturnIndices(CatH, CatIn, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits);
        end
        
        
        Result(Iobj) = CatH(Iobj).selectRows(ResInd(Iobj).Obj2_IndInObj1, 'CreateNewObj',true);
       

    end
end
