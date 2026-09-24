function [MasterUniqueCoo, MasterInd, MS] = unify(Obj, Args)
    % (unify) Unify multiple catalogs into a single master list of unique sources.
    % Description: Match multiple catalogs by coordinates [RA, Dec] or [X,Y]
    %              and unify them into a single catalog, in which no source is
    %              left behind. The function assumes the input object contains
    %              Nobj elements. The output consists of:
    %              1. A MasterUniqueCoo matrix (two columns [RA, Dec] or [X,Y])
    %                 containing the coordinates of the unique sources. The
    %                 coordinates are taken from the catalog in which the source
    %                 first appeared (i.e., was not matched to a previous
    %                 source). By definition, the first entries contain all the
    %                 coordinates in the first element of the input object.
    %              2. An [Nunique X Nobj] matrix containing the row index of
    %                 each unique source in each epoch/catalog. If the source
    %                 does not appear in a given epoch, then NaN is stored.
    %              3. Optionally, a MatchedSources object constructed from the
    %                 unified indexing matrix.
    % Input  : - An AstroCatalog object with multiple elements. Each element
    %            contains a catalog from a specific epoch. The catalogs should
    %            contain either [RA, Dec] or [X,Y] coordinates.
    %          * ...,key,val,...
    %            'MatchRadius' - Matching radius. Default is 1.5.
    %            'MatchRadiusUnits' - Matching radius units. Default is
    %                   'arcsec'.
    %            'IsSpherical' - If true, use spherical matching with
    %                   [RA, Dec]. If false, use planar matching with [X,Y].
    %                   Default is true.
    %            'ColRADec' - Cell array containing the RA/Dec column names.
    %                   Default is {'RA','Dec'}.
    %            'IsDeg' - If empty, infer angular units from the first catalog
    %                   and convert MatchRadius accordingly. If not empty, use
    %                   the provided value directly and assume MatchRadius is
    %                   already in the correct units. Default is [].
    %            'ColXY' - Cell array containing the X/Y column names.
    %                   Default is {'X','Y'}.
    %            'Sort' - If true, sort all catalogs by the second coordinate
    %                   column before matching. Default is false.
    %            'TestSorted' - If true, test that the second list passed to
    %                   the MEX matching function is sorted. Default is false.
    %            'ColUse' - Optional column name used as a source mask. Sources
    %                   for which this column is NaN are ignored. Default is [].
    %            'Col' - Cell array of column names to extract into the output
    %                   MatchedSources object. Default is
    %                   {'RA','Dec','X','Y','FLAGS','MAG_APER_3', ...
    %                    'MAGERR_APER_3','MAG_PSF','MAGERR_PSF'}.
    % Output : - MasterUniqueCoo matrix [Nunique,2]. Coordinates of the unique
    %            sources.
    %          - MasterInd matrix [Nunique,Nobj]. Row index of each unique
    %            source in each catalog/epoch. Missing sources are marked by
    %            NaN.
    %          - A MatchedSources object containing the matched/unified
    %            catalogs, generated only if requested as output.
    % Keywords: match, unify, catalog, coordinates, epochs, AstroCatalog
    % Author  : Eran Ofek (2026 Apr)
    % Remarks : 1. The first catalog defines the initial master list.
    %           2. New sources found in later catalogs are appended to the end
    %              of the master list.
    %           3. Coordinates in MasterUniqueCoo are copied from the catalog
    %              in which the source first appeared.
    %           4. If ColUse is provided, only rows for which ColUse is not NaN
    %              participate in the matching process.
    %           5. In spherical mode, MatchRadius is converted according to
    %              MatchRadiusUnits unless IsDeg is explicitly provided.
    %           6. In planar mode, the XY matching MEX function is used.
    %           7. If Sort is true, all catalogs are sorted by the second
    %              coordinate column before matching.
    %           8. If nargout>2, then the function also calls
    %              imProc.cat.catalog2MatchedSources using the resulting
    %              MasterInd matrix.
    % See also: imUtil.match.mex.matchCatalogs, ...
    %           imUtil.match.mex.matchCatalogsXY, ...
    %           imProc.cat.catalog2MatchedSources
    % Example:
    %   [MasterUniqueCoo, MasterInd] = unify(Obj, MatchRadius=1.5, ...
    %       MatchRadiusUnits='arcsec');
    %
    %   [MasterUniqueCoo, MasterInd, MS] = unify(Obj, MatchRadius=2.0, ...
    %       MatchRadiusUnits='arcsec', Sort=true, TestSorted=true);


    arguments
        Obj
        Args.MatchRadius       = 1.5;
        Args.MatchRadiusUnits  = 'arcsec';
        Args.IsSpherical       = true;
        Args.ColRADec          = {'RA','Dec'};
        Args.IsDeg             = [];
        Args.ColXY             = {'X','Y'};
        Args.Sort              = false;
        Args.TestSorted        = false;
        Args.ColUse            = [];

        Args.Col               = {'RA','Dec','X','Y','FLAGS','MAG_APER_3','MAGERR_APER_3','MAG_PSF','MAGERR_PSF'};
    end

    Nobj = numel(Obj);
    if Nobj == 0
        MasterUniqueCoo = zeros(0,2);
        MasterInd       = zeros(0,0);
        MS              = [];
        return;
    end

    % choose spherical/planar
    if Args.IsSpherical
        ColCoo   = Args.ColRADec;
        MatchFun = @imUtil.match.mex.matchCatalogs;
    else
        ColCoo   = Args.ColXY;
        MatchFun = @imUtil.match.mex.matchCatalogsXY;
    end

    % sort
    if Args.Sort
        Obj.sortrows(ColCoo{2});
    end

    % get coordinates from first catalog
    Iobj = 1;
    [Coo, Units] = Obj(Iobj).getCol(ColCoo);

    % get Use
    if isempty(Args.ColUse)
        Use1 = true(size(Coo,1), 1);
    else
        Use1 = getUseMask(Obj(Iobj), Args.ColUse, size(Coo,1));
    end

    % initialize master from first catalog
    MasterCoo = Coo(Use1, :);
    %MasterXRA  = MasterCoo(:,1);
    %MasterYDec = MasterCoo(:,2);

    % unit handling - keep simple/original logic
    if isempty(Args.IsDeg)
        % automatic selection of units based on first catalog
        if all(strcmp(Units, 'deg'))
            IsDeg = true;
            MatchRadius = convert.angular(Args.MatchRadiusUnits, 'deg', Args.MatchRadius);
        else
            IsDeg = false;
            MatchRadius = convert.angular(Args.MatchRadiusUnits, 'rad', Args.MatchRadius);
        end
    else
        IsDeg = Args.IsDeg;
        MatchRadius = Args.MatchRadius;
    end

    % preallocate full possible size; no further growth needed
    SizeCat = Obj.sizeCatalog;
    if isscalar(SizeCat)
        InitCap = SizeCat;
    else
        InitCap = sum(SizeCat);
    end

    MasterUniqueCoo = nan(InitCap, 2);
    MasterInd       = nan(InitCap, Nobj);

    Nmaster = size(MasterCoo, 1);
    if Nmaster > 0
        MasterUniqueCoo(1:Nmaster, :) = MasterCoo;
        MasterInd(1:Nmaster, Iobj)    = find(Use1);
    end

    % process remaining objects
    for Iobj = 2:Nobj
        CooNext = Obj(Iobj).getCol(ColCoo);
        Use2    = getUseMask(Obj(Iobj), Args.ColUse, size(CooNext,1));

        % active master only
        MasterXRA  = MasterUniqueCoo(1:Nmaster, 1);
        MasterYDec = MasterUniqueCoo(1:Nmaster, 2);

        % Need reverse index to identify new sources in current epoch
        [Ind1, ~, ~, Ind2] = MatchFun(MasterXRA, MasterYDec, ...
                                      CooNext(:,1), CooNext(:,2), ...
                                      MatchRadius, IsDeg, [], Use2, Args.TestSorted);

        % matched sources: fill existing rows for this epoch
        FlagMatch = ~isnan(Ind1);
        if any(FlagMatch)
            MasterInd(FlagMatch, Iobj) = Ind1(FlagMatch);
        end

        % unmatched usable sources in current epoch are new master sources
        FlagNew = Use2 & isnan(Ind2);
        Nnew    = nnz(FlagNew);

        if Nnew > 0
            NewInd = find(FlagNew);
            NewRows = ((Nmaster + 1):(Nmaster + Nnew));

            MasterUniqueCoo(NewRows, :) = CooNext(NewInd, :);
            MasterInd(NewRows, Iobj)    = NewInd;

            Nmaster = Nmaster + Nnew;
        end
    end

    % trim unused tail
    MasterUniqueCoo = MasterUniqueCoo(1:Nmaster, :);
    MasterInd       = MasterInd(1:Nmaster, :);

    if nargout>2
        MS = imProc.cat.catalog2MatchedSources(AC, 'MasterInd',MasterInd, 'Col',Args.Col, 'CopyJD',true);
    end

end


function Use = getUseMask(Obj1, ColUse, Nrow)
    % Return logical use mask for one catalog.
    if isempty(ColUse)
        Use = true(Nrow, 1);
    else
        UseVal = Obj1.getCol(ColUse);
        if isempty(UseVal)
            Use = true(Nrow, 1);
        else
            if isvector(UseVal)
                Use = ~isnan(UseVal(:));
            else
                Use = all(~isnan(UseVal), 2);
            end
        end
    end
end