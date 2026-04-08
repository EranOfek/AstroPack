function [MasterUniqueCoo, MasterInd] = unify(Obj, Args)
    % Unify an AstroCatalog object with multiple catalogs (e.g., different epochs).
    %   This function match multiple catalogs by coordinates (RA, Dec) or
    %   (X,Y) and unify them into a single catalog, in which no object is
    %   left behind.
    %   Assuming the input object contains Nobj elements.
    %   The output are:
    %       A MasterUniqueCoo matrix (two columns [RA, Dec] or [X,Y]).
    %           Containing the coordinates of the unique sources. The
    %           coordinates are from the individual catalog in which the
    %           coordinates first appeared (i.e., not matched).
    %           By definition, the first entries contains all the
    %           coordinates in the first element of the input object.
    %           The number of rows in this matrix is Nunique.
    %       An [Nunique X Nobj] matrix, containing the index of the unique
    %           source in the corresponding epoch. If the source doesn't
    %           appear then a NaN appear in the matrix.
    % Input  : - An AstroCatalog object with multiple elements. Each
    %            element contains a catalog from a specific epoch.
    %            The catalogs should contain [RA, Dec], or [X,Y]
    %            coordinates.
    %          * ...,key,val,...
    %            'MatchRadius' - Matching radius. Default is 1.5.
    %            'MatchRadiusUnits' - Matching radius units.
    %                   Default is 'arcsec'.
    % Output : - MasterUniqueCoo matrix [Nunique,2].
    %          - MasterInd matrix [Nunique,Nobj].
    % Author : Eran Ofek (2026 Apr)
    % Example:
    %   [MasterUniqueCoo, MasterInd] = unify(Obj, MatchRadius=1.5, ...
    %       MatchRadiusUnits='arcsec');

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
    end

    Nobj = numel(Obj);
    if Nobj == 0
        MasterUniqueCoo = zeros(0,2);
        MasterInd       = zeros(0,0);
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
    MasterXRA  = MasterCoo(:,1);
    MasterYDec = MasterCoo(:,2);

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