function [MasterUniqueCoo, MasterInd, MS] = unify(Obj, Args)
    % Unify multiple catalogs into a single master list of unique sources.
    % Description: Match multiple catalogs by coordinates [RA, Dec] or
    %              [X,Y] and unify them into a single catalog, in which no
    %              source is left behind.
    %              This function is similar to unify, but can optionally
    %              build in parallel another master list using the complement
    %              of ColUse (i.e., the unused sources).
    % Input  : - An AstroCatalog object with multiple elements. Each
    %            element contains a catalog from a specific epoch. The
    %            catalogs should contain either [RA, Dec] or [X,Y]
    %            coordinates.
    %          * ...,key,val,...
    %            'MatchRadius' - Matching radius. Default is 1.5.
    %            'MatchRadiusUnits' - Matching radius units. Default is
    %                   'arcsec'.
    %            'IsSpherical' - If true, use spherical matching with
    %                   [RA, Dec]. If false, use planar matching with [X,Y].
    %                   Default is true.
    %            'ColRADec' - Cell array containing the RA/Dec column names.
    %                   Default is {'RA','Dec'}.
    %            'IsDeg' - If empty, infer angular units from the first
    %                   catalog and convert MatchRadius accordingly. If not
    %                   empty, use the provided value directly and assume
    %                   MatchRadius is already in the correct units.
    %                   Default is [].
    %            'ColXY' - Cell array containing the X/Y column names.
    %                   Default is {'X','Y'}.
    %            'Sort' - If true, sort all catalogs by the second
    %                   coordinate column before matching. Default is false.
    %            'TestSorted' - If true, test that the second list passed
    %                   to the MEX matching function is sorted. Default is
    %                   false.
    %            'ColUse' - Optional column name used as a source mask.
    %                   Sources for which this column is NaN are ignored in
    %                   the regular master list. Default is [].
    %            'AddUnUse' - If true, then ColUse is required and a second
    %                   master list is generated simultaneously using the
    %                   complement mask ~Use. Default is false.
    %            'Col' - Cell array of column names to extract into the
    %                   output MatchedSources object. Default is
    %                   {'RA','Dec','X','Y','FLAGS','MAG_APER_3', ...
    %                    'MAGERR_APER_3','MAG_PSF','MAGERR_PSF'}.
    %                   If AddUnUse is true, you may want to add here also:
    %                   'NITER'.
    % Output : - MasterUniqueCoo matrix [Nunique,2]. Coordinates of the
    %            unique sources. If AddUnUse=true, then the unused-source
    %            master list is appended below the used-source master list.
    %          - MasterInd matrix [Nunique,Nobj]. Row index of each unique
    %            source in each catalog/epoch. Missing sources are marked by
    %            NaN. If AddUnUse=true, then the unused-source matrix is
    %            appended below the used-source matrix.
    %          - A MatchedSources object containing the matched/unified
    %            catalogs, generated only if requested as output.
    % Author : Eran Ofek (2026 Apr)
    % Example:
    %   [MasterUniqueCoo, MasterInd] = unify(Obj, MatchRadius=1.5, ...
    %       MatchRadiusUnits='arcsec');
    %
    %   [MasterUniqueCoo, MasterInd] = unify(Obj, ColUse='FLAGS', ...
    %       AddUnUse=true, MatchRadius=1.5, MatchRadiusUnits='arcsec');

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
        Args.AddUnUse          = false;
        Args.Col               = {'RA','Dec','X','Y','FLAGS','MAG_APER_3','MAGERR_APER_3','MAG_PSF','MAGERR_PSF'};
    end

    Nobj = numel(Obj);
    if Nobj == 0
        MasterUniqueCoo = zeros(0,2);
        MasterInd       = zeros(0,0);
        MS              = [];
        return;
    end

    if Args.AddUnUse && isempty(Args.ColUse)
        error('When AddUnUse=true, ColUse must be provided');
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
    NotUse1 = ~Use1;

    % initialize used master from first catalog
    MasterCoo = Coo(Use1, :);

    % unit handling - keep same/original logic
    if isempty(Args.IsDeg)
        % automatic selection of units based on first catalog
        if Args.IsSpherical
            if all(strcmp(Units, 'deg'))
                IsDeg = true;
                MatchRadius = convert.angular(Args.MatchRadiusUnits, 'deg', Args.MatchRadius);
            else
                IsDeg = false;
                MatchRadius = convert.angular(Args.MatchRadiusUnits, 'rad', Args.MatchRadius);
            end
        else
            IsDeg = false;
            MatchRadius = Args.MatchRadius;
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

    % used branch
    MasterUniqueCoo = nan(InitCap, 2);
    MasterInd       = nan(InitCap, Nobj);

    Nmaster = size(MasterCoo, 1);
    if Nmaster > 0
        MasterUniqueCoo(1:Nmaster, :) = MasterCoo;
        MasterInd(1:Nmaster, Iobj)    = find(Use1);
    end

    % unused branch
    if Args.AddUnUse
        MasterCoo_NU = Coo(NotUse1, :);

        MasterUniqueCoo_NU = nan(InitCap, 2);
        MasterInd_NU       = nan(InitCap, Nobj);

        Nmaster_NU = size(MasterCoo_NU, 1);
        if Nmaster_NU > 0
            MasterUniqueCoo_NU(1:Nmaster_NU, :) = MasterCoo_NU;
            MasterInd_NU(1:Nmaster_NU, Iobj)    = find(NotUse1);
        end
    end

    % process remaining objects
    for Iobj = 2:Nobj
        CooNext = Obj(Iobj).getCol(ColCoo);
        Use2    = getUseMask(Obj(Iobj), Args.ColUse, size(CooNext,1));
        NotUse2 = ~Use2;

        %----- used branch -----
        MasterXRA  = MasterUniqueCoo(1:Nmaster, 1);
        MasterYDec = MasterUniqueCoo(1:Nmaster, 2);

        [Ind1, ~, ~, Ind2] = MatchFun(MasterXRA, MasterYDec, ...
                                      CooNext(:,1), CooNext(:,2), ...
                                      MatchRadius, IsDeg, [], Use2, Args.TestSorted);

        FlagMatch = ~isnan(Ind1);
        if any(FlagMatch)
            MasterInd(FlagMatch, Iobj) = Ind1(FlagMatch);
        end

        FlagNew = Use2 & isnan(Ind2);
        Nnew    = nnz(FlagNew);

        if Nnew > 0
            NewInd  = find(FlagNew);
            NewRows = (Nmaster + 1):(Nmaster + Nnew);

            MasterUniqueCoo(NewRows, :) = CooNext(NewInd, :);
            MasterInd(NewRows, Iobj)    = NewInd;

            Nmaster = Nmaster + Nnew;
        end

        %----- unused branch -----
        if Args.AddUnUse
            MasterXRA_NU  = MasterUniqueCoo_NU(1:Nmaster_NU, 1);
            MasterYDec_NU = MasterUniqueCoo_NU(1:Nmaster_NU, 2);

            [Ind1_NU, ~, ~, Ind2_NU] = MatchFun(MasterXRA_NU, MasterYDec_NU, ...
                                                CooNext(:,1), CooNext(:,2), ...
                                                MatchRadius, IsDeg, [], NotUse2, Args.TestSorted);

            FlagMatch_NU = ~isnan(Ind1_NU);
            if any(FlagMatch_NU)
                MasterInd_NU(FlagMatch_NU, Iobj) = Ind1_NU(FlagMatch_NU);
            end

            FlagNew_NU = NotUse2 & isnan(Ind2_NU);
            Nnew_NU    = nnz(FlagNew_NU);

            if Nnew_NU > 0
                NewInd_NU  = find(FlagNew_NU);
                NewRows_NU = (Nmaster_NU + 1):(Nmaster_NU + Nnew_NU);

                MasterUniqueCoo_NU(NewRows_NU, :) = CooNext(NewInd_NU, :);
                MasterInd_NU(NewRows_NU, Iobj)    = NewInd_NU;

                Nmaster_NU = Nmaster_NU + Nnew_NU;
            end
        end
    end

    % trim unused tail
    MasterUniqueCoo = MasterUniqueCoo(1:Nmaster, :);
    MasterInd       = MasterInd(1:Nmaster, :);

    if Args.AddUnUse
        MasterUniqueCoo_NU = MasterUniqueCoo_NU(1:Nmaster_NU, :);
        MasterInd_NU       = MasterInd_NU(1:Nmaster_NU, :);

        MasterUniqueCoo = [MasterUniqueCoo; MasterUniqueCoo_NU];
        MasterInd       = [MasterInd;       MasterInd_NU];
    end

    if nargout > 2
        MS = imProc.cat.catalog2MatchedSources(AC, 'MasterInd', MasterInd, 'Col', Args.Col, 'CopyJD', true);
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