function [MasterUniqueCoo, MasterInd, MS] = unify(Obj, Args)
    % Unify multiple catalogs into a single master list of unique sources.
    % Description: Match multiple catalogs by coordinates [RA, Dec] or
    %              [X,Y] and unify them into a single catalog, in which no
    %              source is left behind.
    %              This function is similar to unify, but can optionally
    %              build in parallel another master list using the complement
    %              of ColUse (i.e., the unused sources).
    % Input  : - An AstroCatalog or AstroImage object with multiple elements. Each
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
    %                   Sources for which have 0 in this column are "used".
    %                   Default is 'FORCED'.
    %            'AddUnUse' - If true, then ColUse is required and a second
    %                   master list is generated simultaneously using the
    %                   complement mask ~Use. Default is false.
    %            'Col' - Cell array of column names to extract into the
    %                   output MatchedSources object. Default is
    %                   {'RA','Dec','X','Y','FLAGS','MAG_APER_3', ...
    %                    'MAGERR_APER_3','MAG_PSF','MAGERR_PSF'}.
    %                   If AddUnUse is true, you may want to add here also:
    %                   'NITER'.
    %            'MinNdet' - Minimum number of epochs/catalogs in which a
    %                   source must be detected in order to be kept in the
    %                   master list. Sources detected in less epochs are
    %                   removed before the [Nunique,Nobj] index matrix is
    %                   allocated, so this argument also controls the peak
    %                   memory consumption. 1 means keep all sources.
    %                   Default is 1.
    %            'InitCap' - Initial number of rows allocated for the master
    %                   list. The list is grown geometrically when needed.
    %                   If empty, use twice the size of the first catalog.
    %                   Default is [].
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
    %   [MasterUniqueCoo, MasterInd] = imProc.match.unify(Obj, MatchRadius=1.5, ...
    %       MatchRadiusUnits='arcsec');
    %
    %   [MasterUniqueCoo, MasterInd] = imProc.match.unify(Obj, ColUse='FLAGS', ...
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
        Args.ColUse            = 'FORCED';
        Args.AddUnUse          = false;
        Args.Col               = {'RA','Dec','X','Y','FLAGS','MAG_APER_3','MAGERR_APER_3','MAG_PSF','MAGERR_PSF'};
        Args.MinNdet           = 1;
        Args.InitCap           = [];
    end

    Nobj = numel(Obj);
    if Nobj == 0
        MasterUniqueCoo = zeros(0,2);
        MasterInd       = zeros(0,0);
        MS              = [];
        return;
    end

    % if Args.AddUnUse && isempty(Args.ColUse)
    %     error('When AddUnUse=true, ColUse must be provided');
    % end

    % choose spherical/planar
    if Args.IsSpherical
        ColCoo   = Args.ColRADec;
        MatchFun = @imUtil.match.mex.matchCatalogs;
    else
        ColCoo   = Args.ColXY;
        MatchFun = @imUtil.match.mex.matchCatalogsXY;
    end

    % sort
    Iobj = 1;
    Cat = Obj(Iobj).getCatData;
    if Args.Sort
        Cat.sortrows(ColCoo{2});
    end

    % get coordinates from first catalog
    [Coo, Units] = Cat.getCol(ColCoo);

    % get Use
    if Args.AddUnUse
        Use1 = getUseMask(Cat, Args.ColUse, size(Coo,1));
    else
        Use1 = true(size(Coo,1), 1);
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

    % The number of unique sources is not known in advance, and the worst
    % case (sum of all catalog sizes) is prohibitive for a large number of
    % epochs. Therefore the master list starts small and is grown
    % geometrically, while the per-epoch row indices are accumulated as
    % (master row, catalog row) pairs. The [Nunique,Nobj] index matrix is
    % materialized only after the master list is complete and filtered.
    SizeCat = Obj.sizeCatalog;
    MaxCap  = sum(SizeCat);
    if isempty(Args.InitCap)
        InitCap = min(MaxCap, max(2.*size(Coo,1), 1024));
    else
        InitCap = min(MaxCap, Args.InitCap);
    end

    PairMaster = cell(Nobj, 1);   % master row indices, per epoch
    PairCat    = cell(Nobj, 1);   % corresponding catalog row indices

    % used branch
    MasterUniqueCoo = nan(InitCap, 2);
    Ndet            = zeros(InitCap, 1);   % number of epochs per master source

    Nmaster = size(MasterCoo, 1);
    if Nmaster > 0
        MasterUniqueCoo(1:Nmaster, :) = MasterCoo;
        Ndet(1:Nmaster)               = 1;
        PairMaster{Iobj} = int32(1:Nmaster).';
        PairCat{Iobj}    = int32(find(Use1));
    end

    % unused branch
    if Args.AddUnUse
        MasterCoo_NU  = Coo(NotUse1, :);
        PairMaster_NU = cell(Nobj, 1);
        PairCat_NU    = cell(Nobj, 1);

        MasterUniqueCoo_NU = nan(InitCap, 2);
        Ndet_NU            = zeros(InitCap, 1);

        Nmaster_NU = size(MasterCoo_NU, 1);
        if Nmaster_NU > 0
            MasterUniqueCoo_NU(1:Nmaster_NU, :) = MasterCoo_NU;
            Ndet_NU(1:Nmaster_NU)               = 1;
            PairMaster_NU{Iobj} = int32(1:Nmaster_NU).';
            PairCat_NU{Iobj}    = int32(find(NotUse1));
        end
    end

    % process remaining objects
    for Iobj = 2:Nobj
        Cat     = Obj(Iobj).getCatData;
        if Args.Sort
            Cat.sortrows(ColCoo{2});
        end

        CooNext = Cat.getCol(ColCoo);
        
        if Args.AddUnUse
            Use2 = getUseMask(Cat, Args.ColUse, size(CooNext,1));
        else
            Use2 = true(size(CooNext,1), 1);
        end
        NotUse2 = ~Use2; %sum(NotUse2)
        %Use2    = getUseMask(Cat, Args.ColUse, size(CooNext,1));
        %NotUse2 = ~Use2;

        %----- used branch -----
        MasterXRA  = MasterUniqueCoo(1:Nmaster, 1);
        MasterYDec = MasterUniqueCoo(1:Nmaster, 2);

        [Ind1, ~, ~, Ind2] = MatchFun(MasterXRA, MasterYDec, ...
                                      CooNext(:,1), CooNext(:,2), ...
                                      MatchRadius, IsDeg, [], Use2, Args.TestSorted);

        FlagMatch = ~isnan(Ind1);
        MatchRows = find(FlagMatch);
        Ndet(MatchRows) = Ndet(MatchRows) + 1;

        FlagNew = Use2 & isnan(Ind2);
        Nnew    = nnz(FlagNew);

        if Nnew > 0
            NewInd  = find(FlagNew);
            NewRows = ((Nmaster + 1):(Nmaster + Nnew)).';

            [MasterUniqueCoo, Ndet] = growMaster(MasterUniqueCoo, Ndet, Nmaster + Nnew, MaxCap);
            MasterUniqueCoo(NewRows, :) = CooNext(NewInd, :);
            Ndet(NewRows)               = 1;

            Nmaster = Nmaster + Nnew;
        else
            NewInd  = zeros(0,1);
            NewRows = zeros(0,1);
        end

        PairMaster{Iobj} = int32([MatchRows;        NewRows]);
        PairCat{Iobj}    = int32([Ind1(FlagMatch);  NewInd]);

        %----- unused branch -----
        if Args.AddUnUse
            MasterXRA_NU  = MasterUniqueCoo_NU(1:Nmaster_NU, 1);
            MasterYDec_NU = MasterUniqueCoo_NU(1:Nmaster_NU, 2);

            [Ind1_NU, ~, ~, Ind2_NU] = MatchFun(MasterXRA_NU, MasterYDec_NU, ...
                                                CooNext(:,1), CooNext(:,2), ...
                                                MatchRadius, IsDeg, [], NotUse2, Args.TestSorted);

            FlagMatch_NU = ~isnan(Ind1_NU);
            MatchRows_NU = find(FlagMatch_NU);
            Ndet_NU(MatchRows_NU) = Ndet_NU(MatchRows_NU) + 1;

            FlagNew_NU = NotUse2 & isnan(Ind2_NU);
            Nnew_NU    = nnz(FlagNew_NU);

            if Nnew_NU > 0
                NewInd_NU  = find(FlagNew_NU);
                NewRows_NU = ((Nmaster_NU + 1):(Nmaster_NU + Nnew_NU)).';

                [MasterUniqueCoo_NU, Ndet_NU] = growMaster(MasterUniqueCoo_NU, Ndet_NU, Nmaster_NU + Nnew_NU, MaxCap);
                MasterUniqueCoo_NU(NewRows_NU, :) = CooNext(NewInd_NU, :);
                Ndet_NU(NewRows_NU)               = 1;

                Nmaster_NU = Nmaster_NU + Nnew_NU;
            else
                NewInd_NU  = zeros(0,1);
                NewRows_NU = zeros(0,1);
            end

            PairMaster_NU{Iobj} = int32([MatchRows_NU;          NewRows_NU]);
            PairCat_NU{Iobj}    = int32([Ind1_NU(FlagMatch_NU); NewInd_NU]);
        end
    end

    % trim unused tail and build the [Nunique,Nobj] index matrix
    [MasterUniqueCoo, MasterInd] = buildMasterInd(MasterUniqueCoo(1:Nmaster,:), Ndet(1:Nmaster),...
                                                  PairMaster, PairCat, Nobj, Args.MinNdet);

    if Args.AddUnUse
        [MasterUniqueCoo_NU, MasterInd_NU] = buildMasterInd(MasterUniqueCoo_NU(1:Nmaster_NU,:), Ndet_NU(1:Nmaster_NU),...
                                                            PairMaster_NU, PairCat_NU, Nobj, Args.MinNdet);

        MasterUniqueCoo = [MasterUniqueCoo; MasterUniqueCoo_NU];
        MasterInd       = [MasterInd;       MasterInd_NU];
    end

    if nargout > 2
        % call it with Obj / Note that the catalog was not changed
        MS = imProc.cat.catalog2MatchedSources(Obj, 'MasterInd', MasterInd, 'Col', Args.Col, 'CopyJD', true);
    end
end


function [Coo, Ndet] = growMaster(Coo, Ndet, NeedRows, MaxCap)
    % Geometrically grow the master coordinates and detection counter.
    Cap = size(Coo, 1);
    if NeedRows > Cap
        NewCap = min(max(2.*Cap, NeedRows), MaxCap);
        Coo(Cap+1:NewCap, :)  = NaN;
        Ndet(Cap+1:NewCap, 1) = 0;
    end
end


function [Coo, Ind] = buildMasterInd(Coo, Ndet, PairMaster, PairCat, Nobj, MinNdet)
    % Build the [Nunique,Nobj] index matrix from the accumulated
    % (master row, catalog row) pairs, keeping only master sources detected
    % in at least MinNdet epochs. The matrix is allocated at its final size.
    Nmaster = size(Coo, 1);
    if MinNdet > 1
        Keep  = Ndet >= MinNdet;
        Nkeep = nnz(Keep);
        Map   = zeros(Nmaster, 1, 'int32');
        Map(Keep) = int32(1:Nkeep);
        Coo   = Coo(Keep, :);
    else
        Nkeep = Nmaster;
        Map   = int32(1:Nmaster).';
    end

    Ind = nan(Nkeep, Nobj);
    for Iobj=1:1:Nobj
        if ~isempty(PairMaster{Iobj})
            Rows  = Map(PairMaster{Iobj});
            Valid = Rows > 0;
            % linear indexing avoids a temporary copy of the column
            Ind(double(Rows(Valid)) + (Iobj-1).*Nkeep) = double(PairCat{Iobj}(Valid));
        end
    end
end


function Use = getUseMask(Cat1, ColUse, Nrow)
    % Return logical use mask for one catalog.
    if isempty(ColUse)
        Use = true(Nrow, 1);
    else
        UseVal = Cat1.getCol(ColUse);
        if isempty(UseVal)
            Use = true(Nrow, 1);
        else
            Use = ~logical(UseVal);   % NOTE THE sign NOT !!! needed for FORCED!
            % if isvector(UseVal)
            %     Use = ~isnan(UseVal(:));
            % else
            %     Use = all(~isnan(UseVal), 2);
            % end
        end
    end
end