function [Result, Flag] = fillEmptyCatColumns(Obj)
    % Give the zero-row catalogs in an object the full column set of the others.
    %     An AstroImage which extracted no sources at all ends up with a
    %     catalog that has neither rows nor columns. Such a catalog cannot be
    %     represented as a FITS binary table (matlab.io.fits.createTbl requires
    %     a non-empty TTYPE), so no catalog product can be saved for it, while
    %     a catalog with columns and no rows is a valid product which records
    %     that the image was processed and yielded nothing - e.g., an image
    %     whose background estimation failed (issue #1226).
    %     Every zero-row catalog whose column set differs from the reference
    %     one is replaced by an empty catalog with the reference columns. This
    %     includes catalogs which got only a few columns from a later stage
    %     run on the empty catalog (e.g., XFULL/YFULL by imProc.cat.addXYfull;
    %     issue #1332).
    %     The reference is the catalog with rows and the most columns (the
    %     first one on ties), so a catalog missing some columns (e.g., after a
    %     failed astrometry) is not taken as the reference. If no catalog has
    %     rows, the one with the most columns is used. If no element has
    %     columns there is nothing to copy and the object is returned
    %     unchanged. Catalogs with rows are never modified.
    % Input  : - An AstroImage object (multi elements supported).
    % Output : - The object, with the zero-row catalogs replaced by empty
    %            catalogs with the columns of the reference catalog.
    %            The input object is modified in place (handle class).
    %          - An array of logicals, the size of the input object, which is
    %            true for the elements whose catalog was filled.
    % Author : A.M. Krassilchtchikov (Aug 2026)
    % Example: [AllSI, Filled] = imProc.cat.fillEmptyCatColumns(AllSI);

    arguments
        Obj AstroImage
    end

    Result = Obj;
    Flag   = false(size(Obj));

    Nrow = arrayfun(@(AI) size(AI.CatData.Catalog, 1), Obj);
    Ncol = arrayfun(@(AI) numel(AI.CatData.ColNames), Obj);
    if all(Ncol==0, 'all')
        % no columns anywhere to fill them from
        return
    end

    % reference: the most columns among the catalogs with rows, else among all
    NcolRef = Ncol;
    if any(Nrow>0, 'all')
        NcolRef(Nrow==0) = -1;
    end
    [~, Iref] = max(NcolRef(:));
    RefCat    = Obj(Iref).CatData;
    RefNames  = RefCat.ColNames(:).';

    Flag = Nrow==0 & arrayfun(@(AI) ~isequal(AI.CatData.ColNames(:).', RefNames), Obj);
    if ~any(Flag, 'all')
        return
    end

    if istable(RefCat.Catalog)
        EmptyCat = RefCat.Catalog([],:);
    else
        EmptyCat = zeros(0, numel(RefCat.ColNames), 'like',RefCat.Catalog);
    end

    for Iobj=find(Flag(:)).'
        Result(Iobj).CatData.Catalog  = EmptyCat;
        Result(Iobj).CatData.ColNames = RefCat.ColNames;
        Result(Iobj).CatData.ColUnits = RefCat.ColUnits;
    end
end
