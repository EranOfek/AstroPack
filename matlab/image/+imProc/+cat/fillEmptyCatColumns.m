function [Result, Flag] = fillEmptyCatColumns(Obj)
    % Give the column-less catalogs in an object the column set of the others.
    %     An AstroImage which extracted no sources at all ends up with a
    %     catalog that has neither rows nor columns. Such a catalog cannot be
    %     represented as a FITS binary table (matlab.io.fits.createTbl requires
    %     a non-empty TTYPE), so no catalog product can be saved for it, while
    %     a catalog with columns and no rows is a valid product which records
    %     that the image was processed and yielded nothing - e.g., an image
    %     whose background estimation failed (issue #1226).
    %     The columns are copied from the first element which has them, so the
    %     empty catalogs match the ones actually written alongside them. If no
    %     element has columns there is nothing to copy and the object is
    %     returned unchanged.
    % Input  : - An AstroImage object (multi elements supported).
    % Output : - The object, with the column-less catalogs replaced by empty
    %            (zero rows) catalogs with the columns of the other elements.
    %            The input object is modified in place (handle class).
    %          - An array of logicals, the size of the input object, which is
    %            true for the elements whose catalog was filled.
    % Author : A.M. Krassilchtchikov (Aug 2026)
    % Example: [AllSI, Filled] = imProc.cat.fillEmptyCatColumns(AllSI);

    arguments
        Obj AstroImage
    end

    Result = Obj;

    Flag = arrayfun(@(AI) isempty(AI.CatData.ColNames), Obj);
    if ~any(Flag, 'all') || all(Flag, 'all')
        % nothing to fill, or no columns anywhere to fill them from
        Flag = false(size(Obj));
        return
    end

    RefCat = Obj(find(~Flag, 1)).CatData;
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
