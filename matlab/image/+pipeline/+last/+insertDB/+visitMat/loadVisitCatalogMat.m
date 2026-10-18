function Cat = loadVisitCatalogMat(CatPath)
    % Load v3 _Cat.mat and reconstruct AstroCatalog array.
    S = load(CatPath);
    pipeline.last.insertDB.visitMat.assertVisitMatFileVersion(S, CatPath, '_Cat');
    CatTable = S.CatTable;
    if ~ismember('CropIdx', CatTable.Properties.VariableNames)
        error('loadVisitCatalogMat:MissingCropIdx', 'CatTable missing CropIdx in %s', CatPath);
    end
    CropList = unique(CatTable.CropIdx);
    N = numel(CropList);
    ColNames = S.ColNames;
    ColUnits = S.ColUnits;
    Cat(N) = AstroCatalog;
    for I = 1:N
        Ti = CatTable(CatTable.CropIdx == CropList(I), :);
        Ti = removevars(Ti, 'CropIdx');
        Cat(I) = AstroCatalog([]);
        Cat(I).ColNames = ColNames;
        Cat(I).ColUnits = ColUnits;
        Cat(I).Catalog = Ti;
    end
end
