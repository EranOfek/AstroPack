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
    Tables = cell(1, N);
    for I = 1:N
        Ti = CatTable(CatTable.CropIdx == CropList(I), :);
        Ti.CropIdx = [];
        Tables{I} = Ti;
    end
    Cat = AstroCatalog(Tables, 'ColNames', S.ColNames, 'ColUnits', S.ColUnits);
end
