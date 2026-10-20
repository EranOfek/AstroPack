function Cat = loadVisitCatalogMat(CatPath)
    % Load v3 _Cat.mat and reconstruct AstroCatalog array (numeric matrices per crop).
    S = load(CatPath);
    pipeline.last.insertDB.visitMat.assertVisitMatFileVersion(S, CatPath, '_Cat');
    CatTable = S.CatTable;
    if ~ismember('CropIdx', CatTable.Properties.VariableNames)
        error('loadVisitCatalogMat:MissingCropIdx', 'CatTable missing CropIdx in %s', CatPath);
    end
    CropIdx = CatTable.CropIdx;
    DataTable = removevars(CatTable, 'CropIdx');
    Mat = table2array(DataTable);
    CropList = unique(CropIdx);
    N = numel(CropList);
    Cells = cell(N, 1);
    for I = 1:N
        Cells{I} = Mat(CropIdx == CropList(I), :);
    end
    ColNames = cellstr(S.ColNames);
    ColUnits = cellstr(S.ColUnits);
    Cat = AstroCatalog(Cells, 'ColNames', ColNames, 'ColUnits', ColUnits);
end
