function saveVisitCatalogMat(CatPath, Cat)
    % Save v3 visit catalog MAT (single CatTable + CropIdx, shared ColNames/ColUnits).
    assertColNamesMatchForSave_(Cat);
    InsertDbMatVersion = pipeline.last.insertDB.visitMat.visitMatFileVersion();
    ColNames = Cat(1).ColNames;
    ColUnits = Cat(1).ColUnits;
    N = numel(Cat);
    CatTable = table();
    for I = 1:N
        if ~isempty(Cat(I).Table)
            Ti = Cat(I).Table;
        else
            Ti = array2table(Cat(I).Catalog, 'VariableNames', ColNames);
        end
        Ti.CropIdx = repmat(uint16(I), height(Ti), 1);
        CatTable = [CatTable; Ti]; %#ok<AGROW>
    end
    save(CatPath, 'InsertDbMatVersion', 'CatTable', 'ColNames', 'ColUnits', '-v7.3');
end

function assertColNamesMatchForSave_(Cat)
    N = numel(Cat);
    if N < 1
        error('saveVisitCatalogMat:EmptyCat', 'Catalog array is empty.');
    end
    Ref = Cat(1).ColNames;
    for I = 2:N
        if ~isequal(Cat(I).ColNames, Ref)
            error('saveVisitCatalogMat:ColNamesMismatch', ...
                'ColNames differ between crops (crop %d vs 1).', I);
        end
    end
end
