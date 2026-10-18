function saveVisitCatalogMat(CatPath, Cat)
    % Save v2 visit catalog MAT (numeric Catalog + shared ColNames/ColUnits).
    assertColNamesMatchForSave_(Cat);
    InsertDbMatVersion = pipeline.last.insertDB.visitMatFileVersion();
    Catalog = arrayfun(@(C) C.Catalog, Cat, 'UniformOutput', false);
    ColNames = Cat(1).ColNames;
    ColUnits = Cat(1).ColUnits;
    save(CatPath, 'InsertDbMatVersion', 'Catalog', 'ColNames', 'ColUnits', '-v7.3');
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
