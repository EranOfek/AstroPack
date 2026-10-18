function Cat = loadVisitCatalogMat(CatPath)
    % Load v2 _Cat.mat and reconstruct AstroCatalog array.
    S = load(CatPath);
    pipeline.last.insertDB.assertVisitMatFileVersion(S, CatPath, '_Cat');
    Catalog = S.Catalog;
    Cat = AstroCatalog(Catalog, 'ColNames', S.ColNames, 'ColUnits', S.ColUnits);
end
