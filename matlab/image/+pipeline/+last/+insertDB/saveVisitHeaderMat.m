function saveVisitHeaderMat(HdrPath, AH)
    % Save v2 visit header MAT (HdrData keyword cells only, no AstroHeader graph).
    InsertDbMatVersion = pipeline.last.insertDB.visitMatFileVersion();
    HdrData = arrayfun(@(H) H.Data, AH, 'UniformOutput', false);
    save(HdrPath, 'InsertDbMatVersion', 'HdrData', '-v7.3');
end
