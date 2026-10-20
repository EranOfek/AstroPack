function assertVisitMatFileVersion(S, MatPath, Label)
    % Error unless S is v3 visit MAT (InsertDbMatVersion matches visitMatFileVersion).
    Expected = pipeline.last.insertDB.visitMat.visitMatFileVersion();
    if isfield(S, 'InsertDbMatVersion') && S.InsertDbMatVersion == Expected
        return
    end
    if isfield(S, 'Cat') || isfield(S, 'AH')
        Id = 'assertVisitMatFileVersion:LegacyMatFormat';
        Msg = sprintf(['%s is legacy full-object MAT (v1). Re-dump with ', ...
            'visitMat.writeVisitCatalogs2Mat_perf and OverWrite=true. Path: %s'], Label, MatPath);
    elseif isfield(S, 'HdrData') || isfield(S, 'Catalog')
        Id = 'assertVisitMatFileVersion:ObsoleteMatV2';
        Msg = sprintf(['%s is obsolete MAT v2 (cell HdrData/Catalog). Re-dump with ', ...
            'visitMat.writeVisitCatalogs2Mat_perf and OverWrite=true. Path: %s'], Label, MatPath);
    else
        Id = 'assertVisitMatFileVersion:UnsupportedMatVersion';
        Got = 'missing';
        if isfield(S, 'InsertDbMatVersion')
            Got = num2str(S.InsertDbMatVersion);
        end
        Msg = sprintf('%s unsupported InsertDbMatVersion=%s (expected %d) in %s', ...
            Label, Got, Expected, MatPath);
    end
    error(Id, '%s', Msg);
end
