function assertVisitMatFileVersion(S, MatPath, Label)
    % Error unless S is v2 visit MAT (InsertDbMatVersion matches visitMatFileVersion).
    if ~isfield(S, 'InsertDbMatVersion') || S.InsertDbMatVersion ~= pipeline.last.insertDB.visitMatFileVersion()
        if isfield(S, 'Cat') || isfield(S, 'AH')
            Id = 'assertVisitMatFileVersion:LegacyMatFormat';
            Msg = sprintf(['%s is legacy full-object MAT (v1). Re-dump with ', ...
                'writeVisitCatalogs2Mat_perf and OverWrite=true. Path: %s'], Label, MatPath);
        else
            Id = 'assertVisitMatFileVersion:UnsupportedMatVersion';
            Msg = sprintf('%s unsupported InsertDbMatVersion in %s', Label, MatPath);
        end
        error(Id, '%s', Msg);
    end
end
