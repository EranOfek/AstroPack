function saveVisitHeaderMat(HdrPath, AH)
    % Save v3 visit header MAT (shared HdrKeys + N×K string value/comment matrices).
    N = numel(AH);
    if N < 1
        error('saveVisitHeaderMat:EmptyHdr', 'Header array is empty.');
    end
    K = size(AH(1).Data, 1);
    HdrKeys = string(AH(1).Data(:, 1));
    for I = 2:N
        if size(AH(I).Data, 1) ~= K
            error('saveVisitHeaderMat:HdrRowCountMismatch', ...
                'Header row count differs (crop %d vs 1).', I);
        end
        KeysI = string(AH(I).Data(:, 1));
        if ~isequal(KeysI, HdrKeys)
            error('saveVisitHeaderMat:HdrKeysMismatch', ...
                'Header keyword list differs (crop %d vs 1).', I);
        end
    end
    HdrValStr = strings(N, K);
    HdrCommentStr = strings(N, K);
    for I = 1:N
        for J = 1:K
            HdrValStr(I, J) = headerMatValToString_(AH(I).Data{J, 2});
            HdrCommentStr(I, J) = headerMatValToString_(AH(I).Data{J, 3});
        end
    end
    InsertDbMatVersion = pipeline.last.insertDB.visitMat.visitMatFileVersion();
    save(HdrPath, 'InsertDbMatVersion', 'HdrKeys', 'HdrValStr', 'HdrCommentStr', '-v7.3');
end

function S = headerMatValToString_(Val)
    if isempty(Val)
        S = "";
        return
    end
    if isnumeric(Val)
        if isscalar(Val) && isnan(Val)
            S = "";
        else
            S = string(Val);
        end
        return
    end
    if islogical(Val)
        S = string(Val);
        return
    end
    S = string(Val);
end
