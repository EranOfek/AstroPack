function AH = loadVisitHeaderMat(HdrPath)
    % Load v3 _Hdr.mat and reconstruct AstroHeader array.
    S = load(HdrPath);
    pipeline.last.insertDB.visitMat.assertVisitMatFileVersion(S, HdrPath, '_Hdr');
    HdrKeys = S.HdrKeys(:);
    HdrValStr = S.HdrValStr;
    HdrCommentStr = S.HdrCommentStr;
    N = size(HdrValStr, 1);
    K = size(HdrValStr, 2);
    AH = AstroHeader([1, N]);
    KeyDict = Dictionary.getDict('Header.Synonyms.KeyNames');
    ValDict = Dictionary.getDict('Header.Synonyms.KeyVal.IMTYPE');
    CommentDict = Dictionary.getDict('Header.Comments.Default');
    TimeDict = Dictionary.getDict('Header.Time.KeyNames');
    TimeDict = Dictionary.string2funHandle(TimeDict);
    for Ih = 1:N
        Data = cell(K, 3);
        for J = 1:K
            Data{J, 1} = char(HdrKeys(J));
            Data{J, 2} = headerMatStringToVal_(HdrValStr(Ih, J));
            Data{J, 3} = char(HdrCommentStr(Ih, J));
        end
        AH(Ih).Data = Data;
        AH(Ih).File = '';
        AH(Ih).KeyDict = KeyDict;
        AH(Ih).ValDict = ValDict;
        AH(Ih).CommentDict = CommentDict;
        AH(Ih).TimeDict = TimeDict;
        AH(Ih).IsKeyUpToDate = false;
    end
end

function Val = headerMatStringToVal_(Str)
    if strlength(string(Str)) == 0
        Val = NaN;
        return
    end
    Ch = char(Str);
    Num = str2double(Ch);
    if ~isnan(Num) && ~(contains(Ch, 'T') && contains(Ch, '-'))
        Val = Num;
        return
    end
    Val = Ch;
end
