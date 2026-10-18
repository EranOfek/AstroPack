function AH = loadVisitHeaderMat(HdrPath)
    % Load v2 _Hdr.mat and reconstruct AstroHeader array.
    S = load(HdrPath);
    pipeline.last.insertDB.assertVisitMatFileVersion(S, HdrPath, '_Hdr');
    HdrData = S.HdrData;
    N = numel(HdrData);
    AH = AstroHeader([1, N]);
    KeyDict = Dictionary.getDict('Header.Synonyms.KeyNames');
    ValDict = Dictionary.getDict('Header.Synonyms.KeyVal.IMTYPE');
    CommentDict = Dictionary.getDict('Header.Comments.Default');
    TimeDict = Dictionary.getDict('Header.Time.KeyNames');
    TimeDict = Dictionary.string2funHandle(TimeDict);
    for Ih = 1:N
        AH(Ih).Data = HdrData{Ih};
        AH(Ih).File = '';
        AH(Ih).KeyDict = KeyDict;
        AH(Ih).ValDict = ValDict;
        AH(Ih).CommentDict = CommentDict;
        AH(Ih).TimeDict = TimeDict;
        AH(Ih).IsKeyUpToDate = false;
    end
end
