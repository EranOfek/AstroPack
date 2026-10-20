function AH = loadVisitHeaderMat(HdrPath)
    % Load v3 _Hdr.mat and reconstruct AstroHeader array.
    S = load(HdrPath);
    pipeline.last.insertDB.visitMat.assertVisitMatFileVersion(S, HdrPath, '_Hdr');
    HdrKeys = string(S.HdrKeys(:));
    HdrValStr = string(S.HdrValStr);
    HdrCommentStr = string(S.HdrCommentStr);
    N = size(HdrValStr, 1);
    K = size(HdrValStr, 2);
    AH = AstroHeader([1, N]);
    KeyDict = Dictionary.getDict('Header.Synonyms.KeyNames');
    ValDict = Dictionary.getDict('Header.Synonyms.KeyVal.IMTYPE');
    CommentDict = Dictionary.getDict('Header.Comments.Default');
    TimeDict = Dictionary.getDict('Header.Time.KeyNames');
    TimeDict = string2funHandle(TimeDict);
    KeysCol = cellstr(HdrKeys);
    ValCell = headerMatStringsToValGrid_(HdrValStr);
    CommCell = cellstr(HdrCommentStr);
    for Ih = 1:N
        Data = cell(K, 3);
        Data(:, 1) = KeysCol;
        Data(:, 2) = ValCell(Ih, :)';
        Data(:, 3) = CommCell(Ih, :)';
        AH(Ih).Data = Data;
        AH(Ih).File = '';
        AH(Ih).KeyDict = KeyDict;
        AH(Ih).ValDict = ValDict;
        AH(Ih).CommentDict = CommentDict;
        AH(Ih).TimeDict = TimeDict;
    end
end

function ValCell = headerMatStringsToValGrid_(StrMat)
    % N×K string matrix -> N×K cell (empty -> NaN, numeric FITS values -> double).
    N = size(StrMat, 1);
    K = size(StrMat, 2);
    ValCell = cell(N, K);
    Empty = strlength(StrMat) == 0;
    ValCell(Empty) = {NaN};
    Num = str2double(StrMat);
    Iso = contains(StrMat, 'T') & contains(StrMat, '-');
    UseNum = ~Empty & ~Iso & ~isnan(Num);
    Ch = cellstr(StrMat);
    ValCell(UseNum) = num2cell(Num(UseNum));
    UseChar = ~Empty & ~UseNum;
    ValCell(UseChar) = Ch(UseChar);
end
