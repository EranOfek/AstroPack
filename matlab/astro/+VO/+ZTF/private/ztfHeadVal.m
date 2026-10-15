function Val = ztfHeadVal(Head, Key, Default)
    % Value of a keyword in a merged ZTF header cell.
    % Input  : - Head - [N x 3] cell from ztfReadBinTable.
    %          - Key  - keyword name.
    %          - Default - returned when the keyword is absent. Default NaN.
    % Output : - Val - the keyword value (last occurrence wins, so the
    %            table header overrides the primary one).
    % Author : D. Kovaleva (Oct 2026)

    arguments
        Head
        Key
        Default = NaN
    end

    Ind = find(strcmp(Head(:,1), Key), 1, 'last');
    if isempty(Ind)
        Val = Default;
    else
        Val = Head{Ind, 2};
    end
end
