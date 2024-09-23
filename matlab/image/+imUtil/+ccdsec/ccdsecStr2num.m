function [NumCCDSEC] = ccdsecStr2num(StrCCDSEC)
    % Convert CCDSEC in string format to [Xmin, Xmax, Ymin, Ymax] matrix.
    % Input  : - CCDSEC in string format.
    %            Either char array, or a strings array, or a cell array of
    %            chars.
    % Output : - A matrix of [Xmin, Xmax, Ymin, Ymax]
    % Author : Eran Ofek (2024 Sep) 
    % Example: [NumCCDSEC] = imUtil.ccdsec.ccdsecStr2num('[1 100 2 101]')
    %          [NumCCDSEC] = imUtil.ccdsec.ccdsecStr2num(["[1 100 2 101]","[2 10 3 20]"])
    %          [NumCCDSEC] = imUtil.ccdsec.ccdsecStr2num({'[1 100 2 101]','[2 10 3 20]'})

    if ischar(StrCCDSEC)
        NumCCDSEC = ccdsecChar2num(StrCCDSEC);
    else
        % strings or cell
        N = numel(StrCCDSEC);
        NumCCDSEC = zeros(N,4);
        for I=1:1:N
            NumCCDSEC(I,:) = ccdsecChar2num(StrCCDSEC{I});
        end
    end
end


function [Num]=ccdsecChar2num(CharCCDSEC)
    % Char array of CCDSEC to numeric format

    Num = sscanf(CharCCDSEC, '[%d %d %d %d]').';
end
