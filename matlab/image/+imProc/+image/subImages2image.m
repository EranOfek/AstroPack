function [Result] = subImages2image(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 May) 
    % Example: imProc.image.subImages2image

    arguments
        AI
        Args.KeyOrigUniqueCCDSEC  = 'ORIGUSEC';
        Args.KeyUniqueCCDSEC      = 'UNIQSEC';
    end

    Nai = numel(AI);

    % read all CCDSEC
    if isnumeric(Args.KeyOrigUniqueCCDSEC) && isnumeric(Args.KeyUniqueCCDSEC)
        OrigUniqueCCDSEC = Args.KeyOrigUniqueCCDSEC;
        UniqueCCDSEC     = Args.KeyUniqueCCDSEC;
    else
        KeySt = AI.getStructKey({Args.KeyOrigUniqueCCDSEC, Args.KeyUniqueCCDSEC});
        if Iai=1:1:Nai
            OrigUniqueCCDSEC = imUtil.ccdsec.ccdsecStr2num(KeySt(Iai).(Args.KeyOrigUniqueCCDSEC));
            UniqueCCDSEC     = imUtil.ccdsec.ccdsecStr2num(KeySt(Iai).(Args.KeyUniqueCCDSEC));
        end
    end

    % combine the images

    % combine the Back

    % combine the Var

    % combine the CatData

    % Fix the WCS
    


end

