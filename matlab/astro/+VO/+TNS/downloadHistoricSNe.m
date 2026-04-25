function [Result] = downloadHistoricSNe(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Apr) 
    % Example: 

    arguments
        X
        Y
        Args.A                 = [];
        Args.B                 = [];
    end


    for I=1:10
        system(sprintf('wget -U Mozilla https://www.wis-tns.org/search?&isTNS_AT=no&name=SN&num_page=500&format=csv&page=%d',I));
    end

end
