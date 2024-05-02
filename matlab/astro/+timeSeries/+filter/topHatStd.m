function [Result] = topHatStd(X, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 May) 
    % Example: 

    arguments
        X
        Args.Dim               = 1;
        Args.B                 = [];
    end

    if Args.Dim==2
        X = X.';
        Args.Dim = 1;
    end
    
    
    
end
