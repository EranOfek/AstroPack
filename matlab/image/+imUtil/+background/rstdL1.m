function Std = rstdL1(Array, Type, Dim)
    % Robust estimaror of sample std using L1 statustics (abs dev. from mean).
    % Input  : - An array.
    %          - Not used.
    %          - Dimension along to calc the statistcs. Default is 1.
    % Output : - The robust std.
    % Author : Eran Ofek (Nov 2021)
    % Example: Array = rand(1e6,10);
    %          Std = imUtil.background.rstdL1(Array,[],1)
    
    arguments
        Array
        Type    = []; % not used
        Dim     = 1
    end
    
    Std = mean(abs(Array - mean(Array, Dim)), Dim).*1.1547;    
    
end
