function [X,Y] = center_ccdsec(CCDSEC)
    % Calculate the [X, Y] centers of lines in CCDSEC
    % Input  : - A 4 column matrix of CCDSEC, one per line.
    % Output : - The X center of the CCDSEC (each entry corresponds to a
    %            line in the input CCDSEC).
    %          - The Y center of the CCDSEC.
    % Author : Eran Ofek (Aug 2021)
    % Example: [X,Y] = imUtil.ccdsec.center_ccdsec([1 100 1 201; 1 10 1 11])
    
    arguments
        CCDSEC(:,4)
    end
    
    X = 0.5.*(CCDSEC(:,1) + CCDSEC(:,2));
    Y = 0.5.*(CCDSEC(:,3) + CCDSEC(:,4));
   
end