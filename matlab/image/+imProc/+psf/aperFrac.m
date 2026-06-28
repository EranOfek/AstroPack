function [Result] = aperFrac(AI, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Jun) 
    % Example: 

    arguments
        AI
       
        Args.AddAper                = false;
        Args.AperRad                = [3, 5, 6, 7];
        Args.KeyAperFrac            = 'APER_FRAC_';

    end

end
