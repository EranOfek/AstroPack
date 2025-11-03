function [Result] = astrometrySingleSubImage(AI, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: 

    arguments
        AI
        Args.CCDSEC
        Args.OtherAW                        % AstroWCS or AstroImage
        Args.OtherCCDSEC
        Args.OtherSubCenter         = [];
        Args.SubCenter              = [];
        Args.SucessAW               = [];
        Args.ThresholdIdenticalWCS  = 0.5;  % pix

    end


    AI
    [RefWCS, Iccdsec]=imProc.astrometry.remapWCS(CCDSEC, AI, AI_CCDSEC);


end
