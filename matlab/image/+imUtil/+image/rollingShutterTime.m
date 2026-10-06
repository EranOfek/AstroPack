function [CorrJD]=rollingShutterTime(JD, Pos, Args)
    % Correct image time for rooling shutter effects.
    % Input  : - JD to correct.
    %          - Position (row or column) in which the target reside.
    %            In LAST, this is the Y_FULL position.
    %          * ...,key,val,... 
    %            'Offset' - Constant time offset [s].
    %                   Default is 0.205 s (for LAST).
    %            'TimePerLine' - Read time per line.
    %                   Default is 46.2e-6 s (for LAST).
    % Output : - Corrected JD
    % Author : Eran Ofek (2026 Oct) 
    % Example: [CorrJD]=imUtil.image.rollingShutterTime(1,[1 5000])

    arguments
        JD 
        Pos    % Y line in LAST              
        Args.Offset       = 0.205;
        Args.TimePerLine  = 46.2e-6;
    end

    CorrJD = JD - (Args.Offset + Pos.*Args.TimePerLine)/86400;
end