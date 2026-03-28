function [DecLo, DecHi, RALo, RAHi] = ps1dr2ParseRange(BaseName)
    % Parse Dec range from PS1DR2 filename convention *_S[01-32]_P[01-96].
    % Each file is a Dec slice covering all RA (0-360 deg).
    % S[01-32] x P[01-96] = 3072 slices over Dec [-30, +90].
    %
    % Input  : - BaseName (string), e.g. 'INC0220570ForExtraction_S01_P01'
    % Output : - DecLo [deg], DecHi [deg], RALo [deg], RAHi [deg]
    %
    % Author : Dana Kovaleva + Claude (2026 Mar)

    tokens = regexp(BaseName, '_S(\d+)_P(\d+)', 'tokens');
    if isempty(tokens)
        DecLo = NaN; DecHi = NaN; RALo = NaN; RAHi = NaN;
        return;
    end
    S = str2double(tokens{1}{1});
    P = str2double(tokens{1}{2});

    DecMin   = -30;
    DecMax   =  90;
    Nsections = 32;
    Nparts    = 96;
    Ntotal    = Nsections * Nparts;  % 3072
    StepDeg   = (DecMax - DecMin) / Ntotal;  % ~0.039 deg

    Idx   = (S - 1) * Nparts + (P - 1);
    DecLo = DecMin + Idx * StepDeg;
    DecHi = DecMin + (Idx + 1) * StepDeg;

    RALo = 0;
    RAHi = 360;
end
