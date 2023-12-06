function Wave = eqSampling(Wave1, Wave2)
    % Given two vectors of wavelength, return the dense one and truncate it
    % according to the overlapping range between the two vectors.
    % Input  : - Vector
    %          - Vector
    % Outpu  : - The best sampled overlappiing vector
    % Author : Eran Ofek (Oct 2021)
    % Eaxmple: Wave1 = (1:1:100).'; Wave2 = (11:0.5:20).';
    %          Wave = astro.spec.eqSampling(Wave1, Wave2)
    %          Wave = astro.spec.eqSampling(Wave1, Wave2, false)
    
    arguments
        Wave1
        Wave2
    end
    
    Den1 = numel(Wave1)./(Wave1(end) - Wave1(1));
    Den2 = numel(Wave2)./(Wave2(end) - Wave2(1));
    
    if Den1>Den2
        % equalize by Wave1
        Wave = Wave1;
    else
        Wave = Wave2;
    end
        
    if Wave1(1)>Wave2(1)
        StartW = Wave1(1);
    else
        StartW = Wave2(1);
    end
    if Wave1(end)>Wave2(end)
        EndW   = Wave2(end);
    else
        EndW   = Wave1(end);
    end
    Wave = Wave(Wave>=StartW & Wave<=EndW);
end
