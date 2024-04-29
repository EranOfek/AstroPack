function [C4] = stdBias_c4(N)
    % Return the c4 bias in std due to finte sample size
    % Input  : - Sample size.
    % Output : - 
    % Author : Eran Ofek (2024 Apr) 
    % Example: tools.math.stat.stdBias_c4(6)

    % See https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation#:~:text=Bias%20in%20standard%20deviation%20for%20autocorrelated%20data.&text=so%20that%20smaller%20values%20of,standard%20deviation%20would%20be%20unity.
    
    C4 = sqrt(2./(N-1)) .*gamma(N./2)./gamma((N-1)./2);
    
    
end
