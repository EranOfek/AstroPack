function Y = logApprox(X)
    % Fast mex and approximate (better than 10^-5) log base-e function 
    %   Useing: tools.math.fun.mex.fastLogLUT_double
    %           tools.math.fun.mex.fastLogAVX2_single
    % Input  : - An array
    % Output : - Log base-e of the array.
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example: tools.math.fun.logApprox(71);

    if isa(X,'single')
        Y = tools.math.fun.mex.fastLogAVX2_single(X);
    elseif isa(X,'double')
        Y = tools.math.fun.mex.fastLogLUT_double(X);
    else
        error('Input must be single or double.');
    end

end
