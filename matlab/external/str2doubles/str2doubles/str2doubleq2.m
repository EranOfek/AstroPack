function y = str2doubleq2 (cs)
%STR2DOUBLEQ2:  Wrapper to discard all zero imaginary values in str2doubleq's output.

    y = str2doubleq (cs);
    if ~isreal(y) && all(imag(y(:))==0)
        y = real(y);
    end
end
