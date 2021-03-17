% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html

function funcarg(x,y,maxval,minval)
    arguments
        x (1,:) double
        y (1,:) double
        maxval (1,1) double = max(max(x),max(y))
        minval (1,1) double = min(min(x),min(y))
    end

    % Function code
    disp(x);
    disp(y);
    disp(maxval);
    disp(minval);
end


% You can call this function with any of these syntaxes:

% funcarg(x,y,maxval,minval) 
% funcarg(x,y,maxval) 
% funcarg(x,y) 
