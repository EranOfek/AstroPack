% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html
% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html#mw_1b62b6d6-a445-4c55-a9b9-9c70becfdbe6


function funcarg(x, y,args)
    arguments
        x (1,:) double
        y (1,:) double
        args.maxval (1,1) double = max(max(x),max(y))
        args.minval (1,1) double = min(min(x),min(y))
    end

    % Function code
    disp(x);
    disp(y);
    disp(args.maxval);
    disp(args.minval);
    
    s = args;
    disp(s);
    
end


% You can call this function with any of these syntaxes:

% funcarg(x,y,maxval,minval) 
% funcarg(x,y,maxval) 
% funcarg(x,y) 
