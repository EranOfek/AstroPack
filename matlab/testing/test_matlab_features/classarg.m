% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html
% https://www.mathworks.com/help/matlab/matlab_prog/function-argument-validation-1.html#mw_1b62b6d6-a445-4c55-a9b9-9c70becfdbe6

classdef classarg < handle
    
    properties
        maxval = -1;
        minval = -1;
    end

    
    methods
        function Obj = classarg()
            % Constructor
        end
        
        
        function Result = test(Obj, x, y, args)
            arguments
                Obj
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

            % Copy args to properties
            fn = fieldnames(args);
            for k=1:numel(fn)
                disp(fn{k});
                if isprop(Obj, fn{k})
                    Obj.(fn{k}) = args.(fn{k});
                end
            end

            disp(Obj.maxval);
            disp(Obj.minval);
    
            Result = true;
        end

    end
end


    
% You can call this function with any of these syntaxes:

% funcarg(x,y,maxval,minval) 
% funcarg(x,y,maxval) 
% funcarg(x,y) 
