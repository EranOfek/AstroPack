function Result = char2int64(X)
    % convert an array of numerical values written as char to int64
    %     Optional detailed description
    % Input  : - an array of numerical values written as char (or double)
    %          * ...,key,val,... 
    % Output : - an array of int64 values 
    % Author : A.M. Krassilchtchikov (2024 Nov) 
    % Example: A = '12345687696786'; A = tools.class.char2int64(A) 
    %
    arguments
        X        
    end
    %
    if strcmpi(class(X),'int64') || prod(isnan(X)) > 0
        Result = X; 
    elseif ischar(X)
        Result = int64(str2num(X)); 
    elseif isnumeric(X)
        Result = int64(X); 
    else
        error('Invalid input class');
    end
end
