function Fun = chebyshevFun(Type, Orders)
    % Generate an anonymous function for Chebyshev polynomials (using
    % symbolic functions)
    % Input  : - Chebyshev polynomial kind: 1, 2. Default is 1.
    %          - Vector of sorted orders to sum into the function:
    %            Default is [0 1].
    % Output : - An anonymous function of the Chebyshev
    %            polynomials specified by the orders argument.
    %            The anonymous function is an array that return the
    %            evaluated polynomials of all requested orders.
    % Author : Eran Ofek (Apr 2022)
    % Example: Fun = tools.math.fun.chebyshevFun(1, [0:1:5]);
   
    arguments
        Type      = 1;
        Orders    = [0 1];
    end
    
    switch Type
        case {1, 't','T'}
            ChebyFun = @chebyshevT;
        case {2, 'u','U'}
            ChebyFun = @chebyshevU;
        otherwise
            error('Unknown Chebyshev function type option');
    end
    
    sym('x');
    
    % use Horner polynomials representation (much faster)
    if any(Orders==0)
        Orders = Orders(Orders~=0);
        SymFun = horner(simplify(ChebyFun(Orders, x)));
        StrFun = vectorize(SymFun);
        StrFun = strrep(StrFun,'[','[ones(size(x)), ');
        StrFun = ['@(x)', StrFun];
        Fun    = str2func(StrFun);
        %Fun = matlabFunction(SymFun);
    else
        Orders = Orders(Orders~=0);
        SymFun = horner(simplify(ChebyFun(Orders, x)));
        StrFun = vectorize(SymFun);
        StrFun = ['@(x)', StrFun];
        Fun    = str2func(StrFun);
    end
    
end
