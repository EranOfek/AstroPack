# Package: tools.math.symbolic


### tools.math.symbolic.symbolic_poly

Build a symbolic polynomial Package: Util.symbolic Description: Construct a symbolic polynomial with given orders


    
    Build a symbolic polynomial  
    Package: Util.symbolic  
    Description: Construct a symbolic polynomial with given orders  
    Input  : - Vector of orders.  
    - Symbolic variable. Default is X.  
    Output : - Vector of symbolic polynomial orders.  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: SymPoly=Util.symbolic.symbolic_poly([2 3 4],X)  
    Reliable: 2  
      
### tools.math.symbolic.symerror

Calculate symbolic errors Package: Util.symbolic Description: Given a symbolic expression and the variables in the expression, calculate the symbolic error function of the expression, with respect to the variables. The output


    
    Calculate symbolic errors  
    Package: Util.symbolic  
    Description: Given a symbolic expression and the variables in the  
    expression, calculate the symbolic error function of the  
    expression, with respect to the variables. The output  
    error expression contains an error variable named  
    D_"original_var" for each variable in the input expression.  
    Input  : - Symbolic expression.  
    * Arbitrary number of variables to differentiate by.  
    Output : - Symbolic error expression.  
    - Cell array of new error variables.  
    Tested : Matlab 6.5  
    By : Eran O. Ofek                    Aug 2004  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: symerror_calc.m  
    Example: [ErrExp,ErrVar]=symerror('sqrt(x^2+y^2)','x','y')  
    or  
    syms x y  
    [ErrExp,ErrVar]=symerror(sqrt(x^2+y^2),'x','y')  
    or  
    syms x y  
    [ErrExp,ErrVar]=symerror(sqrt(x^2+y^2),x,y)  
    Use: char(vectorize(ErrExp)) to convert the symbolic  
    expression to a vectorized string that can be evaluated  
    using eval. The function also returns a cell array of  
    the new error variables (e.g.,  D_"original_var").  
    Reliable: 2  
      
### tools.math.symbolic.symerror_calc

Calculate and evaluate symbolic errors Package: tools.math.symbolic Description: Given a symbolic expression, the names of the variables in the expression and the value of the variables and errors, calculate the symbolic error function and evaluate it.


    
    Calculate and evaluate symbolic errors  
    Package: tools.math.symbolic  
    Description: Given a symbolic expression, the names of the variables  
    in the expression and the value of the variables and errors,  
    calculate the symbolic error function and evaluate it.  
    Input  : - Symbolic function (e.g., 'sqrt(x^2+y^2)').  
    * Arbitrary triplets of arguments:  
    variable symbol followed by a matrix containing  
    the value and a second matrix containing the errors to  
    plug in each variable.  
    Output : - Matrix of values of the evaluated expression.  
    - Matrix of errors of the evaluated expression.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Mar 2007  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    See also: symerror.m  
    Example: syms x y  
    X = [1:1:10]'; ErrX = 0.1.*[1:1:10]';  
    Y = 6; ErrY = 0.2;  
    [Val,Err,ErrExp,ErrVar] = symerror_calc(sqrt(x^2+sin(y)), 'x',X,ErrX, 'y',Y,ErrY);  
    Reliable: 2  
      
      
### tools.math.symbolic.sympoly2d_2orders

Convert a 2D symbolic polynomials into vectors of orders and coef. Package: Util.symbolic Description: Convert a 2D symbolic polynomials into vectors of orders and coef.


    
    Convert a 2D symbolic polynomials into vectors of orders and coef.  
    Package: Util.symbolic  
    Description: Convert a 2D symbolic polynomials into vectors of orders  
    and coef.  
    Input  : - A symbolic 2D polynomial.  
    - Fisrt symbolic variable. Default is X.  
    - Second symbolic variable. Default is Y'.  
    Output : - A two rows matrix with the orders of the polynomilas.  
    The first line indicate the orders of the X polynomials.  
    The second line indicate the orders of the Y polynomials,  
    corresponding to the X polynomilas.  
    - Vector of coeficiants multiplying the polynomial orders.  
    - Vector of symbolic polynomial childrens.  
    By : Eran O. Ofek                    Jan 2017  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: syms X Y;  
    Poly = 0.1*X^2*Y + 2*X*Y + 3*Y;  
    [OutOrder,OutCoef,ChPoly]=Util.symbolic.sympoly2d_2orders(Poly)  
    Reliable: 2  
      
