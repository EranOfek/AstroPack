# Package: tools.math.integral


### tools.math.integral.QuarticSolver

Quartic integral solver: [x1,x2,x3,x4]=QuarticSolver(a,b,c,d,e) Package: Util.integral Description: Quartic integral solver. v.0.31 - Added some correction detected from QuarticSolverVec Changed logic of ChosenSet to accomudate simultaneous convergence of sets 1 & 2


    
    Quartic integral solver: [x1,x2,x3,x4]=QuarticSolver(a,b,c,d,e)  
    Package: Util.integral  
    Description: Quartic integral solver.  
    v.0.31 - Added some correction detected from QuarticSolverVec  
    Changed logic of ChosenSet to accomudate simultaneous convergence of sets 1 & 2  
    - quickly tanslates the input parameters [a,b,c,d,e] to the reference paper parameters [1,a,b,c,d] for consistency  
    - corrected typo in alpha01/beta01 definitions  
    - corrected typos in AnalyticalSolution, added special case  
    - Note the periodicity in nearly-convergent solutions can other  
    than four (related to text on step 4 after table 3). examples:  
    period of 5: [a,b,c,d,e]=[0.111964240308252 -0.88497524334712 -0.197876116344933 -1.07336408259262 -0.373248675102065];  
    period of 6: [a,b,c,d,e]=[-1.380904438798326 0.904866918945240 -0.280749330818231 0.990034312758900 1.413106456228119];  
    period of 22: [a,b,c,d,e]=[0.903755513939902 0.490545114637739 -1.389679906455410 -0.875910689438623 -0.290630547104907];  
    Therefore condition was changed from epsilon1(iter)0 to epsilon1(iter)<8*eps (and similarl for epsilon2)  
    =  
    - Solves for the x1-x4 roots of the quartic equation y(x)=ax^4+bx^3+cx^2+dx+e.  
    - The function always returns four values. Multiple roots, if exist, are given multiple times. No roots will result in four NaN values  
    No convergence will result in four inf values.  
    Input : - A  
    - B  
    - C  
    - D  
    - E  
    Output: - X1  
    - X2  
    - X3  
    - X4  
    - X5  
      
    Reference:  
    PeterStrobach (2010), Journal of Computational and Applied Mathematics 234  
    http://www.sciencedirect.com/science/article/pii/S0377042710002128  
    By    : Aviv Ofir                  Oct 2017  
    Reliable: 1  
      
      
### tools.math.integral.QuarticSolverVec

Quartic integral solver (vectorized): [x1, x2, x3, x4]=QuarticSolverVec(a,b,c,d,e) Package: Util.integral Description: Quartic integral solver (vectorized) v.0.1 - Nearly identical to QuarticSolver v. 0.4, the first successful vectorized implimentation Changed logic of ChosenSet to accomudate simultaneous convergence of sets 1 & 2


    
    Quartic integral solver (vectorized): [x1, x2, x3, x4]=QuarticSolverVec(a,b,c,d,e)  
    Package: Util.integral  
    Description: Quartic integral solver (vectorized)  
    v.0.1 - Nearly identical to QuarticSolver v. 0.4, the first successful vectorized implimentation  
    Changed logic of ChosenSet to accomudate simultaneous convergence of sets 1 & 2  
    - Note the periodicity in nearly-convergent solutions can other  
    than four (related to text on step 4 after table 3). examples:  
    period of 5: [a,b,c,d,e]=[0.111964240308252 -0.88497524334712 -0.197876116344933 -1.07336408259262 -0.373248675102065];  
    period of 6: [a,b,c,d,e]=[-1.380904438798326 0.904866918945240 -0.280749330818231 0.990034312758900 1.413106456228119];  
    period of 22: [a,b,c,d,e]=[0.903755513939902 0.490545114637739 -1.389679906455410 -0.875910689438623 -0.290630547104907];  
    Therefore condition was changed from epsilon1(iter)0 to epsilon1(iter)<8*eps (and similarl for epsilon2)  
    - Special case criterion of the analytical formula was changed to  
    ind=abs(4*Delta0.^3./Delta1.^2)<2*eps;  (instead of exact zero)  
    - vectorized  
      
    - Solves for the x1-x4 roots of the quartic equation y(x)=ax^4+bx^3+cx^2+dx+e.  
    Multiple eqations can be soved simultaneously by entering same-sized column vectors on all inputs.  
    - Note the code immediatly tanslates the input parameters ["a","b","c","d","e"] to the reference paper parameters [1,a,b,c,d] for consistency,  
    and the code probably performes best when "a"=1.  
    Input  : - A  
    - B  
    - C  
    - D  
    - E  
    Output : * The four free output parameters are the polynomial roots.  
    The function always returns four (possibly complex) values. Multiple roots, if exist, are given multiple times. An error will result in four NaN values.  
    No convergence will result in four inf values (still?)  
    - epsilon    : the sum of the magnitudes of the correction steps on of the last itereation (should be a small number)  
    - ValueAtRoot: the value of the polynimial evaluated at the roots (AFTER division by "a" - note translation of parameters above).  
    Should be a very small number in magnitude (ideally zero).  
      
    reference:  
    Peter Strobach (2010), Journal of Computational and Applied Mathematics 234  
    http://www.sciencedirect.com/science/article/pii/S0377042710002128  
    By    : Aviv Ofir                  Oct 2017  
    Reliable: 1  
      
      
### tools.math.integral.int2d

Numerical integration of a 2-D matrix Package: Util.math Description: Numerically interagte a 2-D matrix.


    
    Numerical integration of a 2-D matrix  
    Package: Util.math  
    Description: Numerically interagte a 2-D matrix.  
    Input  : - Vector of X values referes to the columns of the matrix  
    to integrate.  
    - Vector of Y values referes to the rows of the matrix  
    to integrate.  
    - A matrix to integrate.  
    - Interpolation method for integration.  
    See interp2.m for options. Default is 'linear'.  
    Note that ExtraVal is set to 0.  
    Output : - Matrix numerical integral.  
    Tested : Matlab R2011a  
    By : Eran O. Ofek         Dec 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: X=[1:0.2:10]'; Y=[1:0.1:10]; [MX,MY]=meshgrid(X,Y);  
    Z=exp(-((MX-5).^2+(MY-5).^2)./2);  
    S=Util.math.int2d(X',Y,Z);  
    Reliable: 2  
      
### tools.math.integral.integral_percentile

Given a tabulate function find limits that contains percentile Package: Util.math Description: Given a numerically tabulated function (X,Y) and a percentile P, find the limits (a,b) such that int_{a}^{b}(Y(X)dx)=P (i.e., the integral of Y(X) from a


    
    Given a tabulate function find limits that contains percentile  
    Package: Util.math  
    Description: Given a numerically tabulated function (X,Y) and a  
    percentile P, find the limits (a,b) such that  
    int_{a}^{b}(Y(X)dx)=P (i.e., the integral of Y(X) from a  
    to b equal P.  
    Input  : - X  
    - Y  
    - Vector of percentile, for each to calculate the limits  
    of the integral which contains that percentiles of the  
    total integral.  
    - Percentile type:  
    'c'  - central (default). The limits are such that the  
    integarl from min(X) to the left limit is equal  
    to the integral from the right limit to max(X).  
    'l'  - The limits are such that the left limit is 0.  
    'r'  - The limits are such that the right limit is 0.  
    - Method:  
    'Interp'  - interpolate the integral of the tabulated  
    function.  
    This option works well for smooth functions  
    (default).  
    'First'   - Do not interpolate the integral of the  
    tabulated function and select the first  
    point that satisfy the integral.  
    This is recomended only if the function  
    tabulation is dense.  
    - Normalize the tabulated function such its integral equal  
    to 1 {'y' | 'n'}. Default is 'y'.  
    Output : - Vector of left limits.  
    - Vector of right limits.  
    - Total integral.  
    Tested : Matlab 7.0  
    By : Eran O. Ofek                    Dec 2005  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
    -  
### tools.math.integral.quad_mult2bound

Numerical integration using quad, where the upper bound is a vector Package: Util.integral Description: Numerical interation using the built in quad.m function where the upper bound of the interation is a vector. To speed the calculation the program sorts the upper bound


    
    Numerical integration using quad, where the upper bound is a vector  
    Package: Util.integral  
    Description: Numerical interation using the built in quad.m function  
    where the upper bound of the interation is a vector.  
    To speed the calculation the program sorts the upper bound  
    and perform the integrations between successive bounderies.  
    Input  : - Function to integrate (see quad.m for details).  
    - Scalar of lower bound of integration.  
    - Vector of upper bounds of integration.  
    * Additional parameters (see quad.m for details).  
    Output : - Integrals.  
    Tested : Matlab 7.11  
    By : Eran O. Ofek                    May 2011  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
      
      
### tools.math.integral.quad_my

Pass arguments to function in quad Package: Util.integral Description: A version of the built in quad.m function in which it is possible to pass additional parameters to the function, with no need for nested functions.


    
    Pass arguments to function in quad  
    Package: Util.integral  
    Description: A version of the built in quad.m function in which it  
    is possible to pass additional parameters to the function,  
    with no need for nested functions.  
    Input  : - A function handle to integrate, Y=Fun(X).  
    Alternatively, a cell vector in which the first  
    element is the function handle, and the rest of the array  
    contains additional parameters to pass to Fun  
    {@Fun,Par1,Par2,...}.  
    In this case Y=Fun(X,Par1,Par2,...).  
    * Additional parameters as defined in quad.m  
    See fminsaerch.m for details.  
    Output : * The output parameters returned by quad.m  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Fun = @(X,A)(X-A).^2  
    [X,Fc]=Util.integral.quad_my({Fun,2},-1,1);  
    [X,Fc]=fminsearch_my(@sin,0,1);  
    Reliable: 2  
      
      
### tools.math.integral.sp_powerlaw_int

sp_powerlaw_int function                                         General Description: Calculate the value the spherical integral and the line integral of a broken power law of the form: rho = K R^(-W1)               R<=R0


    
      
    sp_powerlaw_int function                                         General  
    Description: Calculate the value the spherical integral and the line  
    integral of a broken power law of the form:  
    rho = K R^(-W1)               R<=R0  
    rho = K R0^(W2-W1) R^(-W2)    R>R0.  
    Input  : - Matrix (or scalar) of X (e.g., radii) in which to calculate  
    the properties of the spherical broken power-law.  
    - The constant, K, that defines the power-law.  
    E.g., rho=Kr^-w (e.g., K is the mass-loading parameter).  
    - Power-law index of the first (inner) power-law.  
    - Radius at which the power-law is breaking.  
    - Power-law index of the second (outer) power-law.  
    - Limits for limit-integration [R1 R2].  
    If provided than will also calculate the line integral  
    between R1 and R2. Default is empty matrix.  
    Output : - Structure with the following fields:  
    .Val   - Value of the function (e.g., Density) at the  
    radius R.  
    If K is something like the mass-loading parameter  
    in gr*cm^-1 then  
    in order to get particle density divide by  
    <mu_p>*m_p  
    .Int   - Spherical integral of the function between  
    radius 0 and R (e.g., mass).  
    .Col   - Line integral of the function between  
    radius R and Infinity (e.g., column density).  
    If K is something like the mass-loading parameter  
    in gr*cm^-1 then  
    in order to get particle density divide by  
    <mu_p>*m_p, and multiply by sigmaT./(<mu_p>*m_p)  
    in order to get the optical depth.  
    .ColLim  Line integral between R1 and R2. If R1 and R2 are  
    not provided than will return NaN.  
    Tested : Matlab 2011b  
    By : Eran O. Ofek                    Apr 2013  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: Res=sp_powerlaw_int(logspace(10,18,100),9.7e16,2,4.4e15,4.36)  
    Res=sp_powerlaw_int(1,1e6,3.5,40,2.5,[0.001 1000]);  
    Reliable: 2  
      
      
### tools.math.integral.summatlevel

summatlevel function                                             General Description: Given a matrix and a level value, return the sum of all the values in the matrix which are larger than the level.


    
      
    summatlevel function                                             General  
    Description: Given a matrix and a level value, return the sum of all the  
    values in the matrix which are larger than the level.  
    Input  : - Level.  
    - Matrix.  
    Output : - Sum of values larger than level.  
    See Also: contour_percentile.m  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jun 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
      
### tools.math.integral.trapzmat

Trapezoidal numerical integration on columns or rows of matrices. Package: Util.math Description: Trapezoidal numerical integration on columns or rows of matrices. Contrary to trapz.m, the X input for this function can


    
    Trapezoidal numerical integration on columns or rows of matrices.  
    Package: Util.math  
    Description: Trapezoidal numerical integration on columns or rows of  
    matrices.  
    Contrary to trapz.m, the X input for this function can  
    be a matrix.  
    Input  : - X matrix.  
    - Y matrix.  
    - Dimension along to preform the integration, default is 1.  
    Output : - Vector of integrals.  
    Tested : Matlab 7.6  
    By : Eran O. Ofek                    Jun 2009  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Reliable: 2  
      
