function Result = unitTest
    % unitTest for Tran2D class
    % Example: Tran2D.unitTest

    %io.msgLog(LogLevel.Test, 'testing Tran2D constructors');
    T=Tran2D;

    TC = Tran2D;
    TC = Tran2D(2);
    TC = Tran2D('cheby1_3_c1','cheby1_2');

    %io.msgLog(LogLevel.Test, 'testing Tran2D design_matrix');
    [Hx,Hy]=design_matrix(T,rand(10,2));

    %io.msgLog(LogLevel.Test, 'testing Tran2D nfuns');
    [NfunX,NfunY]=nfuns(T);
    Ans=isParKnown(T);
    TC=Tran2D; 
    TC.ParY=ones(1,10);  
    TC.ParX=ones(1,10); 
    [Xf,Yf]=forward(TC,[1 1;2 1]);

    TC=Tran2D;
    TC.ParY=zeros(1,10); 
    TC.ParX=zeros(1,10); 
    TC.ParX(1:2) = 1;
    TC.ParX(5)=0.03;
    TC.ParX(7)=0.01;
    TC.ParY(1) = 2;
    TC.ParY(3)=1.01;
    TC.ParY(5)=0.01;
    TC.ParY(8)=0.001;
    XY = [1 2; 1.1, 0.2; 0.3, 2.1];

    %io.msgLog(LogLevel.Test, 'testing Tran2D forward, backward');
    [Xf,Yf]=forward(TC,XY);
    [X,Y]=backward(TC,[Xf, Yf]);

    if ~all(abs(XY - [X, Y])<1e-4)
        error('Forward/backward transformation faild');
    end

    % symbolic representations
    %io.msgLog(LogLevel.Test, 'testing Tran2D functionals2simPoly');
    TC=Tran2D;
    [CX,CY,PX,PY]=Tran2D.functionals2symPoly(TC.ColCell,TC.FunX,TC.FunY,TC.FunNX,TC.FunNY);
    %io.msgLog(LogLevel.Test, 'testing Tran2D symPoly');
    [CX,CY,PX,PY]=symPoly(TC);
    TC = Tran2D;
    TC.symPoly;
    TC.ParX = ones(1,10);
    TC.ParY = ones(1,10);
    TC.polyCoef;
    %io.msgLog(LogLevel.Test, 'testing Tran2D symPoly2deg');
    [PolyX_Xdeg,PolyX_Ydeg,PolyY_Xdeg,PolyY_Ydeg]=symPoly2deg(TC);
    TC.ParX = ones(1,10);
    TC.ParY = ones(1,10);
    TC.polyRep;

    % selected_trans
    %io.msgLog(LogLevel.Test, 'testing Tran2D selected_trans');
    [FunX,FunY,ColCell]=Tran2D.selected_trans('cheby1_3_c1');

    % tran2d2wcs
%             TC = Tran2D('cheby1_3_c1');
%             W=Tran2D2wcsCl(TC); <-- doesn't work

    % Symbolic representations of polynomials:
    % E.g., converting Chebyshev functionals to regular polynomials.
    % The functionals2symPoly static function returns the symbolic
    % representation of the original functionals.
    % CX,CY is an array of symbolic functions of the coeficienst of the
    % polynomial functionals.
    % PX, PY are the polynomials functionals.
    TC=Tran2D;
    [CX,CY,PX,PY] = Tran2D.functionals2symPoly(TC.ColCell,TC.FunX,TC.FunY,TC.FunNX,TC.FunNY);
    % symPoly is a method of Tran2D that populate the PolyRep with
    % the CX, CY, PX, PY symbolic functions.
    % This function is slow and should be run once the Tran2D was constructed.
    [CX,CY,PX,PY]=symPoly(TC);
    % Next populate the ParX, ParY:
    TC.ParY=zeros(1,10);  TC.ParX=zeros(1,10); 
    TC.ParX(1:2) = 1; TC.ParX(5)=0.03; TC.ParX(7)=0.01;
    TC.ParY(1) = 2; TC.ParY(3)=1.01; TC.ParY(5)=0.01; TC.ParY(8)=0.001;
    % Calculate the numerical value of the polynomials coef.
    % The coef. are populated in: TC.PolyRep.PolyParX, PolyParY:
    [PolyCoefX, PolyCoefY, PX, PY] = TC.polyCoef;
    % Return the X and Y poly deg vectors of the polynomial representations
    % and populate them in PolyRep:
    [PolyX_Xdeg,PolyX_Ydeg,PolyY_Xdeg,PolyY_Ydeg]=symPoly2deg(TC);

    % Update the PolyRep polynomial representation property
    TC=polyRep(TC,true)


    % fitAstrometricTran: AssymRMS/ErrorOnMean are NaN with too few
    % degrees of freedom (issue #1366)
    rng(5);
    for N=[8 15 60]
        X   = rand(N,1).*1700;
        Y   = rand(N,1).*1700;
        Mag = 12 + rand(N,1).*5;
        Xi  = 1e-4.*X + 0.01 + 1e-6.*randn(N,1);
        Yi  = 1e-4.*Y - 0.02 + 1e-6.*randn(N,1);
        [~,Res] = fitAstrometricTran(Tran2D('poly3'), Xi, Yi, X, Y, 'ExtraData',[], 'Mag',Mag, 'ErrPos',1e-6);
        if Res.Ngood<20
            assert(isnan(Res.AssymRMS) && isnan(Res.ErrorOnMean), 'fitAstrometricTran: exact fit not flagged');
        else
            assert(isfinite(Res.AssymRMS) && isfinite(Res.ErrorOnMean), 'fitAstrometricTran: no AssymRMS');
        end
    end


    %io.msgStyle(LogLevel.Test, '@passed', 'Tran2D test passed');

    Result = true;

end

