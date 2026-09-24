function Result = unitTest
    % unitTest for +imProc.match
    % Example: imProc.trans.unitTest
    
    % imProc.trans.matchPattern
    % only shift
    Nstar = 1000;
    Ref = rand(Nstar,2).*2048 - 1024;
    %Ref = sortrows(Ref,2);
    Noverlap = 300;
    Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
    Cat = [Cat; rand(Nstar-Noverlap,2).*1024];
    Cat(:,1) = Cat(:,1) + 220 + randn(Nstar,1).*0.3;
    Cat(:,2) = Cat(:,2) + 130 + randn(Nstar,1).*0.3;
    Cat      = sortrows(Cat,2);
    Flip     = [1 -1];
    Ref      = Ref.*Flip;
    Result = imProc.trans.fitPattern(Cat,Ref);
    
    if ~(Result.Sol.SN>10 && all(Result.Sol.Flip==Flip) && abs(Result.Sol.ShiftX-220)<3 && abs(Result.Sol.ShiftY-130)<3)
        error('Problem with matchPattern');
    end
    
    % shift and rotation
    Ns = 5000;
    Nm = 2000;
    Theta = 35;
    Ref = rand(Ns,2).*2048 - 1024;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    Cat = [Cat; rand(Nm,2).*1024];
    % [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY];
    %Tr = [35, 1, 120, 45, 1, 1];
    Tr = [35, 1, 120, 45, 1, -1];
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    Result = imProc.trans.fitPattern(Cat,Ref);

    % Shift rotation
    Ns = 5000;
    Nm = 2000;
    Ref = rand(Ns,2).*2048 - 1024;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    %Tr = [-15, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [-115, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [15, 1.0, -20, 45, 1, 1];   % ok
    %Tr = [115, 1.0, -20, 45, 1, 1];  % ok
    %Tr = [-15, 1.0, -20, 45, -1, -1];  % ok
    Tr = [-115, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [15, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [115, 1.0, -20, 45, -1, -1];  % ok
    %Tr = [15, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [115, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [-115, 1.0, -20, 45, 1, -1];  % ok
    %Tr = [-15, 1.1, -20, 45, 1, -1];  % ok
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    Cat = [Cat; rand(Nm,2).*1024];
    Result = imProc.trans.fitPattern(Cat,Ref);

    
    % fitTransformation
    Nsrc = 1000;
    Cat = rand(Nsrc,3).*[1024 1024 10];
    Ref = Cat + 0.1.*randn(Nsrc,3);
    Ref = [Ref, rand(Nsrc,1).*2];
    T   = Tran2D;
    [Param, Res] = imProc.trans.fitTransformation(Cat, Ref, 'Tran',T);

    % projection
    Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 1, 'tan', 'Coo0Units','rad', 'ColUnits',{'rad','rad'});
    Result = imProc.trans.projection(rand(100,2), 0.5, 0.5, 180./pi, 'tan', 'Coo0Units','deg', 'ColUnits',{'deg','deg'});
    
    % projectionInv
    Long = 1;
    Lat  = 1;
    [X,Y]=celestial.proj.pr_gnomonic(Long, Lat, 1,[0.5 0.5]);
    [Long1,Lat1]=celestial.proj.pr_ignomonic(X,Y,[0.5 0.5]);
    if ~all(Long==Long1) || ~all(Lat==Lat1)
        error('Problem with gnomonic projection');
    end
    Cat = rand(100,2);
    % long/lat -> x/y
    Result = imProc.trans.projection(Cat, 0.5, 0.5, 1, 'tan', 'Coo0Units','rad', 'ColUnits',{'rad','rad'});
    % x/y -> long/lat
    Res2   = imProc.trans.projectionInv(Result, 0.5, 0.5, 1, 'tan', 'Coo0Units','rad', 'AddNewCols',{});
    if ~all(Res2.Catalog(:,1:2)==Cat,'all')
        error('Problem with gnomonic projection');
    end
    
    % tranAffine
    Cat  = rand(100,2);
    Tran = [1 1];
    Result = imProc.trans.tranAffine(Cat, Tran, true);
    if ~all(abs((Result.Catalog-Cat)-Tran)<1e-15, 'all')
        error('Problem with tranAffine');
    end
    Result = imProc.trans.tranAffine(Cat, Tran, false);
    if ~all(abs((Result.Catalog-Cat))<1e-15, 'all')
        error('Problem with tranAffine');
    end
    
    
    Result = true;
end