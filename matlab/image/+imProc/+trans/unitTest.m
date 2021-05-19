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
    
    if ~(Result.Sol.SN>30 && all(Result.Sol.Flip==Flip) && abs(Result.Sol.ShiftX-220)<3 && abs(Result.Sol.ShiftY-130)<3)
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


    Result = true;
end