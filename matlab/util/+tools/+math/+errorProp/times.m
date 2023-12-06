function [Val, Err]=times(X,D_X,Y,D_Y)
    % Error propagation of times (multiplication) operation
    % Input  : - X
    %          - Error on X
    %          - Y
    %          - Error on Y
    % Output : - Result of X.*Y
    %          - Error on X.*Y calculated using:
    %            sqrt(D_x.^2.*y.^2 + D_y.^2.*x.^2);
    % Author : Eran Ofek (Sep 2023)
    % Example: X = rand(4,5); D_X=0.01.*ones(4,5); Y=rand(1,5); D_Y=0.01;
    %          [Val, Err] = tools.math.errorProp.times(X,D_X,Y,D_Y)
    
    Val = X.*Y;
    Err = sqrt(D_X.^2.*Y.^2 + D_Y.^2.*X.^2);
end