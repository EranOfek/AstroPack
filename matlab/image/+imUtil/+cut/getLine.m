function [XL, YL, Val] = getLine(Image,X,Y)
    % Get the values in image along a line or polygon defined by start and
    % end points.
    % Input  : - Image matrix.
    %          - Vector of X points defines that verteces of the polygon.
    %          - Vector of Y points defines that verteces of the polygon.
    % Output : - Vector of pixel X positions.
    %          - Vector of pixel Y positions.
    %          - Vector of image values at X,Y positions.
    % Author : Eran Ofek (Oct 2022)
    % Example: Image = rand(100,100); X=[ 5 10]; Y=[7 20];
    %          [XL, YL, Val] = imUtil.cut.getLine(Image,X,Y)
    
    arguments
        Image
        X
        Y
        
    end
    
    Nx = numel(X);
    for Ix=1:1:Nx-1
        XV(Ix).X = (floor(X(Ix)):1:ceil(X(Ix+1)));
        YV(Ix).Y = interp1([X(Ix), X(Ix+1)],[Y(Ix), Y(Ix+1)], XV(Ix).X, 'linear', 'extrap');
    end
    XL = [XV.X];
    YL = [YV.Y];
    
    Val = interp2(Image, XL, YL, 'nearest');
        
end