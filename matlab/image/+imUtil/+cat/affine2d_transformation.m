function [NewX,NewY,AffineMatrix,AffineMatrixNoFlip]=affine2d_transformation(Cat,AffineMatrix,Direction,Args)
% Applay a 2D affine transformation to [X,Y] coordinates 
% Package: +imUtil.cat
% Description: Applay a 2D affine transformation to [X,Y] coordinates 
% Input  : - Catalog for which to transform the coordinates.
%            If empty, then return only the affine matrix.
%          - Either a 2D affine matrix, or a vector of rotation and shift.
%            If a matrix, this is a 3x3 affine matrix
%            e.g., [cos(Theta), -sin(Theta), ShiftX
%                   sin(Theta),  cos(Theta), ShiftY
%                   0            0           1]
%            Otherwise a vector of [Theta, Scale, ShiftX, ShiftY]
%            or [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY]
%          - Direction of rotation:
%            '+' (or positive) will rotate the coordinates in respect to the reference
%            frame, while '-' (or negative) will rotate the reference frame.
%            Default is '+'.
%          * Pairs of ...,key,val,... The following keywords are avaialble:
%            'ThetaUnits' - If the AffineMatrix (second input argument) is
%                   a vector of three elements then here you can specify 
%                   the units of the rotation angle Theta.
%                   Default is 'deg'.
%            'ColX' - Catalog column that contains the X axis. Default is 1.
%            'ColY' - Catalog column that contains the Y axis. Default is 2.
% Output : - A vector of new X coordinates.
%          - A vector of new Y coordinates.
%          - A 3x3 affine matrix for 2D transformation.
%          - A 3x3 affine matrix for 2D transformation, but without the
%            flip.
%      By: Eran O. Ofek                         May 2020
% Example: Cat = rand(10,2);
%          [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,[0 10 10])

arguments
    Cat
    AffineMatrix
    Direction   = '+';
    Args.ThetaUnits = 'deg';
    Args.ColX       = 1;
    Args.ColY       = 2;
end


if isnumeric(Direction)
    if Direction>0
        Direction = '+';
    else
        Direction = '-';
    end
end

if numel(AffineMatrix)~=9
    if numel(AffineMatrix)==4
        AffineMatrix = [AffineMatrix, 1, 1];
    end
    % assume [Theta, ShiftX, ShiftY]
    Theta  = convert.angular(Args.ThetaUnits,'rad',AffineMatrix(1)); % Theta in radians
    Scale  = AffineMatrix(2);
    ShiftX = AffineMatrix(3);
    ShiftY = AffineMatrix(4);
    FlipX  = AffineMatrix(5);
    FlipY  = AffineMatrix(6);
    
    AffineMatrix       = [FlipX.*Scale.*cos(Theta), -FlipY.*Scale.*sin(Theta), ShiftX; FlipX.*Scale.*sin(Theta),  FlipY.*Scale.*cos(Theta), ShiftY; 0  0  1];
    AffineMatrixNoFlip = [       Scale.*cos(Theta),        -Scale.*sin(Theta), ShiftX;        Scale.*sin(Theta),         Scale.*cos(Theta), ShiftY; 0  0  1];
end

switch Direction
    case '+'
        % do nothing
    case '-'
        AffineMatrix = AffineMatrix.';
    otherwise
        error('Unknown Direction option (must be "+" or "-")');
end


% apply affine transformayion
if ~isempty(Cat)
    N = size(Cat,1);
    X = Cat(:,Args.ColX);
    Y = Cat(:,Args.ColY);
    Z = ones(N,1);

    Coo    = [X,Y,Z];
    NewCoo = [AffineMatrix * Coo.'].';
    NewX   = NewCoo(:,1);
    NewY   = NewCoo(:,2);
else
    NewX = [];
    NewY = [];
end

