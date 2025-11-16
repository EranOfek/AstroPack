function [K] = king(Par, SizeXY, PosXY)
    % create a matrix or a cube of 2D normalized King functions 
    % Package: +imUtil.kernel2
    % Input  : - Two column matrix of [Core Radius, Slope].
    %            Default is [1, 1].
    %          - Stamp size [X,Y]. Default is [15 15].
    %          - [X,Y] Position of the Gaussian center in the stamp.
    %            Default is the ceil(stamp_size/2).
    % Output : - A matrix or a cube with the 2D King functions which sum is
    %            normalized to 1.
    %            If a cube, the third dimension corresponds to the template
    %            index.
    % Author : Eran Ofek (Nov 2025)
    % Example: imUtil.kernel2.king
    %          imUtil.kernel2.king([1 1; 2 2]); % a template bank of king funs.
    
    arguments
        Par    = [1 1];
        SizeXY = [15 15];
        PosXY  = [];
    end
    
    if isempty(PosXY)
        PosXY = ceil(SizeXY.*0.5);
    end
    
    [MatX,MatY] = meshgrid( (1:1:SizeXY(1))-PosXY(1), (1:1:SizeXY(2))-PosXY(2) );
    MatR2 = MatX.^2 + MatY.^2;
    
    CoreRadius2 = Par(:,1).^2;
    Slope       = Par(:,2);

    Nc = numel(CoreRadius2);
    if Nc>1
        CoreRadius2 = reshape(CoreRadius2, [1 1 Nc]);
        Slope       = reshape(Slope,       [1 1 Nc]);
    end
    K = (1 + MatR2./CoreRadius2).^Slope;
    
    % doing direct normalization
    % otherwise bug when sigma is small
    K = K./sum(K,[1 2]);

end

