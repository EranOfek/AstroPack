function [IntImage]=interp_sparse2full(X, Y, Image, OutSize, Args)
% Interpolate sparse image to a full image
% OBSOLETE: use imUtil.image.sparse2full instead.
% Package: imUtil.partition
% Description: 
% Input  : - Matrix of X poisitions.
%          - Matrix of Y poisitions.
%          - Image.
%          - Output size [X,Y]
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
% Output : - Interpolated full image.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Sep 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: 
% Reliable: 
%--------------------------------------------------------------------------

arguments
    X
    Y
    Image
    OutSize                             % [X,Y]
    Args.Method               = 'interp';   % 'interp' | 'fit' | 'inpaintn' | 'conv'
    Args.InterpMethod         = 'linear'; %'makima';   % 'linear' | 'nearest' | 'spline' | 'cubic' | 'makima'
    Args.ConvRadius           = [];
end

[MatX,MatY] = meshgrid((1:1:OutSize(1)),(1:1:OutSize(2)));

switch lower(Args.Method)
    case 'interp'
        
        if (any(size(Image)==1))
            % assume X,Y,Image are vectors and represent a scattered image
            % in this case use scattered interpolation
            warning('Switched to linear scattered interpolation');
            
            F = scatteredInterpolant(X,Y,Image,'linear');
            IntImage = F(MatX,MatY);
            
            
        else
            % regular grid - use interp2
            IntImage = interp2(X,Y,Image,MatX,MatY,Args.InterpMethod);
        end
        
    case 'conv'
        % use convolution
        FullImage = zeros(fliplr(OutSize));
        Ind = imUtil.image.sub2ind_fast(OutSize, floor(Y(:)), floor(X(:)) );
        FullImage(Ind) = Image(:);
        
        % convolve delta functions with circ
        if isempty(Args.ConvRadius)
            error('Must supply ConvRadius');
        end
        Radius   = Args.ConvRadius;
        Circ     = imUtil.kernel2.circ(Radius,[2.*Radius+1, 2.*Radius+1]);
        IntImage = imUtil.filter.conv2_fast(FullImage, Circ);
        
    case 'inpaintn'
        % use inpaintn
        FullImage = nan(fliplr(OutSize));
        Ind = imUtil.image.sub2ind_fast(OutSize, floor(Y(:)), floor(X(:)) );
        FullImage(Ind) = Image(:);
        IntImage  = inpaintn(FullImage);
        
    case 'fit'
        error('fit option - not yet implemented');
    otherwise
        error('Unknown Method option');
end
