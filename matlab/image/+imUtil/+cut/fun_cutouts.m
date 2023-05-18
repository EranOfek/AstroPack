function [varargout] = fun_cutouts(Image, X, Y, Fun, Args)
% Apply a function thar returns a scalar to all cutouts in image around locations.
% Input  : - a 2D image (background subtracted), or a 3D cube of cutouts
%            (3rd dim is ciutout index).
%          - A vector of X coordinates around to calculate moments.
%          - A vector of Y coordinates around to calculate moments.
%          - Function handle to apply on all cutouts. The function gets a
%            matrix and returns a scalar.
%          * ...,key,val,...
%            'FunArgs' - A cell array of additional arguments to pass to
%                       the function. Default is {}.
%            'FunWorksOnCube' - Function works on cube (where 3rd dim is
%                       the cutout index). Default is false.
%            'FunWorksOnMatrix' - Function works on matrix (over 1st dim.).
%                       Default is false.
%            'Radius' - Radius of cutouts (if input is a matrix).
%            'Circle' - A flag indicating if to extract the stamps with
%                       circular shape. Default is false.
%            'mexCutout' - use imUtil.cut.mexCutout.m (true) or
%                       imUtil.cut.find_within_radius_mat (false).
%                       Default is true.
% Output : * Arbitrary numbre of vectors. Each vector for each output of
%            Fun. each element in each vector corresponds to one cutout.
% Author : Eran Ofek (Mar 2021)
% Example: [M] = imUtil.cut.fun_cutouts(rand(1000,1000), rand(100,1).*600+10, rand(100,1).*600+10, @median, 'FunWorksOnMatrix',true)
%          [M] = imUtil.cut.fun_cutouts(rand(20,20,100), [], [], @median, 'FunWorksOnMatrix',true)

arguments
    Image
    X
    Y
    Fun function_handle
    Args.FunArgs cell                                  = {};
    Args.FunWorksOnCube                                = false;
    Args.FunWorksOnMatrix                              = false;  % first dim of matrix
    Args.Radius                                        = 5;
    Args.Circle(1,1) logical                           = false;
    Args.mexCutout(1,1) logical                        = true;
end

NdimImage = ndims(Image);
if NdimImage==2
    % Image is 2D - build a cube of 2D stamps
    if Args.mexCutout
        [Cube]=imUtil.cut.mex.mex_cutout(Image,[X,Y],Args.Radius.*2+1);
        Cube  = squeeze(Cube);
        RoundX = round(X);
        RoundY = round(Y);
    else
        [Cube,RoundX,RoundY]=imUtil.cut.find_within_radius_mat(Image, X, Y, Args.Radius, Args.Circle);
    end
    
    
elseif NdimImage==3
    % Image is a lready a cube of stamps
    Cube   = Image;
    RoundX = round(X);
    RoundY = round(Y);
else
    error('Image number of dimensions must be 2 or 3');
end

if Args.FunWorksOnCube
    [varargout{1:nargout}] = Fun(Cube, Args.FunArgs{:});
else
    if Args.FunWorksOnMatrix
        Size                   = size(Cube);
        Cube                   = reshape(Cube, Size(1).*Size(2), Size(3));
        [varargout{1:nargout}] = Fun(Cube, Args.FunArgs{:});
    else
        % loop all over cutouts
        Ncut = size(Cube,3);
        Result = zeros(Ncut,1);
        for Icut=1:1:Ncut
            [varargout{1:nargout}(Icut)] = Fun(squeeze(Cube(:,:,Icut)), Args.FunArgs{:});
            %Result(Icut) = Fun(squeeze(Cube(:,:,Icut)), Args.FunArgs{:});
        end
    end
end