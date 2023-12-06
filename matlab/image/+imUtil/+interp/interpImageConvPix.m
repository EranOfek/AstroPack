function Image=interpImageConvPix(Image, X, Y, Args)
    % Interpolation using kernel, over specified pixels, or NaN pixels in a 2D matrix.
    %   Interpolation using the kernel-convolution-hole method.
    %   The interpolation algorithm is efficient and uses the following
    %   scheme:
    %   We select a stamp around the interpolated pixel.
    %   We prepare a Gaussian (default) kernel, in which the central pixel is set to 0.
    %   The modified Gaussian kernel is normalized to unity,
    %   multiplied by the image stamp, and integrated over the stamp (i.e., local convolution).
    %   The result of the local convolution replaces the value of the interpolated pixel.
    % 
    % Input  : - A 2D image
    %          - X position (see more options below).
    %            If this argument is empty, then will find all the NaN
    %            pixels in the matrix and interpolate over those pixels.
    %          - Y position.
    %            If this argument is empty, then the X position is treated
    %            as a linear index of the pixel to interpolate.
    %          * ...,key,val,...
    %            'Kernel' - A kernel matrix, or a function handle that will
    %                   generate a kernel matrix (see functions in
    %                   imUtil.kernel2).
    %                   Default is @imUtil.kernel2.gauss
    %            'KernelArgs' - A cell array of arguments to pass to the
    %                   kernel function. Default is {}.
    %            'mexCutout' - A logical indicating if to use mex option 
    %                   in imUtil.cut.image2cutouts
    %                   Default is true.
    % Output : - Image in which the specified pixels are interpolated over.
    % Author : Eran Ofek (Jun 2023)
    % Example: Image = rand(1000,1000);
    %          K = randi(1e6,1000,1);
    %          [Y,X] = ind2sub(size(Image),K);
    %          Image(K) = NaN;
    %          Kernel     = imUtil.kernel2.gauss; %ones(11,11)./121;
    %          Conv2 = imUtil.interp.interpImageConvPix(Image, X, Y);

    arguments
        Image
        X                        = [];
        Y                        = [];
        Args.Kernel              = @imUtil.kernel2.gauss;
        Args.KernelArgs cell     = {};
        Args.mexCutout logical   = true;
    end

    SizeIm = size(Image);
    if isempty(X)
        % assume both X and Y are empty
        % find NaNs
        K = find(isnan(Image));
        [Y, X] = imUtil.image.ind2sub_fast(SizeIm, K);
    else
        if isempty(Y)
            % assume X is the linear index of the pixel to interpolate
            [Y, X] = imUtil.image.ind2sub_fast(SizeIm, X);
        end
    end
    
    if ~isempty(X)
        RoundX = round(X);
        RoundY = round(Y);
    
        % Get kernel
        if isa(Args.Kernel, 'function_handle')
            Kernel = Args.Kernel(Args.KernelArgs{:});
        else
            % assume kernel is a matrix
            Kernel = Args.Kernel;
        end
        SizeKernel = size(Kernel);
        MaxRadius  = (SizeKernel - 1).*0.5;
    
       
        % assume input is a 2-D image
    
        [Cube, RoundX, RoundY] = imUtil.cut.image2cutouts(Image, RoundX, RoundY, MaxRadius, 'mexCutout',Args.mexCutout);
       
        [SI, SJ, Nim] = size(Cube);
      
        NormKernel = sum(Kernel, 'all', 'omitnan');
    
        Sum2           = squeeze(sum(Cube.*Kernel, [1 2], 'omitnan'));
        IsN            = isnan(Cube);
        KernelNaN      = repmat(Kernel, 1, 1, Nim);
        KernelNaN(IsN) = NaN;
        Norm2          = squeeze(sum(KernelNaN, [1 2], 'omitnan'));
    
        K        = imUtil.image.sub2ind_fast(SizeIm, RoundY, RoundX);
        Image(K) = Sum2./Norm2 .* NormKernel;
    end
end