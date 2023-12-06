function Result = conv2_nan(Mat1, Mat2, UseFFT, PadMethod)
    % 2D convolution ignoring NaNs in the first input matrix.
    %   Implemented using two calls to conv2_fast.
    %   In the first call the NaNs in the first input matrix are replaced
    %   by zero.
    %   In he second call the NaNs are replaced by zeros, and all the rest
    %   set to one. The output is the first convolution divided by the
    %   second (normalizing) convolution.
    % Input  : - A matrix (possibly including NaNs).
    %          - A matrix (kernel) without NaNs.
    %          - UseFFT: Method to use. [] - auto, otherwise logical. Default is [].
    %          - Padding before operation (padded region will be removed from
    %            output).
    %            '' - do nothing (Default).
    %            'circular' - circular boundry conditions.
    %            'replicate' - relpicate nearest edge value.
    %            'symmetric' - mirror reflection boundry conditions.
    % Output : - The convolution result.
    % Author : Eran Ofek (Nov 2021)
    % Example: Mat2=imUtil.kernel2.gauss;
    %          Mat1        = randn(100,100)+10;
    %          Mat1(1,1)   = NaN;
    %          Mat1(50,20) = NaN;
    %          Result = imUtil.filter.conv2_nan(Mat1, Mat2);

    arguments
        Mat1
        Mat2
        UseFFT            = [];
        PadMethod         = ''; % 'symmetric'; % circular | replicate | symmetric
    end

    Mat0 = Mat1;
    IsNaN = isnan(Mat1);
    Mat0(IsNaN) = 0;
    MatN = ones(size(Mat0));
    MatN(IsNaN) = 0;
    
    Norm    = imUtil.filter.conv2_fast(MatN, Mat2, UseFFT, PadMethod);
    Result  = imUtil.filter.conv2_fast(Mat0, Mat2, UseFFT, PadMethod)./Norm;
    
end