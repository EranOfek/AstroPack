function Image = interpImageConv(Image, Args)
    % Interpolate image over NaNs using conv2_nan
    %   Will work only if the kernel is larger thn the contogous blocks of
    %   NaNs.
    % Input  : - A matrix.ImageNaNImageNaN
    %          * ...,key,val,...
    %            'Kernel' - A kernel matrix, or a function handle that generates
    %                   a kernel. Default is @imUtil.kernel2.gauss.
    %            'KernelArgs' - A cell array of arguments to pass to the
    %                   Kernel function. Default is {}.
    %            'UseFFT' - Method to use. [] - auto, otherwise logical. Default is [].
    %            'PadMethod' - Padding before operation (padded region will be removed from
    %                   output).
    %                   '' - do nothing (Default).
    %                   'circular' - circular boundry conditions.
    %                   'replicate' - relpicate nearest edge value.
    %                   'symmetric' - mirror reflection boundry conditions.
    % Output : - An interpolated over NaN image.
    % Author : Eran Ofek (Nov 2021)
    % Example: [MatX, MatY] = meshgrid((1:1000),(1:1000));
    %          Image = MatX.*MatY + randn(1000,1000).*0.01;
    %          Image(10,10) = NaN; Image(20,11) = NaN; Image(50,60) = NaN;
    %          Image(1,30) = NaN; Image(21,11) = NaN;
    %          Result = imUtil.interp.interpImageConv(Image);
    
    arguments
        Image
        Args.Kernel            = @imUtil.kernel2.gauss;
        Args.KernelArgs cell   = {};
        Args.UseFFT            = [];
        Args.PadMethod         = '';
    end
    
    if isnumeric(Args.Kernel)
        Kernel = Args.Kernel;
    else
        Kernel = Args.Kernel(Args.KernelArgs{:});
    end
    Ind     = find(isnan(Image));
    Result2 = imUtil.filter.conv2_nan(Image, Kernel, Args.UseFFT, Args.PadMethod);
    Image(Ind) = Result2(Ind);
    
end
    