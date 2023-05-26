function [Result,PeakDet]=collapse(Image, Args)
    % Collapse a 2-D image, including pre and post filtering, and peak detection.
    %   Collapse a 2-D image on one axis (specified by DimWave argument).
    %   Prior to the collapse, the matrix is optionally convolved with a
    %   2-D Gaussian.
    %   The collapse can be done using a variety of functions.
    %   The 1-D collapse function can be optionlly filtered with a 1-D
    %   Gaussian and nomalized by the std of the vector.
    %   Find peaks in the 1-D collapsed vector, above some threshold in
    %   units of sigma.
    % Input  : - A 2-D matrix.
    %          * ...,key,val,...
    %            'DimWave' - The dimension along to collapse the image
    %                   (e.g., wavelength dimension).
    %                   Default is 2.
    %            'PreConv' - sigma-width of Gaussian to use to convolve the
    %                   image with prior to the collapse.
    %                   Alternatively, this can be a function handle that
    %                   calculates the convolution kernel (e.g..,
    %                   @imUtil.kernel2.gauss)
    %                   If empty, then do not convolve.
    %                   Default is [].
    %            'PreConvArgs' - A cell array of additional arguments to
    %                   pass to the 'PreConv' function handle.
    %                   Default is {}.
    %            'PostFilter' - The sigma-width of the 1-D Gaussian to use
    %                   in the post collapse filter step.
    %                   If empty, then do not filter.
    %                   Default is 2.
    %            'Fun' - One of the follwing collapse function:
    %                   'median' - Medain collapse (default).
    %                   'mean', 'std', 'quantile', 'max'.
    %            'Quant' - For the quantile collapse function this is the
    %                   quantile. Default is 0.95.
    %            'Threshold' - Detection threshold for peaks in the
    %                   collapsed data. Default is 5 (sigmas).
    % Output : - Vector of collapse data.
    %          - A structure with the following fields:
    %            .PeaksPos - Position (whole pixel) of found peaks.
    %            .PeakSN - S/N of found peaks.
    %
    % AUthor : Eran Ofek (May 2023)
    % Example: R=randn(100,100); R(30,:)=R(30,:)+1;
    %          Res=imUtil.spec.trace.collapse(R,'Fun','mean');
    
    arguments
        Image
        Args.DimWave          = 2;  % i.e., collapse-dim
        Args.PreConv          = [];  % filter to convolve with prior to collapse
        Args.PreConvArgs cell = {};
        Args.PostFilter       = 2;  % sigma-width
        Args.Fun              = 'median';
        Args.Quant            = 0.95;
        Args.Threshold        = 5;
        
    end
    
    if ~isempty(Args.PreConv)
        if isa(Args.PreConv, 'function_handle')
            Conv = Args.PreConv(Args.PreConvArgs{:});
        else
            Conv = imUtil.kernel2.gauss(Args.PreConv);
        end
        Image = imUtil.filter.conv2_fast(Image, Conv);
    end
    
    % collapse image in the wave-dir
    switch Args.Fun
        case 'mean'
            Result = mean(Image, Args.DimWave, 'omitnan');
        case 'median'
            Result = median(Image, Args.DimWave, 'omitnan');
        case 'std'
            Result = std(Image, [], Args.DimWave, 'omitnan');
        case 'quantile'
            Result = quantile(Image, Args.Quant, Args.DimWave);
        case 'max'
            Result = max(Image,[],Args.DimWave);
        otherwise
            error('Unknown Fun option');
    end
           
    % post filter
    Std = tools.math.stat.rstd(Result);
    
    if ~isempty(Args.PostFilter)
        FiltSize = ceil(Args.PostFilter.*4);
        Filter   = normpdf((-FiltSize:1:FiltSize),0,Args.PostFilter);
        Result   = conv(Result, Filter', 'same');
        Norm     = sqrt(sum(Filter.^2));
    else
        Norm     = 1;
        
    end
    
    if nargout>1
        Std = Std.*Norm;
        %Std = tools.math.stat.rstd(Result);

        SN = Result./Std;

        Flag = logical((SN > Args.Threshold).*islocalmax(Result));
        %[find(Flag), SN(Flag)]
        
        PeakDet.PeaksPos = find(Flag);
        PrekDet.PeakSN   = SN(Flag);
    end
        
    
end
