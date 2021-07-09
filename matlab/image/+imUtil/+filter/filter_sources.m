function [SmBackEst, BackEst] = filter_sources(Image, Args)
    % Generate a background image filtered from sources using sucessive filtering
    % Description: This routine filter out sources from an image and
    %       generate an approximate background image.
    %       The following steps are implemented:
    %       1. Filtering the image against template bank of sources.
    %       2. Set the value around sources found by step 1 to NaN.
    %       3. Use inpaint_nans to interpolate over NaNs.
    %       4. Smooth the image using another filter (default is annulus).
    %       5. if Niter>1 go to step 1, and find sources this time using
    %          the newly estimated background.
    % Input  : - A 2D image.
    %          * ...,key,val,...
    %            'Back' - Background matrix for first iteration.
    %                   If empty, estimate the background.
    %                   Default is [].
    %            'Var' - Varince matrix for first iteration.
    %                   If empty, estimate the variance.
    %                   Default is [].
    %            'Threshold' - Sources detection threshold (sigmas).
    %                   Default is 3.
    %            'BackFun' - Function handle for background estimation.
    %                   Default is @median.
    %            'BackFunArgs' - A cell array of additional arguments to
    %                   pass to the background estimation function.
    %                   Default is {[1 2],'omitnan'}.
    %            'VarFun' - Function handle for variance estimation.
    %                   Default is @imUtil.background.rvar.
    %            'VarFunArgs' - A cell array of additional arguments to
    %                   pass to the variance estimation function.
    %                   Default is {}.
    %            'Template' - A template for filtering in the source
    %                   finding step, or a function handle for generating
    %                   templates. The template is a matrix or a cube of
    %                   templates (all will be filtered).
    %                   Default is imUtil.kernel2.gauss.
    %            'TemplateArgs' - A cell array of arguments to pass to the
    %                   Template filter function. Default is {[0.1 2 3].'}.
    %            'RadiusNaN' - Radius in pixels which will be replaced with
    %                   NaN around all the found sources (i.e., pixels with
    %                   S/N>Threshold). Default is 3.
    %            'InpaintMethod' - inpaint_nans method (see inpaint_nans
    %                   for details). Default is 2.
    %            'SmoothFilter' - A final smoothing filter.
    %                   Either a 2D matrix, or a function handle to generate
    %                   the filter, or empty.
    %                   If empty, skip this step and do not smooth the
    %                   background image.
    %                   Default is @imUtil.kernel2.annulus
    %            'SmoothFilterArgs' - A cell array of arguments to pass to
    %                   the SmoothFilter funcion. Default is {[16 32]}.
    %            'PadMethod' - Padding before convolution method.
    %                   Default is 'symmetric'.
    %            'VarFromBack' - If true, for the next iteration use Var=Back.
    %                   Otherwise, use the origibal Var image.
    %            'Niter' - Number of iterations. Default is 1.
    % Output : - The smooth background image.
    %          - The background image prior to smoothing.
    % Author : Eran Ofek (Jul 2021)
    % Example: AI=AstroImage('PTF_Cropped.fits');
    %          AI=cast(AI,'double'); AI.Image = AI.Image.*1.6;
    %          [SmBackEst, BackEst] = imUtil.filter.filter_sources(AI.Image)
    
    arguments
        Image
        Args.Back                       = [];
        Args.Var                        = [];
        Args.Threshold                  = 3;
        Args.BackFun function_handle    = @median
        Args.BackFunArgs cell           = {[1 2],'omitnan'};
        Args.VarFun function_handle     = @imUtil.background.rvar;
        Args.VarFunArgs cell            = {};
        Args.Template                   = @imUtil.kernel2.gauss; % or template
        Args.TemplateArgs cell          = {[0.1 2 3].'};
        Args.RadiusNaN                  = 3;
        Args.InpaintMethod              = 2;
        % final smoothing
        Args.SmoothFilter               = @imUtil.kernel2.annulus; % or template
        Args.SmoothFilterArgs cell      = {[16 32]};
        Args.PadMethod                  = 'symmetric';
        Args.VarFromBack(1,1) logical   = true;   % use variance=background in second iteration
        Args.Niter                      = 1;
    end
    
    % estimate background and variance
    if isempty(Args.Back)
        Back = Args.BackFun(Image, Args.BackFunArgs{:});
    else
        Back = Args.Back;
    end
    if isempty(Args.Var)
        Var  = Args.VarFun(Image, Args.VarFunArgs{:});
    else
        Var  = Args.Var;
    end
    
    for Iiter=1:1:Args.Niter
        % filter the image with a template bank
        SN = imUtil.filter.filter2_snBank(Image, Back, Var, Args.Template, Args.TemplateArgs{:});
        % collapse the SN image for all templates
        SN = max(SN,[],3);  
        % Construct an image of delta functions at source positions
        BW = zeros(size(SN));
        BW(SN>Args.Threshold) = 1;

        % convolve the BW map with a NaN filter
        FilterNaN  = imUtil.kernel2.circ(Args.RadiusNaN);
        FilteredBW = imUtil.filter.conv2_fast(BW, FilterNaN, [], Args.PadMethod);
        FilteredBW(FilteredBW>(10.*eps)) = NaN;
        FilteredBW = FilteredBW + 1;

        % interpolate over NaNs - non-smooth background estimator
        BackEst = inpaint_nans(Image.*FilteredBW, Args.InpaintMethod);

        % final smoothing
        if isempty(Args.SmoothFilter)
            SmBackEst = BackEst;
        else
            if isa(Args.SmoothFilter,'function_handle')
                SmFilter = Args.SmoothFilter(Args.SmoothFilterArgs{:});
            else
                SmFilter = Args.SmoothFilter;
            end
            SmBackEst = imUtil.filter.conv2_fast(BackEst, SmFilter, [], Args.PadMethod);
        end
        Back      = SmBackEst;
        if Args.VarFromBack
            Var       = SmBackEst;
        end
    end
    
end