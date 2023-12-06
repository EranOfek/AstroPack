function [Result, BackIm] = filterSources(Obj, Args)
    % Generate a background image filtered from sources using sucessive filtering
    %   The image is populated in the Back field or returned as a
    %   BackImage object.
    % Description: This routine filter out sources from an image and
    %       generate an approximate background image.
    %       The following steps are implemented:
    %       1. Filtering the image against template bank of sources.
    %           The first guess back and var will be taken from the Back
    %           and Var fields.
    %       2. Set the value around sources found by step 1 to NaN.
    %       3. Use inpaint_nans to interpolate over NaNs.
    %       4. Smooth the image using another filter (default is annulus).
    %       5. if Niter>1 go to step 1, and find sources this time using
    %          the newly estimated background.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'DataProp' - Data property on which to filter the sources.
    %                   Default is 'Image'.
    %            'UpdateBack' - A logical indicating if to update the
    %                   'Back' property in the AstroImage with the filtered
    %                   image. Default is true.
    %            'UpdateVar' - A logical indicating if to update the
    %                   'Var' property in the AstroImage with the filtered
    %                   image. Default is true.
    %                   Note that this is meaningful, only if gain was set
    %                   to 1.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    %            'Threshold' - Sources detection threshold (sigmas).
    %                   Default is 3.
    %            'BackFun' - Function handle for background estimation (if
    %                   not available in the Back field).
    %                   Default is @median.
    %            'BackFunArgs' - A cell array of additional arguments to
    %                   pass to the background estimation function.
    %                   Default is {[1 2],'omitnan'}.
    %            'VarFun' - Function handle for variance estimation (if not
    %                   avaialble in the Var field).
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
    % Output : - An updated AstroImage object.
    %          - An optional BackImage object with the smoothed background
    %            images.
    % Author : Eran Ofek (Jul 2021)
    % Example: AI=AstroImage('PTF_Cropped.fits');
    %          AI=cast(AI,'double'); AI.Image = AI.Image.*1.6;
    %          imProc.background.filterSources(AI)
    
    arguments
        Obj AstroImage
        Args.DataProp                   = 'Image';
        Args.UpdateBack(1,1) logical    = true;
        Args.UpdateVar(1,1) logical     = true;
        Args.CreateNewObj               = [];
        %
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
    
    if isempty(Args.CreateNewObj)
        if nargout==0
            % update existing object
            Result = Obj;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Args.CreateNewObj = false;
    end
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % check if back/var exist, and use them as first guess
        
        [SmBackEst] = imUtil.filter.filter_sources(Obj(Iobj).(Args.DataProp), 'Back',Obj(Iobj).Back,...
                                                                                       'Var', Obj(Iobj).Var,...
                                                                                       'Threshold',Args.Threshold,...
                                                                                       'BackFun',Args.BackFun,...
                                                                                       'BackFunArgs',Args.BackFunArgs,...
                                                                                       'VarFun',Args.VarFun,...
                                                                                       'VarFunArgs',Args.VarFunArgs,...
                                                                                       'Template',Args.Template,...
                                                                                       'TemplateArgs',Args.TemplateArgs,...
                                                                                       'RadiusNaN',Args.RadiusNaN,...
                                                                                       'InpaintMethod',Args.InpaintMethod,...
                                                                                       'SmoothFilter',Args.SmoothFilter,...
                                                                                       'SmoothFilterArgs',Args.SmoothFilterArgs,...
                                                                                       'PadMethod',Args.PadMethod,...
                                                                                       'VarFromBack',Args.VarFromBack,...
                                                                                       'Niter',Args.Niter);
 
        %
        if Args.UpdateBack
            Result(Iobj).Back = SmBackEst;
        end
        if Args.UpdateVar
            Result(Iobj).Var  = SmBackEst;
        end
        if nargout>1
            if Iobj==1
                BackIm = BackImage(size(Obj));
            end
            BackIm(Iobj).Image = SmBackEst;
        end
    end
    
end