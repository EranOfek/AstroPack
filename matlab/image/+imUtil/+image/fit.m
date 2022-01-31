function Result = fit(Image, Model, Var, Args)
    % Return the chi^2 of a model to an image.
    % Input  : - A matrix (Image).
    %          - A matrix (Model).
    %          - Amatrix or scalar of image variance (or std).
    %            Default is 1.
    %          * ...,key,val,...
    %            'IsVar' - A logical indicatig if the Variance image is
    %                   variance (true), or std (false). Default is true.
    %            'SigmaClip' - [Low, High] sigmacliping.
    %                   Default is [5 5].
    %            'Niter' - Number of sigma clipping iterations.
    %                   For no iteration use Niter=1 and SigmaCllip=[Inf Inf].
    %            'UseLSQ' - Use \ for finding the flux ratio, between the
    %                   image and model.
    %                   If false, will use the RatioFun.
    %                   Default is false.
    %            'RatioFun' - A function handle for the ratio function to
    %                   be used in order to estimate the image to model ratio.
    %                   Default is @median.
    %            'RatioFunArgs' - A cell array of arguments to pass to the
    %                   RatioFun. Default is {'omitnan'}.
    % Output : - A structure containing the best fit information:
    %            .Std - std of best fit.
    %            .Chi2 - chi^2
    %            .Dof - DOF
    %            .Ratio - Image/Model ratio.
    % Author : Eran Ofek (Jan 2022)
    % Example: Image = 1+randn(1000,1000); Model=10+randn(1000,1000);
    %          R = imUtil.image.fit(Image, Model);
   
    arguments
        Image
        Model
        Var                             = 1;
        Args.IsVar logical              = true;
        Args.SigmaClip                  = [5 5];
        Args.Niter                      = 2;
        Args.UseLSQ logical             = false;
        Args.RatioFun function_handle   = @median;
        Args.RatioFunArgs cell          = {'omitnan'};
    end
    
    Args.SigmaClip = abs(Args.SigmaClip);
    
    if numel(Var)==1
        Var = Var + zeros(size(Image));
    end
    
    Npix = numel(Image);
    Flag = true(Npix,1);
    for Iiter=1:1:Args.Niter
        if Args.UseLSQ
            Ratio = Image(Flag)\Model(Flag);
        else
            Ratio = Args.RatioFun(Image(Flag)./Model(Flag), [1], Args.RatioFunArgs{:});
        end
        Resid = Image - Ratio.*Model;
        Result.Std  = std(Resid,[],[1 2]);
        S = Resid./Result.Std;
        Flag = S(:)>-Args.SigmaClip(1) & S(:)<Args.SigmaClip(2);
    end
    
    if Args.IsVar
        Result.Chi2 = sum(Resid(Flag).^2./Var(Flag),[1]);
    else
        % Var is Std
        Result.Chi2 = sum((Resid(Flag)./Var(Flag)).^2,[1]);
    end
    
    Result.Dof   = sum(Flag) - 1;
    Result.Ratio = Ratio;
    
end