function [Pos,FI, Back, Var] = findRings(Image, Args)
    % Find rings in image using match filter template bank.
    % Input  : - An image (matrix).
    %          * ...,key,val,... 
    %            'Back' - Background level. If empty, do nothing (unless
    %                   'CalcBack' is true). Defult is [].
    %            'Var' - Variance level. If empty, do nothing (unless
    %                   'CalcBack' is true). Defult is [].
    %            'RingTempBank' - A cube of templates.
    %                   Or a two column matrix of [inner outer] radii.
    %                   Default is [11 20; 15 33; 21 50; 31 60]
    %            'CalcBack' - If true, then calc back and variance.
    %                   Default is false.
    %            'FunBackVar' - Function for back and var calculation.
    %                   Default is @imUtil.background.modeVar_LogHist
    %            'FunBackVarArgs' - A cell array of additional arguments to pass to the
    %                   FunBackVar function. Default is {}.
    %            'Threshold' - Detection threshold. Default is 50.
    % Output : - A five column matrix of [X,Y,SN,ImageIndex,LinaerIndexIn2D].
    %          - Filtered image noramlized by the std.
    %          - Background.
    %          - Variance.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Pos,FI]=imUtil.sources.findRings(Img, 'CalcBack',true);

    arguments
        Image
        Args.Back              = [];
        Args.Var               = [];
        Args.RingsTempBank     = [11 20; 15 33; 21 50; 31 60]; % or cube
        Args.CalcBack          = false;
        Args.FunBackVar        = @imUtil.background.modeVar_LogHist;
        Args.FunBackVarArgs    = {};
        Args.Threshold         = 50;
    end

    if Args.CalcBack
        % estimate Back and Var
        [Args.Back, Args.Var] = Args.FunBackVar(Image, Args.FunBackVarArgs{:});
    end

    if ~isempty(Args.Back)
        Image = Image - Args.Back;
    end
    %if ~isempty(Args.Var)
    %    Image = Image./sqrt(Args.Var);
    %end

    if size(Args.RingsTempBank,2)==2
        % RingTempBank contains [Inner, Outer] radii
       
        MaxRad = max(Args.RingsTempBank,[],'all');
        Args.RingsTempBank = imUtil.kernel2.annulus(Args.RingsTempBank, [MaxRad MaxRad].*2+1);
    end

    FI = imUtil.filter.filter2_fast(Image, Args.RingsTempBank);
    Norm2PSF = sqrt(sum(Args.RingsTempBank.^2,[1 2]));
    FI = FI./(sqrt(Args.Var).*Norm2PSF);
    
    [Pos] = imUtil.sources.findLocalMax(FI,'Variance',1,'Threshold',Args.Threshold);

    Back = Args.Back;
    Var  = Args.Var;

end
