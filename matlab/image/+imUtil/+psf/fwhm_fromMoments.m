function [Result] = fwhm_fromMoments(Image, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Nov) 
    % Example: 

   


    arguments
        Image
        Args.CCDSEC       = [];

        Args.Threshold    = 20;

        Args.HalfSize     = [];
        Args.MinSN        = 50;
        Args.Background   = [];
        Args.Variance     = [];
        Args.SigmaVec     = logspace(0,2,5).';
        Args.KernelFun    = @imUtil.kernel2.gauss; % @imUtil.kernel2.gauss;
        Args.MinStars     = 5;
        Args.PixScale     = 1;
        Args.Method       = 'bisec';
        Args.MaxIter      = 6;
    end

    Image = single(Image);

    if ~isempty(Args.CCDSEC)
        Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));

    else
        if ~isempty(Args.HalfSize)
            SizeIm   = size(Image);
            CenterIm = floor(SizeIm.*0.5);
            Args.CCDSEC = [CenterIm(2)-Args.HalfSize, CenterIm(2)+Args.HalfSize, CenterIm(1)-Args.HalfSize, CenterIm(1)+Args.HalfSize];    
            Image = Image(Args.CCDSEC(1,3):Args.CCDSEC(1,4), Args.CCDSEC(1,1):Args.CCDSEC(1,2));
        end
    end

    % subtract background
    Back = median(Image,'all','omitnan');
    Var  = tools.math.stat.rstd(Image).^2;

    [Result,Template,FiltImage,FiltImageVar] = tools.sources.findSources(Image, 'Threshold',Args.Threshold, )


    switch lower(Args.Method)
        case {'maxndet','maxndetinterp'}
            % Choose the template that maximize the SN for the largest number
            % of stars, ecluding the shaprpest star

            Args.SigmaVec = [0.1; Args.SigmaVec(:)];  % add a sharp object (always first) to bank of templates

            % filter image with filter bandk of gaussians with variable width
            SN = imUtil.filter.filter2_snBank(Image, Args.Background, Args.Variance, Args.KernelFun, Args.SigmaVec);
            % Pos contains: [X,Y,SN,index]
            [~,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,Args.MinSN);

end
