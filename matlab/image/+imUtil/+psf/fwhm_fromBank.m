function [FWHM,Nstars]=fwhm_fromBank(Image,Args)
    % Measure the FWHM of an image by cross corr. with a Gaussian template bank
    % Package: +imUtil.psf
    % Description: Measure the FWHM of an image by cross corr. with a Gaussian
    %              template bank and choose the best FWHM by the mode of most
    %              detection above some S/N.
    % Input  : - An image in matrix format.
    %          * list of ...,key,val,...
    %            'CCDSEC' - CCDSEC [Xmin Xmax Ymin Ymax] of region in which to
    %                   measure FWHM. If empty use entire image. Default is [].
    %            'HalfSize' - Image half size. If 'CCDSEC' is empty, and this
    %                   argument is provided, then run this program on centeral
    %                   image with this half size. Default is [].
    %            'MinSN' - Minimum S/N to use. Default is 50.
    %            'Background' - A background image. Default is [].
    %            'Variance'   - A variance image. Default is [].
    %            'SigmaVec'   - Vector of the Gaussian bank sigmas.
    %                           This should not include a sharp object (such a
    %                           sharp object is added by the code).
    %                           Default is logspace(0,2,5).
    %            'KernelFun'  - Function handle for kernel function.
    %                           Default is @imUtil.kernel2.gauss
    %            'MinStars'   - Minimum numbre of stars needed to estimate
    %                           FWHM. Default is 5.
    %            'PixScale'   - Pixel scale ["/pix]. Default is 1.
    %            'Method'     - Method: 
    %                           'bisec' - Bi-sector search
    %                           'MaxNdet' - Choose the filter with the max
    %                                   number of detections.
    %                           'MaxNdetInterp' - Same as 'MaxNdet', but with
    %                                   interpolation over number of detections.
    %                           Default is 'bisec'.
    %            'MaxIter'    - Numbre of iterations for the 'bisec' method.
    %                           Default is 6.
    % Output : - FWHM [arcsec].
    %          - Number of stars used for estimating the FWHM.
    % Author : Eran Ofek (Mar 2021)
    % Example: FWHM=imUtil.psf.fwhm_fromBank(AI.Image);


    arguments
        Image
        Args.CCDSEC       = [];
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


    switch lower(Args.Method)
        case {'maxndet','maxndetinterp'}
            % Choose the template that maximize the SN for the largest number
            % of stars, ecluding the shaprpest star

            Args.SigmaVec = [0.1; Args.SigmaVec(:)];  % add a sharp object (always first) to bank of templates

            % filter image with filter bandk of gaussians with variable width
            SN = imUtil.filter.filter2_snBank(Image, Args.Background, Args.Variance, Args.KernelFun, Args.SigmaVec);
            % Pos contains: [X,Y,SN,index]
            [~,Pos,MaxIsn]=imUtil.image.local_maxima(SN,1,Args.MinSN);

            % remove sharp objects
            Pos = Pos(Pos(:,4)~=1,:);

            Nstars = size(Pos,1);

            if Nstars<Args.MinStars
                FWHM = NaN;
            else

                switch lower(Args.Method)
                    case 'maxndet'
                        % Choose the templates (FWHM) that has the maximum number of max-detections
                        FWHM = 2.35.*Args.PixScale.*Args.SigmaVec(mode(Pos(:,4),'all'));
                    case 'maxndetinterp'
                        % Choose the templates (FWHM) that has the maximum number of max-detections
                        % but interplate over the number of max-detections for each templates
                        Ntemp  = numel(Args.SigmaVec);
                        Ncount = histcounts(Pos(:,4),(0.5:1:Ntemp+0.5)').';
                        LinSpace = (1:1:Ntemp)';
                        Extram = tools.find.find_local_extremum(LinSpace, Ncount);
                        Extram = Extram(Extram(:,3)<0,:);
                        if isempty(Extram)
                            FWHM = NaN;
                        else
                            [~,Imax]  = max(Extram(:,2));
                            TempInd   = Extram(Imax,1);
                            BestSigma = interp1(LinSpace, Args.SigmaVec, TempInd, 'cubic');
                            FWHM      = 2.35.*Args.PixScale.*BestSigma;
                        end
                    otherwise
                        error('Unknown Method option');
                end
            end
        case 'bisec'
            % bi-sector method

            
            
            Nsigma        = numel(Args.SigmaVec);
            Args.SigmaVec = Args.SigmaVec(:);
            
            SigmaVec = [0.1; Args.SigmaVec];  % add a sharp object (always first) to bank of templates

            if isempty(Args.Background) || isempty(Args.Variance)
                Med1 = median(Image,"all","omitnan");

                F1   = Image>(Med1 - 10.*sqrt(Med1)) & Image<(Med1 + 5.*sqrt(Med1));
                Med2 = median(Image(F1), "all", "omitnan");
                Args.Background  = Med2;
                Args.Variance    = var(Image(F1), [], "all", "omitnan");

                %[Args.Background, Args.Variance] =  imUtil.background.modeVar_LogHist(Image);
            end

            Cont = true;
            I = 0;
            while Cont
                I = I + 1;
                %for I=1:1:Args.MaxIter
                % filter image with filter bandk of gaussians with variable width

                
                SN = imUtil.filter.filter2_snBank(Image, Args.Background, Args.Variance, Args.KernelFun, SigmaVec(:));
                %SN = imUtil.filter.filter2_snBank(Image, Args.Background, Args.Variance, Args.KernelFun, SigmaVec);
                % Pos contains: [X,Y,SN,index]
                [~,Pos,MaxIsn]=imUtil.image.local_maxima(SN, 1, Args.MinSN);

                % remove sharp objects
                Pos = Pos(Pos(:,4)~=1,:);
                %Pos = Pos(Pos(:,4)~=Nsigma+1,:);

                Nstars = size(Pos,1);

                if Nstars<Args.MinStars
                    Cont    = false;
                    BestInd = NaN;
                else
                    BestInd = mode(Pos(:,4),'all');
                    if I<Args.MaxIter
                        
                        %SigmaVec(BestInd)
                        if BestInd>Nsigma
                            SigmaVec(end+1) = SigmaVec(end) + SigmaVec(end) - SigmaVec(end-1);
                        end
                        ShiftInd = 1;
                        SigmaVec = [0.1, logspace(log10(SigmaVec(BestInd-ShiftInd)), log10(SigmaVec(BestInd+ShiftInd)), Nsigma)];
                    end
                end

                if I==Args.MaxIter
                    Cont = false;
                end
            end

            if isnan(BestInd)
                FWHM = NaN;
            else
                FWHM = SigmaVec(BestInd).*2.35.*Args.PixScale;
            end


        otherwise
            error('Unknown Method option');
    end
end