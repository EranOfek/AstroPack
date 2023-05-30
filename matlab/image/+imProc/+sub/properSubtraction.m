function [D, S, Scorr, Z2] = properSubtraction(ObjNew, ObjRef, Args)
    %
    % Example: AIreg=imProc.transIm.imwarp(AI, AI(1), 'FillValues',NaN,'CreateNewObj',true);
    %          AIreg= imProc.background.background(AIreg,'SubSizeXY',[]);    
    %          AIreg=imProc.sources.findMeasureSources(AIreg);           
    %          m=imProc.match.match(AIreg(1),AIreg(2),'CooType','pix')

    %          D = imProc.sub.properSubtraction(AIreg(2), AIreg(1));

    arguments
        ObjNew AstroImage
        ObjRef AstroImage
        Args.backgroundArgs cell          = {'SubSizeXY',[]};
        Args.MeanVarFun function_handle   = @tools.math.stat.nanmean;
        Args.RefBack                      = [];  % can use to overide Ref background level
        Args.ReplaceNanN logical          = true;
        Args.ReplaceNanR logical          = true;

        Args.NewZP                        = 'PH_ZP';
        Args.RefZP                        = 'PH_ZP';

    end

    N_N  = numel(ObjNew);
    N_R  = numel(ObjRef);
    Nmax = max(N_N, N_R);

    % --- Check for empty data and generate if needed ---
    % check for background and variance
    % check for PSF
    [IsEmptyNewImage, IsEmptyNewBack, IsEmptyNewVar, IsEmptyNewPSF] = isemptyImage(ObjNew, {'Image','Back','Var', 'PSF'});
    [IsEmptyRefImage, IsEmptyRefBack, IsEmptyRefVar, IsEmptyRefPSF] = isemptyImage(ObjRef, {'Image','Back','Var', 'PSF'});

    % Image
    if any(IsEmptyNewImage)
        error('%d out of %d ImageData property in the New images are empty', sum(IsEmptyNewImage), N_N);
    end
    if any(IsEmptyRefImage)
        error('%d out of %d ImageData property in the Ref images are empty', sum(IsEmptyRefImage), N_R);
    end
    % Back and Var
    if any(IsEmptyNewBack) || any(IsEmptyNewVar)
        % some Background/Variance are missing
        % recalculate Back and Var
        ObjNew = imProc.background.background(ObjNew, Args.backgroundArgs{:});
    end
    if any(IsEmptyRefBack) || any(IsEmptyRefVar)
        % some Background/Variance are missing
        % recalculate Back and Var
        ObjRef = imProc.background.background(ObjRef, Args.backgroundArgs{:});
    end
    % PSF
    if any(IsEmptyNewPSF)
        error('%d out of %d PSFData property in the New images are empty', sum(IsEmptyNewPSF), N_N);
    end
    if any(IsEmptyRefPSF)
        error('%d out of %d PSFData property in the Ref images are empty', sum(IsEmptyRefPSF), N_R);
    end

    D     = ObjNew.copy;
    S     = ObjNew.copy;
    Scorr = ObjNew.copy;
    Z2    = ObjNew.copy;

    for Imax=1:1:Nmax
        Ir = min(N_R, Imax);
        In = min(N_N, Imax);

        % subtract background
        Nwb = ObjNew(In).Image;
        Rwb = ObjRef(Ir).Image;

        N = ObjNew(In).Image - ObjNew(In).Back;
        BackN = ObjNew(In).Back;
        if isempty(Args.RefBack)
            R = ObjRef(Ir).Image - ObjRef(Ir).Back;
            BackR = ObjRef(Ir).Back;
        else
            R = ObjRef(Ir).Image - Args.RefBack;
            BackR = Args.RefBack;
        end

        % Find regions in N or R that are NaN (for later on)
        FlagNaN = isnan(N) | isnan(R); 

        % replace NaNs
        if Args.ReplaceNanN
            N = imUtil.image.replaceVal(N, NaN, 0);
        end
        if Args.ReplaceNanR
            R = imUtil.image.replaceVal(R, NaN, 0);
        end

        % get photometriz zer point
        if ischar(Args.NewZP)
            ZP_New = ObjNew(In).HeaderData.getVal(Args.NewZP);
            Fn     = 10.^(-0.4.*ZP_New);
        else
            Fn     = Args.NewZP;
        end
        if ischar(Args.RefZP)
            ZP_Ref = ObjRef(In).HeaderData.getVal(Args.RefZP);
            Fr     = 10.^(-0.4.*ZP_Ref);
        else
            Fr     = Args.RefZP;
        end
        % normalize Fr and Fn such that Fn=1
        Fr = Fn./Fr; %Fr./Fn; - why?
        Fn = 1;
        

        % get PSF and pad and shift
        Pr = ObjRef(Ir).PSFData.padShift(size(R), 'fftshift','fftshift');
        Pn = ObjNew(In).PSFData.padShift(size(N), 'fftshift','fftshift');
        
        

        % get std
        SigmaR = sqrt(Args.MeanVarFun(ObjRef(Ir).Var, 'all'));
        SigmaN = sqrt(Args.MeanVarFun(ObjNew(In).Var, 'all'));
                
        



        % Image subtraction
        R_hat = fft2(R);
        N_hat = fft2(N);
        Pr_hat = fft2(Pr);
        Pn_hat = fft2(Pn);
        [D_hat, Pd_hat, Fd, D_den, D_num, D_denSqrt] = imUtil.properSub.subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr);
        ImageD=ifft2(D_hat);
        
        Pd     = ifft2(Pd_hat);
        Pd     = imUtil.psf.full2stamp(Pd);

        tic;
        S_hat = D_hat.*conj(Pd_hat);
        ImageS = ifft2(S_hat);
        toc

        tic;
        ImageS = imUtil.filter.filter2_fast(ImageD, Pd);
        toc


        FlagN      = isnan(Nwb);
        Nwb(FlagN) = median(BackN,'all','omitnan');
        FlagN      = isnan(Rwb);
        Rwb(FlagN) = median(BackR,'all','omitnan');

        [Kr_hat, Kn_hat, V_Sr, V_Sn, Vcorr] = imUtil.properSub.sourceNoise(Fr, Fn, Pr_hat, Pn_hat, D_den, Nwb.*4.7, Rwb.*4.7);
        ImageScorr = ImageS./sqrt(Vcorr);
        
        %VcorrNorm = Vcorr./median(Vcorr,'all','omitnan');
        %ImageScorr = ImageS./VcorrNorm;

        [ImageZ2,Zhat,Norm] = imUtil.properSub.translient(N.*Fn, R.*Fr, Pn, Pr, SigmaN, SigmaR);
        ImageZ2 = ImageZ2 - median(ImageZ2,'all','omitnan');
        ImageZ2 = ImageZ2./tools.math.stat.rstd(ImageZ2,'all');

        % FrVec = (0.9:0.01:1.1)';
        % Nr = numel(FrVec);
        % for II=1:1:Nr
        %     Fr= FrVec(II)

        %[ImageD, Pd, ImageS, ImageScorr] = imUtil.properSub.subtraction(N, R, Pn, Pr, SigmaN, SigmaR, 'Fn',Fn, 'Fr',Fr, 'SigmaAstN',[0.05 0.05], 'SigmaAstR',[0.05 0.05], 'EmpiricalNorm',false);

        % remove from D regions that are NaNs in R or N
        ImageD(FlagNaN) = NaN;
        ImageS(FlagNaN) = NaN;
        ImageScorr(FlagNaN) = NaN;

        ImageS = ImageS - median(ImageS,'all','omitnan');
        ImageS = ImageS./tools.math.stat.rstd(ImageS,'all');
        
        ImageScorr = ImageScorr - median(ImageScorr,'all','omitnan');
        ImageScorr = ImageScorr./tools.math.stat.rstd(ImageScorr,'all');


        % D(II).Image = ImageD;
        % D(II).PSF   = Pd;
        % 
        % S(II).Image = ImageS;
        % S(II).PSF   = Pd;
        % 
        % Scorr(II).Image = ImageScorr;
        % Scorr(II).PSF   = Pd;

        D(In).Image = ImageD;
        D(In).PSF   = Pd;

        S(In).Image = ImageS;
        S(In).PSF   = Pd;

        Scorr(In).Image = ImageScorr;
        Scorr(In).PSF   = Pd;

        Z2(In).Image = ImageZ2;

        %end
        %ds9(single(abs(S)>5).*S,3)
        'a'

    end

end
