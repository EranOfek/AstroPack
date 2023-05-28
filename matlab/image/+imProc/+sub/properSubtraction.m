function Result = properSubtraction(ObjNew, ObjRef, Args)
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

 
    for Imax=1:1:Nmax
        Ir = min(N_R, Imax);
        In = min(N_N, Imax);

        % subtract background
        N = ObjNew(In).Image - ObjNew(In).Back;
        if isempty(Args.RefBack)
            R = ObjRef(Ir).Image - ObjRef(Ir).Back;
        else
            R = ObjRef(Ir).Image - Args.RefBack;
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
        D=ifft2(D_hat);
        S_hat = D_hat.*conj(Pd_hat);
        S = ifft2(S_hat);
        S = S - median(S,'all','omitnan');
        S = S./tools.math.stat.rstd(S,'all');


        [D, Pd, S, Scorr] = imUtil.properSub.subtraction(N, R, Pn, Pr, SigmaN, SigmaR, 'SigmaAstN',[0.1 0.1], 'SigmaAstR',[0.1 0.1], 'EmpiricalNorm',true);

        % remove from D regions that are NaNs in R or N
        D(FlagNaN) = NaN;
        S(FlagNaN) = NaN;
        Scorr(FlagNaN) = NaN;

        %ds9(single(abs(S)>5).*S,3)
        'a'




    end

end
