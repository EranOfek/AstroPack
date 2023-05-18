function Result = properSubtraction(ObjNew, ObjRef, Args)
    %

    arguments
        ObjNew AstroImage
        ObjRef AstroImage
        Args.backgroundArgs cell          = {};
        Args.MeanVarFun fumction_handle   = @tools.math.stat.nanmedian;
        Args.RefBack                      = [];  % can use to overide Ref background level
        Args.ReplaceNanN logical          = true;
        Args.ReplaceNanR logical          = true;
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
        In = min(N_N, Imax);
        Ir = min(N_R, Imax);

        % get PSF and pad and shift
        Pn = ObjNew(In).PSFData.getPSF;
        Pr = ObjRef(Ir).PSFData.getPSF;

        % get std
        Sigma_n = Args.MeanVarFun(ObjeNew(In).Var);
        Sigma_r = Args.MeanVarFun(ObjeRef(Ir).Var);

        % get flux normalization
        %Fn = 
        %Fr = 

        % subtract background
        N = ObjNew(In).Image - ObjNew(In).Back;
        if isempty(Args.RefBack)
            R = ObjRef(Ir).Image - ObjRef(Ir).Back;
        else
            R = ObjRef(Ir).Image - Args.RefBack;
        end

        % replace NaNs
        if Args.ReplaceNanN
            N = ImUtil.image.replaceVal(N, NaN, 0);
        end
        if Args.ReplaceNanR
            R = ImUtil.image.replaceVal(R, NaN, 0);
        end

        % Image subtraction



    end

end
