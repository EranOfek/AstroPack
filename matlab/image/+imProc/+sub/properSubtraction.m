function [D, S, Scorr, Z2, S2, F_S, SdN, SdR, Fd] = properSubtraction(ObjNew, ObjRef, Args)
    % 
    % Example: AIreg=imProc.transIm.imwarp(AI, AI(1), 'FillValues',NaN,'CreateNewObj',true);
    %          AIreg= imProc.background.background(AIreg,'SubSizeXY',[]);    
    %          AIreg=imProc.sources.findMeasureSources(AIreg);           
    %          m=imProc.match.match(AIreg(1),AIreg(2),'CooType','pix')

    %          [D,S,Scorr,Z2, S2, F_S, SdN, SdR] = imProc.sub.properSubtraction(AIreg(2), AIreg(1));

    arguments
        ObjNew AstroImage
        ObjRef AstroImage

        Args.RemoveResidBack logical      = true;
        Args.backgroundArgs cell          = {'SubSizeXY',[]};
        Args.MeanVarFun function_handle   = @tools.math.stat.nanmean;
        Args.RefBack                      = [];  % can use to overide Ref background level
        Args.ReplaceNanN logical          = true;
        Args.ReplaceNanR logical          = true;

        Args.NewZP                        = 'PH_ZP';
        Args.RefZP                        = 'PH_ZP';
        
        Args.SigmaAstN                    = 0.1;   % or keyword
        Args.SigmaAstR                    = 0.1;   % or keyword
        
        Args.AbsFun           = @(X) abs(X);
        Args.Eps              = 0;
        
        Args.NormS logical    = true;
        Args.NormD logical    = false;

        Args.NormDbyFd logical = true;
        Args.HalfSizePSF       = [];   % if empty do not touch
        Args.full2stampArgs cell = {};
        Args.CalcTranslient logical    = true;  % if false will skip Z2 and S2
        
        Args.SuppressEdgesPSF logical   = true;  % operational only if HalfSizePSF not empty
        Args.SuppressEdgesAnnulus       = [5 7]; % arguments for cosbell supression
        
    end

    if 1==0
        % Debug code:

        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/171342v2
        AI(1) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');

        %AI(1) = AstroImage.readFileNamesObj('LAST.01.02.01_20230425.215545.030_clear_185-02_001_001_010_sci_coadd_Image_1.fits');

        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/172022v2
        AI(2) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');
        %AI(2) = AstroImage.readFileNamesObj('LAST.01.02.01_20230425.214904.914_clear_185-02_001_001_010_sci_coadd_Image_1.fits');
        
        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/181501v2
        AI(3) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');

        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/9/
        AI(4) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');

        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/10/
        AI(5) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');

        cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.02/2023/04/25/proc/11/
        AI(6) = AstroImage.readFileNamesObj('LAST.01.02.01_2*010_sci_coadd_Image_1.fits');


        %AIreg=imProc.transIm.imwarp(AI, AI(1), 'FillValues',NaN,'CreateNewObj',true);
        AIreg=imProc.transIm.interp2wcs(AI, AI(1), 'InterpMethod','cubic');

        % shift by 0.1 pix (for testing translient)
        AIshift = imProc.transIm.imwarp(AIreg, [0.1 0.1])

        AIreg= imProc.background.background(AIreg,'SubSizeXY',[]); %[256 256]);  
        AIreg=imProc.sources.findMeasureSources(AIreg);           
        % add RA,Dec
        AIreg = imProc.astrometry.addCoordinates2catalog(AIreg);

        %m=imProc.match.match(AIreg(1),AIreg(5),'CooType','pix');


        % The astrometric solutions are good
        %R=imProc.astrometry.astrometryQualityData(AI);

        % bad on image 4:
        %R=imProc.astrometry.astrometryQualityData(AIreg);

        ds9(AIreg(1),1)
        ds9(AIreg(2),2)


        [DD,S,Scorr,Z2,S2, F_S,SdN, SdR] = imProc.sub.properSubtraction(AIreg(3), AIreg(1) ,'HalfSize',[8 8]);
        [DDs,Ss,Scorrs,Z2s,S2s, F_Ss,SdNs, SdRs] = imProc.sub.properSubtraction(AIshift(3), AIreg(1) ,'HalfSize',[8 8]);

        R=imProc.sources.findTransients(AIreg(3), AIreg(1), DD, S, Scorr, Z2, S2);
    end



    N_N  = numel(ObjNew);
    N_R  = numel(ObjRef);
    Nmax = max(N_N, N_R);

    % --- Check for empty data and generate if needed ---
    % check for background and variance
    % check for PSF
    % Check New
    if isa(ObjNew, 'AstroImage')
        [IsEmptyNewImage, IsEmptyNewBack, IsEmptyNewVar, IsEmptyNewPSF] = isemptyImage(ObjNew, {'Image','Back','Var', 'PSF'});
        % Image
        if any(IsEmptyNewImage)
            error('%d out of %d ImageData property in the New images are empty', sum(IsEmptyNewImage), N_N);
        end
        % Back and Var
        if any(IsEmptyNewBack) || any(IsEmptyNewVar)
            % some Background/Variance are missing
            % recalculate Back and Var
            ObjNew = imProc.background.background(ObjNew, Args.backgroundArgs{:});
        end
        % PSF
        if any(IsEmptyNewPSF)
            error('%d out of %d PSFData property in the New images are empty', sum(IsEmptyNewPSF), N_N);
        end
    end
    % Check Ref
    if isa(ObjRef, 'AstroImage')    
        [IsEmptyRefImage, IsEmptyRefBack, IsEmptyRefVar, IsEmptyRefPSF] = isemptyImage(ObjRef, {'Image','Back','Var', 'PSF'});

        if any(IsEmptyRefImage)
            error('%d out of %d ImageData property in the Ref images are empty', sum(IsEmptyRefImage), N_R);
        end    
    
        if any(IsEmptyRefBack) || any(IsEmptyRefVar)
            % some Background/Variance are missing
            % recalculate Back and Var
            ObjRef = imProc.background.background(ObjRef, Args.backgroundArgs{:});
        end    
    
        if any(IsEmptyRefPSF)
            error('%d out of %d PSFData property in the Ref images are empty', sum(IsEmptyRefPSF), N_R);
        end
    end

    % Allocate outputs:
    D     = ObjNew.copy;
    S     = ObjNew.copy;
    S2    = ObjNew.copy;
    Scorr = ObjNew.copy;
    Z2    = ObjNew.copy;
    SdN   = ObjNew.copy;
    SdR   = ObjNew.copy;

    Fd    = zeros(Nmax,1);
    F_S   = zeros(Nmax,1);
    for Imax=1:1:Nmax
        Ir = min(N_R, Imax);
        In = min(N_N, Imax);

        % keep the new and ref with background:
        Nwb = ObjNew(In).Image;
        Rwb = ObjRef(Ir).Image;

        % subtract background
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

        % get photometric zero point
        if ischar(Args.NewZP)
            ZP_New = ObjNew(In).HeaderData.getVal(Args.NewZP);
            Fn     = 10.^(-0.4.*ZP_New);
        else
            Fn     = Args.NewZP;
        end
        if ischar(Args.RefZP)
            ZP_Ref = ObjRef(Ir).HeaderData.getVal(Args.RefZP);
            Fr     = 10.^(-0.4.*ZP_Ref);
        else
            Fr     = Args.RefZP;
        end
        % normalize Fr and Fn such that Fn=1
        Fr = Fn./Fr; %Fr./Fn; - why?
        Fn = 1;
        
        % get PSF and pad and shift 
        Pn = ObjNew(In).PSFData.padShift(size(N), 'fftshift','fftshift', 'OutType','cube');
        Pr = ObjRef(Ir).PSFData.padShift(size(R), 'fftshift','fftshift', 'OutType','cube');
        
        % get std
        SigmaN = sqrt(Args.MeanVarFun(ObjNew(In).Var, 'all'));
        SigmaR = sqrt(Args.MeanVarFun(ObjRef(Ir).Var, 'all'));
        
        if Args.RemoveResidBack
            N = N - median(N,'all','omitnan');
            R = R - median(R,'all','omitnan');
        end

        
        % Image subtraction
        N_hat = fft2(N);
        R_hat = fft2(R);
        Pn_hat = fft2(Pn);
        Pr_hat = fft2(Pr);
        
        % replace NaNs in Nwb and Rwb with median values
        FlagN      = isnan(Nwb);
        Nwb(FlagN) = median(BackN,'all','omitnan');
        FlagN      = isnan(Rwb);
        Rwb(FlagN) = median(BackR,'all','omitnan');
        
        [ImageScorr, ImageS, ImageD, Pd, Fd(Imax), F_S(Imax), D_den, D_num, D_denSqrt, SdeltaN, SdeltaR] = imUtil.properSub.subtractionScorr(N_hat, R_hat,...
                                                                              Pn_hat, Pr_hat,...
                                                                              SigmaN, SigmaR,...
                                                                              Fn, Fr,...
                                                                              'VN',Nwb,...
                                                                              'VR',Rwb,...
                                                                              'SigmaAstN',Args.SigmaAstN,...
                                                                              'SigmaAstR',Args.SigmaAstR,...
                                                                              'AbsFun',Args.AbsFun,...
                                                                              'Eps',Args.Eps,...
                                                                              'SetToNaN',FlagNaN,...
                                                                              'NormS',Args.NormS,...
                                                                              'NormD',Args.NormD,...
                                                                              'NormDbyFd',Args.NormDbyFd,...
                                                                              'IsFFT',true);
                                                                               

        if Args.CalcTranslient
            [ImageZ2,Zhat,Norm] = imUtil.properSub.translient(N, R, Pn, Pr, SigmaN, SigmaR);
            % move this to the translient code
            ImageZ2(FlagNaN) = NaN;
    
            k = 1;  % chi^2 with k=1 dof
            ExpectedMedian = k.*(1 - 2./(9.*k)).^3;
    
            %ImageZ2 = ImageZ2 - median(ImageZ2,'all','omitnan') + ExpectedMedian;
            ImageZ2 = ImageZ2./tools.math.stat.rstd(ImageZ2,'all').*sqrt(2.*k);
            Z2(Imax).Image = ImageZ2;
        
            ImageS2 = ImageS.^2;
            ImageS2 = ImageS2./tools.math.stat.rstd(ImageS2,'all').*sqrt(2.*k);

            S2(Imax).Image    = ImageS2;
            S2(Imax).MaskData = D(Imax).MaskData;

        end  

        if ~isempty(Args.HalfSizePSF)
            Pd = imUtil.psf.full2stamp(Pd, 'StampHalfSize',Args.HalfSizePSF, Args.full2stampArgs{:});
            if Args.SuppressEdgesPSF
                Pd = imUtil.psf.suppressEdges(Pd, 'FunPar',Args.SuppressEdgesAnnulus);
            end
        end

        D(Imax).Image = ImageD;
        D(Imax).PSF   = Pd;
        % propagate the mask image
        D(Imax).MaskData = funBinary(ObjNew(In).MaskData, ObjRef(Ir).MaskData,@bitor, 'CreateNewObj',true);


        S(Imax).Image = ImageS;
        S(Imax).PSF   = Pd;
        S(Imax).MaskData = D(Imax).MaskData;

        
        Scorr(Imax).Image = ImageScorr;
        Scorr(Imax).PSF   = Pd;
        Scorr(Imax).MaskData = D(Imax).MaskData;

        

        SdN(Imax).Image = SdeltaN;
        SdR(Imax).Image = SdeltaR;

    end

end
