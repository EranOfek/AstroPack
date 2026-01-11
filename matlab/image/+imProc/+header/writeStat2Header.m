function [AI] = writeStat2Header(AI, Args)
    % Write image statistics to header
    %   Given an AstroImage with Background, PSF, WCS, and catalog,
    %   write statistical data to header.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'WriteBack' - Write background stat. Default is true.
    %                   Including: mean background, range background.
    %            'WriteStars' - Write stars data. Default is true.
    %                   Including: n stars, magnitude quantile
    %            'WritePSF' - Write PSF data. Default is true.
    %                   Including: FWHM, A, B, Theta, Median X^2, Y^2, X*Y
    %            'WriteScale' - Write scale data. Default is true.
    %                   Including: Scale, Rotation
    %            For more arguments see code.
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (2026 Jan) 
    % Example: AI=imProc.header.writeStat2Header(AI);

    arguments
        AI

        Args.WriteBack         = true;
        Args.WriteStars        = true;
        Args.WritePSF          = true;
        Args.WriteScale        = true;

        Args.ColX2             = 'X2';
        Args.ColY2             = 'Y2';
        Args.ColXY             = 'XY';
        Args.ColMag            = 'MAG_PSF';
        Args.MagQuantile       = 0.95;

        % background related
        Args.KeyMeanBack       = 'MEAN_BCK';
        Args.KeyRangeBack      = 'RNG_BCK';
        % stars related
        Args.KeyNstars         = 'N_STARS';
        Args.KeyMagQuant       = 'MAG_95Q';
        % PSF realted
        Args.KeyFWHM           = 'FWHM';
        Args.KeyShapePSF       = {'FWHM_A', 'FWHM_B', 'FWHM_TH'};
        Args.KeySqrtSumPSF2    = 'SSPSF2';
        Args.KeyMedM2          = {'MED_X2', 'MED_Y2', 'MED_XY'};

        % astrometry related
        Args.KeyScaleRot       = {'PIXSCALE', 'ROTAT'};

    end
    ARCSEC_DEG = 3600;

    Keys = fieldnames(Args);
    Nkey = numel(Keys);

    if Args.WriteBack
        ColsBack = {Args.KeyMeanBack, Args.KeyRangeBack};
    else
        ColsBack = {};
    end
    if Args.WriteStars
        ColsStars = {Args.KeyNstars, Args.KeyMagQuant};
    else
        ColsStars = {};
    end
    if Args.WritePSF
        ColsPSF = {Args.KeyFWHM, Args.KeyShapePSF{:}, Args.KeySqrtSumPSF2, Args.KeyMedM2{:}};
    else
        ColsPSF = {};
    end
    if Args.WriteScale
        ColsScale = Args.KeyScaleRot;
    else
        ColsScale = {};
    end
    Cols = [ColsBack, ColsStars, ColsPSF, ColsScale];
    Ncols = numel(Cols);

    Nai = numel(AI);
    for Iai=1:1:Nai
        % 
        Data = nan(Ncols, 1);
        Idata = 0;
        if Args.WriteBack
            Idata = Idata + 1;
            Data(Idata) = mean(AI(Iai).BackData.Data, 'all', 'omitnan');
            Idata = Idata + 1;
            Data(Idata) = range(AI(Iai).BackData.Data, 'all', 'omitnan');
        end
        if Args.WriteStars
            Idata = Idata + 1;            
            Data(Idata) = size(AI(Iai).CatData.Catalog, 1);
            Idata = Idata + 1;
            Mag = AI(Iai).CatData.getCol(Args.ColMag);
            Data(Idata) = quantile(Mag, Args.MagQuantile);
        end
        if Args.WritePSF
            Idata = Idata + 1;
            Data(Idata) = AI(Iai).PSFData.fwhm;
            % SigmaX, SigmaY, Rho
            Idata = Idata + 1;
            [~,~,BestFit] = AI(Iai).PSFData.fitFunPSF();
            [A, B, Theta] = imUtil.psf.gaussianSigma2SemiAxis(BestFit{1}.Par(2), BestFit{1}.Par(3), BestFit{1}.Par(4));
            Data(Idata:Idata+2) = [A, B, Theta];
            Idata = Idata + 3;
            Data(Idata) = sqrt(sum(AI(Iai).PSFData.Data.^2, 'all'));
            Idata = Idata + 1;
            M2 = AI(Iai).CatData.getCol({Args.ColX2, Args.ColY2, Args.ColXY});
            Data(Idata:Idata+2) = median(M2, 1, 'omitnan');
            Idata = Idata + 2;
        if Args.WriteScale
            Idata = Idata + 1;
            CD = AI(Iai).WCS.CD;
            [ScRot] = imUtil.astrometry.cdmatrix2rotScale(CD(1,1), CD(1,2), CD(2,1), CD(2,2));
                        
            Data(Idata:Idata+1) = [ScRot.Scale .* ARCSEC_DEG, ScRot.PA_deg];
        end

        Cell = [Cols(:), num2cell(Data)];
        AI(Iai).HeaderData.insertKey(Cell, 'end');

    end



end
