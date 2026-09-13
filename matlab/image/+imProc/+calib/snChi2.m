function [Obj, Result] = snChi2(Obj, Args)
    % Estimate the PSF-fit systematic chi2 floor (SNCHI2) and write it to the header.
    %       For bright sources the PSF-fit chi2/dof grows above 1 because a
    %       fractional PSF-shape mismatch eps leaves flux-proportional
    %       residuals that the noise model does not contain (issue #1271):
    %           E[chi2/dof] ~= 1 + (eps*SN)^2/dof = 1 + (SNCHI2/MAGERR)^2 ,
    %       where SNCHI2 = 1.086*eps/sqrt(dof) is a single per-image scalar
    %       in magnitude-error units (MAGERR = 1.086/SN is the relative flux
    %       error, see the FLUXERR convention).
    %       This function estimates SNCHI2 from the catalog itself: for each
    %       source in the systematics-dominated regime it forms
    %           K = sqrt(chi2dof - 1) * MAGERR
    %       and takes the median over the selected sources. The estimate is
    %       self-contained (instrumental MAGERR suffices; no photometric
    %       calibration product is required) and dof never needs to be known
    %       explicitly - it is folded into the reported value.
    %       A catalog user recovers the corrected statistic as
    %           chi2dof_corr = chi2dof / (1 + (SNCHI2/MAGERR)^2) .
    %       Note SNCHI2 is valid for the catalog's own PSF-fit FitRadius;
    %       a fit with dof' pixels requires rescaling by sqrt(dof/dof').
    % Input  : - An AstroImage object (array supported). The catalog must
    %            contain the chi2/dof and magnitude-error columns.
    %          * ...,key,val,...
    %            'Chi2Col' - Column name of the PSF-fit chi2 per dof.
    %                   Default is 'PSF_CHI2DOF'.
    %            'MagErrCol' - Column name of the PSF magnitude error.
    %                   If absent from the catalog, MAGERR is derived as
    %                   1.086*FluxErrCol (FLUXERR is dF/F).
    %                   Default is 'MAGERR_PSF'.
    %            'FluxErrCol' - Fallback relative flux error column, used
    %                   only when MagErrCol is not found.
    %                   Default is 'FLUXERR_PSF'.
    %            'MinChi2Dof' - Use only sources with chi2dof above this
    %                   value, so that the systematic term dominates the
    %                   ~sqrt(2/dof) statistical scatter of chi2dof around
    %                   1. Default is 2.
    %            'MaxMagErr' - Use only sources with MAGERR below this
    %                   value [mag] (bright end). Default is 0.05.
    %            'MinNsrc' - Minimum number of selected sources for a valid
    %                   estimate; below it SNCHI2 is NaN (written as a
    %                   blank keyword value). Default is 10.
    %            'UpdateHeader' - Write the SNCHI2 keyword to the header.
    %                   Default is true.
    %            'KeyName' - Header keyword name. Default is 'SNCHI2'.
    %            'Verbose' - Default is false.
    % Output : - The input AstroImage with the SNCHI2 header keyword
    %            populated (if UpdateHeader is true).
    %          - A structure array (element per image) with fields:
    %            .SNCHI2 - The systematic floor [mag]; NaN if not estimable.
    %            .Nsrc   - Number of sources used.
    %            .MedChi2Dof - Median chi2dof of the sources used.
    % Author : Dana Kovaleva (Sep 2026), issue #1271
    % Example: AI = imProc.calib.snChi2(AI);
    %          [AI, Res] = imProc.calib.snChi2(AI, 'MinChi2Dof',1.5);

    arguments
        Obj AstroImage
        Args.Chi2Col        = 'PSF_CHI2DOF';
        Args.MagErrCol      = 'MAGERR_PSF';
        Args.FluxErrCol     = 'FLUXERR_PSF';
        Args.MinChi2Dof     = 2;
        Args.MaxMagErr      = 0.05;
        Args.MinNsrc        = 10;
        Args.UpdateHeader logical = true;
        Args.KeyName        = 'SNCHI2';
        Args.Verbose logical = false;
    end

    Nobj   = numel(Obj);
    Result = struct('SNCHI2',cell(size(Obj)), 'Nsrc',cell(size(Obj)), 'MedChi2Dof',cell(size(Obj)));

    for Iobj = 1:Nobj
        SnChi2     = NaN;
        Nsrc       = 0;
        MedChi2Dof = NaN;

        Cat = Obj(Iobj).CatData;
        if ~isemptyCatalog(Cat)
            Chi2Dof = getColOrNaN(Cat, Args.Chi2Col);
            MagErr  = getColOrNaN(Cat, Args.MagErrCol);
            if all(isnan(MagErr))
                FluxErr = getColOrNaN(Cat, Args.FluxErrCol);
                MagErr  = 1.086.*FluxErr;
            end

            Sel = isfinite(Chi2Dof) & isfinite(MagErr) & ...
                  Chi2Dof > Args.MinChi2Dof & ...
                  MagErr > 0 & MagErr < Args.MaxMagErr;
            Nsrc = sum(Sel);

            if Nsrc >= Args.MinNsrc
                K          = sqrt(Chi2Dof(Sel) - 1) .* MagErr(Sel);
                SnChi2     = median(K, 'omitnan');
                MedChi2Dof = median(Chi2Dof(Sel), 'omitnan');
            end
        end

        if Args.Verbose
            fprintf('imProc.calib.snChi2: image %d: %s = %.4f mag (Nsrc=%d)\n', ...
                    Iobj, Args.KeyName, SnChi2, Nsrc);
        end

        if Args.UpdateHeader
            % NaN is written as a blank keyword value (issue #1252 convention)
            Obj(Iobj).HeaderData.replaceVal(Args.KeyName, SnChi2, ...
                'Comment',{'PSF chi2 sys floor [mag]; E[chi2dof]=1+(SNCHI2/MAGERR)^2'});
        end

        Result(Iobj).SNCHI2     = SnChi2;
        Result(Iobj).Nsrc       = Nsrc;
        Result(Iobj).MedChi2Dof = MedChi2Dof;
    end
end

function Col = getColOrNaN(Cat, ColName)
    % Return the named column, or NaN vector if the column is absent.
    ColInd = colnameDict2ind(Cat, ColName);
    if isempty(ColInd) || any(isnan(ColInd))
        Col = nan(sizeCatalog(Cat), 1);
    else
        Col = getCol(Cat, ColInd);
    end
end
