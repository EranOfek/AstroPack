function Result = applyColorTerm(Obj, Args)
    % Apply the per-star colour correction to calibrated magnitudes (issue #1287)
    %
    %   The photometric zero point is built from ONE reference spectrum,
    %   F_nu = (lambda/pivot)^alpha with alpha = PT_REFSL (default 1.5), applied
    %   to every star. A star whose spectral slope differs from that value is
    %   therefore mis-calibrated, by an amount that also grows with airmass
    %   (the atmosphere's wavelength dependence amplifies it). This function
    %   removes that error star by star, using
    %     (a) the per-image sensitivity coefficients measured at calibration time
    %         and stored in the header (PT_CTA, PT_CTA2, PT_REFSL, PT_REFC), and
    %     (b) a fixed global relation alpha(BP_RP) derived from Gaia XP spectra,
    %   so it needs only a catalog and a header - no re-fitting, and it can be
    %   applied to archived products.
    %
    %   The correction is
    %     DeltaMag = PT_CTA*(alpha - alpha0) + PT_CTA2*(alpha - alpha0)^2
    %   with alpha = polyval(AlphaPoly, BP_RP) and alpha0 = PT_REFSL, and is
    %   ADDED to the calibrated magnitude. It vanishes at the anchor colour
    %   PT_REFC (the colour whose slope equals alpha0), so a star there is
    %   unchanged and the mean calibration is preserved. The quadratic term
    %   matters: a linear-only model errs by ~35 mmag at BP_RP 0.8 and by
    %   hundreds of mmag at BP_RP > 2 (see evaluateColorTerm).
    %
    %   LUPTITUDE CAVEAT: the correction is applied as a magnitude offset, which
    %   assumes DeltaMag = DeltaZP. That is exact for classical magnitudes
    %   (PhotCalibTrans MagType='mag'), but only approximate for luptitudes
    %   (MagType='lup', the default), whose asinh softening damps the response to
    %   a zero-point shift as the flux approaches zero. Verified on a LAST coadd
    %   against the exact per-source integral (PhotCalibTrans/evaluateZP with
    %   RefSpecSlopePerSource): agreement is 0.14 mmag rstd / 3.4 mmag max for
    %   sources brighter than 16 mag, and 0.11 mmag rstd for S/N > 20, but
    %   degrades for noise-dominated sources (S/N < 20), where the photometry is
    %   not usable anyway. Use the exact route if sub-mmag accuracy is needed at
    %   the detection limit.
    %
    % Input  : - An AstroImage (uses its CatData and HeaderData) or an
    %            AstroCatalog (then 'Header' must be supplied).
    %          * ...,key,val,...
    %            'Header'   - AstroHeader to read the PT_* keywords from. Required
    %                         when the input is an AstroCatalog; ignored for an
    %                         AstroImage (its HeaderData is used). Default [].
    %            'ColorCol' - Catalog column holding the colour. Default 'BP_RP'
    %                         (attached by imProc.cat.addColor, issue #1289).
    %            'AlphaPoly'- Coefficients [c2 c1 c0] of alpha(BP_RP), highest
    %                         power first (as polyval). Default
    %                         [-0.0516 2.4450 -1.3658], from 58k Gaia XP spectra.
    %            'SigmaAlpha' - Intrinsic scatter of the alpha(BP_RP) relation,
    %                         used for the correction error. Default 0.09.
    %            'UseQuadratic' - Include the PT_CTA2 term. Default true. Set
    %                         false for the linear-only model.
    %            'ColorRange' - Colours are clamped to this range before the
    %                         relation is evaluated, so wild colours cannot
    %                         produce wild corrections. Default [0.3 3.5].
    %            'OutputMode' - 'delta' writes only the correction columns;
    %                         'apply' writes only the corrected magnitudes;
    %                         'both' (default) writes both.
    %            'MagColNames' - Magnitude columns to correct. Default {} means
    %                         all columns starting with 'MAG_' that are not
    %                         error columns. Ignored when OutputMode='delta'.
    %            'OutSuffix'- Suffix for corrected magnitude columns, e.g.
    %                         MAG_AB_APER_3 -> MAG_AB_APER_3_CT. Default '_CT'.
    %                         Set '' to overwrite the input columns in place.
    %            'DeltaColName' / 'DeltaErrColName' - Names of the correction and
    %                         its error columns. Defaults 'MAG_CT' / 'MAGERR_CT'.
    %            'CreateNewObj' - Operate on a copy. Default false.
    % Output : - The input object with the colour-correction columns inserted
    %            (and/or corrected magnitude columns). Sources without a colour
    %            get DeltaMag = NaN and their magnitudes are left unchanged.
    % Author : Dana Kovaleva (Sep 2026)
    % Example:
    %   AI = imProc.cat.addColor(AI);                 % ensure BP_RP is present
    %   AI = imProc.calib.applyColorTerm(AI);         % delta + corrected mags
    %   Cat = imProc.calib.applyColorTerm(Cat, 'Header', H, 'OutputMode','delta');

    arguments
        Obj
        Args.Header                     = []
        Args.ColorCol char              = 'BP_RP'
        Args.AlphaPoly (1,3) double     = [-0.0516, 2.4450, -1.3658]
        Args.SigmaAlpha (1,1) double    = 0.09
        Args.UseQuadratic logical       = true
        Args.ColorRange (1,2) double    = [0.3, 3.5]
        Args.OutputMode char {mustBeMember(Args.OutputMode,{'delta','apply','both'})} = 'both'
        Args.QualityGate logical        = true   % reject images whose measured coefficients are inconsistent with the physical expectations below
        Args.CTA2Ref (1,1) double       = 0.0266 % band constant: PT_CTA2 = 0.543*Var[ln lambda] of the LAST band (measured 0.0266+-0.0002 on 3 telescopes)
        Args.CTA2Tol (1,1) double       = 0.004  % width channel: |PT_CTA2 - CTA2Ref| beyond this => transmission shape suspect
        Args.CTALawAB (1,2) double      = [0.020, 0.024]  % mean channel: expected PT_CTA = a + b*AIRMASS (5-field fit, Sep 2026)
        Args.CTATol (1,1) double        = 0.030  % allowed |PT_CTA - law|; catches mean-wavelength degeneracies (e.g. field 1680: PT_CTA 0.104 vs law ~0.05, PT_CTA2 normal)
        Args.AirmassKey char            = 'AIRMASS'  % header key for the airmass used by the mean channel; if missing, the mean channel is skipped
        Args.MagColNames                = {}
        Args.OutSuffix char             = '_CT'
        Args.DeltaColName char          = 'MAG_CT'
        Args.DeltaErrColName char       = 'MAGERR_CT'
        Args.CreateNewObj logical       = false
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    Nobj = numel(Result);
    for Iobj = 1:1:Nobj
        IsAstroImage = isa(Result(Iobj), 'AstroImage') || isa(Result(Iobj), 'AstroDiff') || isa(Result(Iobj), 'AstroZOGY');
        if IsAstroImage
            Cat    = Result(Iobj).CatData;
            Header = Result(Iobj).HeaderData;
        else
            Cat = Result(Iobj);
            if isempty(Args.Header)
                error('imProc:calib:applyColorTerm:NoHeader', ...
                    'A Header must be supplied when the input is an AstroCatalog.');
            end
            if numel(Args.Header) == 1
                Header = Args.Header;
            else
                Header = Args.Header(Iobj);
            end
        end

        if isempty(Cat) || isempty(Cat.Catalog) || size(Cat.Catalog,1) == 0
            continue;
        end

        % --- header coefficients ---
        CTA    = getHeaderVal(Header, 'PT_CTA');
        CTA2   = getHeaderVal(Header, 'PT_CTA2');
        Alpha0 = getHeaderVal(Header, 'PT_REFSL');
        RefCol = getHeaderVal(Header, 'PT_REFC');

        if ~isfinite(CTA)
            warning('imProc:calib:applyColorTerm:NoCoef', ...
                'PT_CTA missing or NaN - no colour correction applied.');
            continue;
        end
        if ~isfinite(Alpha0)
            % Fall back to the colour-anchored definition of alpha0.
            if isfinite(RefCol)
                Alpha0 = polyval(Args.AlphaPoly, RefCol);
            else
                warning('imProc:calib:applyColorTerm:NoAlpha0', ...
                    'Neither PT_REFSL nor PT_REFC available - no colour correction applied.');
                continue;
            end
        end
        if ~Args.UseQuadratic || ~isfinite(CTA2)
            CTA2 = 0;
        end

        % --- quality gate on the measured coefficients (issue #1287) ---
        % Both coefficients are moments of the throughput-weighted ln(lambda)
        % distribution, so each guards one failure mode of the transmission fit:
        %   width channel: PT_CTA2 is a band CONSTANT (0.543*Var[ln lambda]);
        %       a deviation means the fitted band has the wrong width.
        %   mean channel : PT_CTA follows a smooth airmass law; a deviation
        %       means the fitted band centre is displaced (this is the channel
        %       that catches degenerate fits like field 1680, whose PT_CTA2
        %       was normal while PT_CTA was twice the law).
        % A tripped gate skips the correction for this image (magnitudes are
        % left untouched), because a biased coefficient does more harm than an
        % uncorrected colour term.
        if Args.QualityGate
            GateMsg = '';
            if isfinite(CTA2) && CTA2 ~= 0 && abs(CTA2 - Args.CTA2Ref) > Args.CTA2Tol
                GateMsg = sprintf('PT_CTA2=%.4f vs band constant %.4f (tol %.4f)', ...
                    CTA2, Args.CTA2Ref, Args.CTA2Tol);
            end
            AMgate = getHeaderVal(Header, Args.AirmassKey);
            if isempty(GateMsg) && isfinite(AMgate)
                CTAExp = Args.CTALawAB(1) + Args.CTALawAB(2).*AMgate;
                if abs(CTA - CTAExp) > Args.CTATol
                    GateMsg = sprintf('PT_CTA=%.4f vs airmass law %.4f at AM=%.2f (tol %.4f)', ...
                        CTA, CTAExp, AMgate, Args.CTATol);
                end
            end
            if ~isempty(GateMsg)
                warning('imProc:calib:applyColorTerm:QualityGate', ...
                    'Coefficient quality gate tripped (%s) - no colour correction applied to this image.', GateMsg);
                continue;
            end
        end

        % Consistency check: the anchor colour should map to alpha0.
        if isfinite(RefCol)
            AlphaAtRef = polyval(Args.AlphaPoly, RefCol);
            if abs(AlphaAtRef - Alpha0) > 0.05
                warning('imProc:calib:applyColorTerm:AnchorMismatch', ...
                    ['Anchor colour PT_REFC=%.4f maps to alpha=%.4f but PT_REFSL=%.4f. ', ...
                     'The correction will not vanish at PT_REFC.'], RefCol, AlphaAtRef, Alpha0);
            end
        end

        % --- per-star correction ---
        if ~any(strcmp(Cat.ColNames, Args.ColorCol))
            warning('imProc:calib:applyColorTerm:NoColor', ...
                'Colour column ''%s'' not found - no colour correction applied. Run imProc.cat.addColor first.', ...
                Args.ColorCol);
            continue;
        end
        Color = Cat.getCol(Args.ColorCol);
        Color = Color(:);
        Known = isfinite(Color);

        ColorC = min(max(Color, Args.ColorRange(1)), Args.ColorRange(2));
        Alpha  = polyval(Args.AlphaPoly, ColorC);
        Dev    = Alpha - Alpha0;

        Delta    = CTA.*Dev + CTA2.*(Dev.^2);
        % d(Delta)/d(alpha) propagated through the relation's intrinsic scatter.
        DeltaErr = abs(CTA + 2.*CTA2.*Dev) .* Args.SigmaAlpha;

        Delta(~Known)    = NaN;
        DeltaErr(~Known) = NaN;

        if ismember(Args.OutputMode, {'delta','both'})
            Cat = replaceOrInsert(Cat, Delta,    Args.DeltaColName);
            Cat = replaceOrInsert(Cat, DeltaErr, Args.DeltaErrColName);
        end

        if ismember(Args.OutputMode, {'apply','both'})
            MagCols = Args.MagColNames;
            if isempty(MagCols)
                All     = Cat.ColNames;
                MagCols = All(startsWith(All, 'MAG_') & ~startsWith(All, 'MAGERR_'));
                % never correct the correction columns themselves
                MagCols = MagCols(~strcmp(MagCols, Args.DeltaColName));
            elseif ischar(MagCols)
                MagCols = {MagCols};
            end

            % Unknown colour -> leave the magnitude untouched.
            DeltaApply = Delta;
            DeltaApply(~Known) = 0;

            for Icol = 1:numel(MagCols)
                if ~any(strcmp(Cat.ColNames, MagCols{Icol}))
                    continue;
                end
                MagVal = Cat.getCol(MagCols{Icol});
                Cat    = replaceOrInsert(Cat, MagVal(:) + DeltaApply, [MagCols{Icol}, Args.OutSuffix]);
            end
        end

        if IsAstroImage
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
    end
end

function V = getHeaderVal(Header, Key)
    % Read a numeric header keyword, returning NaN when absent or unreadable.
    V = NaN;
    try
        if ~isempty(Header) && Header.isKeyExist(Key)
            V = Header.getVal(Key);
            if isempty(V) || ~isnumeric(V)
                V = NaN;
            end
        end
    catch
        V = NaN;
    end
    V = double(V(1));
end

function Cat = replaceOrInsert(Cat, Data, ColName)
    % Insert a column, or overwrite it if it already exists, so a re-run does
    % not create duplicates.
    if any(strcmp(Cat.ColNames, ColName))
        Cat = Cat.replaceCol(Data(:), ColName);
    else
        Cat = Cat.insertCol(Data(:), Inf, {ColName});
    end
end
