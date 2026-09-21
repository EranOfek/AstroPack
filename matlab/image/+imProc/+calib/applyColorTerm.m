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
    %   ADDED to the calibrated magnitude. It vanishes at the anchor colour,
    %   by default the header's PT_REFC (1.0 unless the image was calibrated
    %   with fitPhotCalibTrans('RefColorPerImage',true), which puts that
    %   image's own median colour there) - see 'RefColorSource'. Only the
    %   star-to-star colour differential is physical: the anchor merely
    %   declares which colour is left uncorrected, and shifts every star of
    %   the image by the same amount. The quadratic term
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
        Args.CoefSource char {mustBeMember(Args.CoefSource,{'model','header'})} = 'header'
                                                 % 'header' (default): the per-image measured PT_CTA/PT_CTA2, screened
                                                 %   by the quality gate. Preferred: the per-image value tracks real
                                                 %   night-to-night atmospheric variation that a single global law
                                                 %   cannot follow (measured on 1677.c: 5.8 vs 11.3 mmag red-star LC
                                                 %   scatter).
                                                 % 'model': deterministic coefficients - PT_CTA from the
                                                 %   airmass law CTALawAB at the header AIRMASS, PT_CTA2 = the band
                                                 %   constant CTA2Ref. The per-image header values are treated as
                                                 %   diagnostics only (they carry transmission-fit noise; see the
                                                 %   quality gate). Falls back to 'header' when AIRMASS is missing.
        Args.RefColorSource char {mustBeMember(Args.RefColorSource,{'image','header'})} = 'header'
                                                 % where the anchor colour (the colour at which the correction
                                                 %   vanishes) comes from.
                                                 % 'header' (default): PT_REFC as written by the calibration -
                                                 %   1.0 by default, or this image's own median colour when it was
                                                 %   calibrated with fitPhotCalibTrans('RefColorPerImage',true).
                                                 %   Preferred because the anchor that was used is then recorded in
                                                 %   the product and the choice stays reversible.
                                                 % 'image': recompute the median colour of THIS image's bright,
                                                 %   colour-known stars here, at apply time, ignoring PT_REFC. The
                                                 %   correction is then mean-free over the image's stars and carries
                                                 %   no epoch-common component - but the anchor used is written back
                                                 %   to the header only for AstroImage input (see UpdateHeaderAnchor);
                                                 %   for a bare AstroCatalog it is not recorded anywhere.
                                                 % An anchor far from a field's median colour costs |g(anchor) -
                                                 %   g(median)| * sigma(PT_CTA) of epoch-common wobble: measured on
                                                 %   field 1677 (58 visits), the bright floor of MAG_APER_3 went
                                                 %   3.7 -> 6.0 mmag with the anchor 0.29 mag away, 3.7 -> 4.1 with
                                                 %   it 0.09 mag away, and 3.7 -> 3.7 with the per-image median.
                                                 % The anchor is a convention (the constant of integration of a
                                                 %   differential correction), not a physical constant: any choice
                                                 %   is admissible provided it is recorded, and converting between
                                                 %   anchors is exact arithmetic given the stored coefficients.
        Args.RefColorMagCol char        = ''     % magnitude column for the 'image' anchor brightness cut; '' -> 'MAG_APER_3'
                                                 %   if present, else the first MAG_* column, else no brightness cut
        Args.RefColorMagMax (1,1) double = 16    % 'image' anchor ensemble = colour-known stars brighter than this.
                                                 %   A brightness cut is REQUIRED: over the full detection list the
                                                 %   median colour wanders by up to 0.3 mag between visits (detection
                                                 %   depth varies), which would itself inject 1-6 mmag per epoch.
        Args.RefColorMinN (1,1) double  = 20     % minimum ensemble size; below it, fall back to the header anchor
        Args.UpdateHeaderAnchor logical = true   % with RefColorSource='image' and AstroImage input, write the anchor
                                                 %   actually used back to PT_REFC, so the magnitudes can be put back
                                                 %   on any other anchor afterwards. Without this the choice is lost.
        Args.DeMean logical             = false  % SUPERSEDED by RefColorSource='image', which achieves the same
                                                 %   zero-mean property through the anchor; kept as an exact escape
                                                 %   hatch (it also zeroes the quadratic term's ensemble mean).
                                                 %   Subtracts the per-image ensemble median of the correction, making it
                                                 %   zero-mean over this image's (bright, colour-known) stars. Use for
                                                 %   RELATIVE (light-curve) photometry: kills the per-epoch common
                                                 %   shift by construction. Leave false for ABSOLUTE photometry, where
                                                 %   the ensemble-mean correction is a real error to be applied.
                                                 %   Under DeMean, colourless stars receive zero correction, which now
                                                 %   means "assume the ensemble-typical colour" - the best default.
        Args.DeMeanMagCol char          = ''     % magnitude column for the DeMean brightness cut; '' -> 'MAG_APER_3'
                                                 %   if present, else the first MAG_* column, else no brightness cut
        Args.DeMeanMagMax (1,1) double  = 16     % DeMean ensemble = colour-known stars brighter than this
        Args.QualityGate logical        = true   % ('header' mode only) reject images whose measured coefficients are inconsistent with the physical expectations below
        Args.CTA2Ref (1,1) double       = 0.0266 % band constant: PT_CTA2 = 0.543*Var[ln lambda] of the LAST band (measured 0.0266+-0.0002 on 3 telescopes)
        Args.CTA2Tol (1,1) double       = 0.004  % width channel: |PT_CTA2 - CTA2Ref| beyond this => transmission shape suspect
        Args.CTALawAB (1,2) double      = [0.020, 0.024]  % mean channel: expected PT_CTA = a + b*AIRMASS (5-field fit, Sep 2026)
        Args.CTATol (1,1) double        = 0.030  % allowed |PT_CTA - law|; catches mean-wavelength degeneracies (e.g. field 1680: PT_CTA 0.104 vs law ~0.05, PT_CTA2 normal)
        Args.AirmassKey char            = 'AIRMASS'  % header key for the airmass used by the mean channel; if missing, the mean channel is skipped
        Args.MagColNames                = {}
        Args.OutSuffix char             = '_CT'
        Args.ApplyAperColorTerm logical = true   % also apply the aperture-correction colour term of each column (APCC_<tag> in the header, issue #1270), on top of the zero-point term above. The two are independent: the zero-point term is common to every magnitude and cancels in MAG_PSF - MAG_APER_3, which is exactly what APCC_ measures.
        Args.Verbose logical            = false  % warn when a requested correction could not be applied
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

        % --- deterministic ('model') coefficients (default) ---
        % The physical content of PT_CTA is a smooth airmass law and PT_CTA2 is
        % a band constant; the per-image measured values add only transmission-
        % fit noise (2-8 mmag per epoch into bright-star light curves) and, on
        % degenerate fits, bias. In 'model' mode the coefficients are computed
        % from those laws and the header values serve as diagnostics only.
        UseModelCoef = strcmp(Args.CoefSource, 'model');
        if UseModelCoef
            AMmodel = getHeaderVal(Header, Args.AirmassKey);
            if isfinite(AMmodel)
                CTA  = Args.CTALawAB(1) + Args.CTALawAB(2).*AMmodel;
                CTA2 = Args.CTA2Ref;
                if ~isfinite(Alpha0)
                    Alpha0 = 1.5;
                end
            else
                % Without an airmass neither the model law nor the gate's mean
                % channel can run, so a biased header coefficient could slip
                % through unchecked. Be conservative: skip the correction.
                % (Set CoefSource='header' explicitly to force header values.)
                warning('imProc:calib:applyColorTerm:ModelNoAirmass', ...
                    'CoefSource=''model'' but header key ''%s'' is missing - no colour correction applied to this image.', ...
                    Args.AirmassKey);
                continue;
            end
        end

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
        if Args.QualityGate && ~UseModelCoef
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

        % Anchor of the correction: the colour at which it vanishes. With
        % RefColorSource='image' this is the median colour of the image's
        % bright, colour-known stars, so the correction has ~zero mean over
        % the field and cannot introduce an epoch-common shift; otherwise it
        % is the header value PT_REFC. Dev0 is the anchor's slope offset.
        AnchorCol = RefCol;
        if strcmp(Args.RefColorSource, 'image')
            SelAnc = Known;
            MagColAnc = Args.RefColorMagCol;
            if isempty(MagColAnc)
                if any(strcmp(Cat.ColNames, 'MAG_APER_3'))
                    MagColAnc = 'MAG_APER_3';
                else
                    CandAnc = Cat.ColNames(startsWith(Cat.ColNames,'MAG_') & ~startsWith(Cat.ColNames,'MAGERR_'));
                    CandAnc = CandAnc(~strcmp(CandAnc, Args.DeltaColName));
                    if ~isempty(CandAnc); MagColAnc = CandAnc{1}; end
                end
            end
            if ~isempty(MagColAnc) && any(strcmp(Cat.ColNames, MagColAnc))
                MagAnc = Cat.getCol(MagColAnc);
                SelAnc = SelAnc & MagAnc(:) < Args.RefColorMagMax;
            end
            if sum(SelAnc) >= Args.RefColorMinN
                AnchorCol = median(Color(SelAnc), 'omitnan');
            else
                warning('imProc:calib:applyColorTerm:AnchorEnsemble', ...
                    'RefColorSource=''image'' but only %d ensemble stars (<%d) - falling back to the header anchor PT_REFC.', ...
                    sum(SelAnc), Args.RefColorMinN);
            end
        end
        if ~isfinite(AnchorCol)
            warning('imProc:calib:applyColorTerm:NoAnchor', ...
                'No usable anchor colour (PT_REFC missing and image anchor unavailable) - no colour correction applied.');
            continue;
        end
        % Record the anchor that was actually used. The correction shifts every
        % star of this image by -PT_CTA*g(anchor), so without the anchor the
        % magnitudes sit on a system that cannot be converted to any other.
        if Args.UpdateHeaderAnchor && strcmp(Args.RefColorSource, 'image') && IsAstroImage
            Result(Iobj).HeaderData.replaceVal('PT_REFC', AnchorCol, ...
                'Comment', {'Anchor colour BP_RP of the applied colour term'});
        end

        Dev0 = polyval(Args.AlphaPoly, min(max(AnchorCol, Args.ColorRange(1)), Args.ColorRange(2))) - Alpha0;

        ColorC = min(max(Color, Args.ColorRange(1)), Args.ColorRange(2));
        Alpha  = polyval(Args.AlphaPoly, ColorC);
        Dev    = Alpha - Alpha0;

        % DeltaMag(alpha) in whichever form the header carries it. The
        % tabulated curve (PT_CA00.., written by StoreMode='table') wins when
        % present; otherwise the polynomial, to whatever order is available -
        % PT_CTA3/PT_CTA4 extend it over the full alpha range, and a header
        % written before them falls back to the quadratic unchanged.
        [Tab, GridDev] = headerColorTable(Header, Alpha0);
        if ~isempty(Tab)
            DeltaOf = @(D) interp1(GridDev, Tab, D, 'pchip', NaN);
            % local slope for the error, by finite difference on the curve
            Hd      = 0.05;
            SlopeOf = @(D) (DeltaOf(D + Hd) - DeltaOf(D - Hd)) ./ (2.*Hd);
        else
            CTA3 = getHeaderVal(Header, 'PT_CTA3');  if ~isfinite(CTA3); CTA3 = 0; end
            CTA4 = getHeaderVal(Header, 'PT_CTA4');  if ~isfinite(CTA4); CTA4 = 0; end
            DeltaOf = @(D) CTA.*D + CTA2.*D.^2 + CTA3.*D.^3 + CTA4.*D.^4;
            SlopeOf = @(D) CTA + 2.*CTA2.*D + 3.*CTA3.*D.^2 + 4.*CTA4.*D.^3;
        end

        Delta    = DeltaOf(Dev) - DeltaOf(Dev0);
        % d(Delta)/d(alpha) propagated through the relation's intrinsic scatter.
        DeltaErr = abs(SlopeOf(Dev)) .* Args.SigmaAlpha;

        Delta(~Known)    = NaN;
        DeltaErr(~Known) = NaN;

        % --- per-image ensemble de-mean (relative-photometry mode) ---
        % Subtracting the ensemble median re-anchors the correction at this
        % image's own stellar locus, so its per-image mean is zero by
        % construction and no epoch-to-epoch common shift can be injected.
        % Equivalent to an adaptive anchor colour; exact including the
        % quadratic term. The subtracted constant is a pure zero-point
        % convention, so DeltaErr is unchanged.
        if Args.DeMean
            SelDM = Known;
            MagColDM = Args.DeMeanMagCol;
            if isempty(MagColDM)
                if any(strcmp(Cat.ColNames, 'MAG_APER_3'))
                    MagColDM = 'MAG_APER_3';
                else
                    CandDM = Cat.ColNames(startsWith(Cat.ColNames,'MAG_') & ~startsWith(Cat.ColNames,'MAGERR_'));
                    CandDM = CandDM(~strcmp(CandDM, Args.DeltaColName));
                    if ~isempty(CandDM); MagColDM = CandDM{1}; end
                end
            end
            if ~isempty(MagColDM) && any(strcmp(Cat.ColNames, MagColDM))
                MagDM = Cat.getCol(MagColDM);
                SelDM = SelDM & MagDM(:) < Args.DeMeanMagMax;
            end
            if sum(SelDM) >= 10
                Delta(Known) = Delta(Known) - median(Delta(SelDM), 'omitnan');
            else
                warning('imProc:calib:applyColorTerm:DeMeanEnsemble', ...
                    'DeMean requested but only %d ensemble stars available (<10) - correction left un-demeaned.', sum(SelDM));
            end
        end

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
            AppliedAper = {};

            % Anchor of the aperture-correction colour terms. Written next to
            % them by aperCorrToHeader; PT_REFC is the fallback for headers
            % predating that keyword, where the two anchors always coincided.
            AperRef = getHeaderVal(Header, 'APCC_REF');
            if ~isfinite(AperRef); AperRef = RefCol; end

            for Icol = 1:numel(MagCols)
                if ~any(strcmp(Cat.ColNames, MagCols{Icol}))
                    continue;
                end
                MagVal = Cat.getCol(MagCols{Icol});
                Shift  = DeltaApply;

                % Aperture-correction colour term of THIS column (issue #1270):
                % the chromaticity of MAG_<col> - MAG_APER_3, fitted at
                % calibration time and published as APCC_<tag>. It is a
                % different effect from the zero-point term above - that one is
                % common to every magnitude and cancels in their difference -
                % so the two add.
                if Args.ApplyAperColorTerm
                    [A5, Tag] = aperColorCoef(Header, MagCols{Icol});
                    if isfinite(A5)
                        dCol = A5 .* (ColorC(:) - AperRef);
                        dCol(~Known) = 0;
                        Shift = Shift + dCol;
                        AppliedAper{end+1} = Tag; %#ok<AGROW>
                    end
                end

                Cat = replaceOrInsert(Cat, MagVal(:) + Shift, [MagCols{Icol}, Args.OutSuffix]);
            end
            if Args.ApplyAperColorTerm && isempty(AppliedAper) && Args.Verbose
                warning('imProc:calib:applyColorTerm:NoAperColorTerm', ...
                    'ApplyAperColorTerm requested but no APCC_ keyword matched the requested magnitude columns.');
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

function [A5, Tag] = aperColorCoef(Header, MagColName)
    % Aperture-correction colour slope of one magnitude column, from APCC_<tag>.
    %   The tag mapping is PhotCalibTrans.fluxCol2AperCorrKeys, so it matches
    %   whatever aperCorrToHeader wrote. NaN when the column has no term -
    %   which is the normal case for the reference aperture (MAG_APER_3), whose
    %   correction is zero by definition.
    A5  = NaN;
    Tag = '';
    try
        Keys = PhotCalibTrans.fluxCol2AperCorrKeys(MagColName);
        A5   = getHeaderVal(Header, Keys.Ccol);
        Tag  = Keys.Ccol;
    catch
        % unrecognised column name - no colour term for it
    end
end

function [Tab, GridDev] = headerColorTable(Header, Alpha0)
    % Tabulated DeltaMag(alpha) from PT_CAnn, as offsets from Alpha0.
    %   Empty when the header does not carry a table, which is the normal case
    %   for the default StoreMode='coef'.
    Tab = [];  GridDev = [];
    N = getHeaderVal(Header, 'PT_CAN');
    if ~isfinite(N) || N < 2
        return;
    end
    A0 = getHeaderVal(Header, 'PT_CAA0');
    dA = getHeaderVal(Header, 'PT_CADA');
    if ~isfinite(A0) || ~isfinite(dA) || dA == 0
        return;
    end
    V = nan(1, N);
    for I = 1:N
        V(I) = getHeaderVal(Header, sprintf('PT_CA%02d', I-1));
    end
    if all(isfinite(V))
        Tab     = V;
        GridDev = A0 + (0:(N-1)).*dA - Alpha0;
    end
end
