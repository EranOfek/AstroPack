function [Template, Info] = smearTemplate(Obj, Args)
    % Registration-smeared bad-pixel kernel, measured from defects or derived
    % from the coaddition shift track.
    %   Detector-fixed defects are shifted by the per-epoch registration before
    %   coaddition, so in a coadd they appear as a small blob rather than a
    %   single pixel. A defect that holds a constant value while only its
    %   position moves produces the same shape for every such pixel in one
    %   coadd, set by the per-epoch shifts.
    %
    %   That shape can be obtained two ways. 'measured' stacks the defects
    %   themselves out of the difference image. 'derived' pushes one delta
    %   pixel through the same registration and stacking the coaddition ran
    %   and convolves with P_deltaN, so it needs no defects at all, only the
    %   shift track. The derived method is due to @agioffe, see
    %   EranOfek/AstroPack#1192.
    %
    % Input  : - A single element AstroZOGY, or AstroDiff, in which the
    %            difference Image, Dbs, PSFData, WCS and New are populated.
    %            'derived' additionally needs P_deltaNhat, so subtractionD
    %            must have run.
    %          * ...,key,val,...
    %            'Method' - 'measured', 'derived', or 'auto'.
    %                   'auto' derives when the shift track is reachable and
    %                   otherwise stacks DarkHighVal defects. The visit
    %                   directory and crop are taken from the arguments when
    %                   given and from the object otherwise, so 'auto'
    %                   usually needs nothing passed. The derived path cannot
    %                   fail for lack of defects, so it is preferred whenever
    %                   it can run. Info.Method reports which was used.
    %                   Default is 'auto'.
    %
    %            --- both methods ---
    %            'HalfSize' - Cutout half size. Default is 7.
    %            'MomRadiusFactor' - MomRadius for the offset measurement, in
    %                   units of FWHM. Matches imProc.sub.findTransients.
    %                   Default is 1.7.
    %
    %            --- measured only ---
    %            'Bits' - Cell of mask bit names marking the defects, read
    %                   from the New image mask.
    %                   Must be sign consistent and additive. 'HighRN' and
    %                   'FlatHighStd' were both tested and are much worse:
    %                   they contribute 10 to 30 times more components, so the
    %                   median stack becomes their population, and neither has
    %                   a fixed sign so the stack does not converge.
    %                   Default is {'DarkHighVal'}.
    %            'MaxArea' - Max mask-component area, in pixels. Default is 25.
    %            'MinFluxPerPix' - Reject cutouts whose peak is below this.
    %                   Default is 0.
    %            'MaxFluxPerPix' - Reject cutouts whose peak exceeds this.
    %                   Bright residuals ring and distort the template.
    %                   Default is 70.
    %            'MaxNumDefects' - Cap on the number of cutouts stacked.
    %                   Default is 3000.
    %            'MinNumDefects' - Below this, return an empty template.
    %                   Default is 50.
    %            'StarCatName' - catsHTM catalogue used to reject calibrators
    %                   sitting on a star. Default is 'GAIADR3'.
    %            'MinStarDistFWHM' - Reject a calibrator closer than this many
    %                   FWHM to a catalogue star. Zero disables the cut.
    %                   Default is 2.5.
    %            'SrcXY' - Two column [X,Y] of source positions in image
    %                   pixels. When given, the catalogue query is skipped and
    %                   these are used instead. Default is [].
    %
    %            --- derived only ---
    %            'ShiftXY' - Two column per epoch shift track. When empty it
    %                   is recovered from VisitDir and CropID. Default is [].
    %            'VisitDir','CropID' - Where to recover the track from, via
    %                   the archived MergedMat and lcUtil.positionDrift. Both
    %                   default to empty and are then resolved from the
    %                   object: the directory from
    %                   Obj.New.ImageData.FileName, the crop from the CROPID
    %                   header keyword.
    %            'CoaddMode' - 'wrobust' reproduces the coadd rejection,
    %                   which is a first order part of the shape and takes the
    %                   agreement with a measured template from about 0.87 to
    %                   about 0.98. 'mean' is the linear faint limit.
    %                   Default is 'wrobust'.
    %            'MaxSearchShift' - Extra stamp padding. Default is 8.
    %            'ColX','ColY','ColSN','MinSN' - Columns and cut used by
    %                   lcUtil.positionDrift on the archived MergedMat. The
    %                   archive carries X1/Y1 rather than the X/Y production
    %                   used. Measured effect on the track is 0.012 to 0.023
    %                   pix, with the derived kernel unchanged to four
    %                   decimals.
    %
    %            The derived method cannot fail for lack of defects, which is
    %            its main advantage, so MinNumDefects and the star cut do not
    %            apply to it.
    % Output : - The normalized template, or [] if it could not be obtained.
    %          - A struct with Method, Core, Offset and Reason, plus
    %            NumComp, NumUsed, Scatter, NumNearSrc, X and Y for
    %            'measured', and Nepoch, SpanX and SpanY for 'derived'.
    %            Reason is '' on success and says why otherwise.
    % Author : Ruslan Konno + Claude (Aug 2026), derived method after @agioffe
    % Example: [T,I] = imUtil.properSub.smearTemplate(AD);
    %          [T,I] = imUtil.properSub.smearTemplate(AD, 'Method','derived', ...
    %                        'VisitDir',VisitDir, 'CropID',10);

    arguments
        Obj(1,1)
        Args.Method               = 'auto';
        Args.HalfSize             = 7;
        Args.MomRadiusFactor      = 1.7;

        Args.Bits cell            = {'DarkHighVal'};
        Args.MaxArea              = 25;
        Args.MinFluxPerPix        = 0;
        Args.MaxFluxPerPix        = 70;
        Args.MaxNumDefects        = 3000;
        Args.MinNumDefects        = 50;
        Args.StarCatName          = 'GAIADR3';
        Args.MinStarDistFWHM      = 2.5;
        Args.SrcXY                = [];

        Args.ShiftXY              = [];
        Args.VisitDir             = '';
        Args.CropID               = [];
        Args.CoaddMode            = 'wrobust';
        Args.MaxSearchShift       = 8;
        Args.ColX                 = 'X1';
        Args.ColY                 = 'Y1';
        Args.ColSN                = 'SN_3';
        Args.MinSN                = 8;
    end

    Template = [];
    Info     = struct('Method','', 'NumComp',0, 'NumUsed',0, ...
                      'Scatter',NaN, 'Core',NaN, 'Offset',[NaN NaN], ...
                      'NumNearSrc',0, 'X',[], 'Y',[], ...
                      'Nepoch',NaN, 'SpanX',NaN, 'SpanY',NaN, 'Reason','');

    % Fill in the visit directory and crop from the object where they were
    % not given, so the derived path is usable without plumbing them through
    % subtractionS.
    [Args.VisitDir, Args.CropID] = resolveVisit(Obj, Args);

    % Resolve 'auto'. Deriving needs only the shift track, and unlike the
    % measured path it cannot fail for lack of defects, so it is used
    % whenever the track is reachable.
    Method = lower(Args.Method);
    if strcmp(Method, 'auto')
        if ~isempty(Args.ShiftXY) || ...
                (~isempty(Args.VisitDir) && ~isempty(Args.CropID))
            Method = 'derived';
        else
            Method = 'measured';
        end
    end

    Info.Method = Method;

    if strcmp(Method, 'derived')
        [Template, Info] = derivedFromShifts(Obj, Args, Info);
        return
    end
    if ~strcmp(Method, 'measured')
        Info.Reason = sprintf('unknown Method option %s', Args.Method);
        return
    end

    Image    = Obj.Image;
    MaskData = Obj.New.MaskData;

    if isempty(Image) || isempty(MaskData) || MaskData.isemptyImage
        Info.Reason = 'no image or no mask';
        return
    end

    Fwhm     = Obj.PSFData.fwhm;
    SizeIm   = size(Image);
    HalfSize = Args.HalfSize;
    Cen      = HalfSize + 1;

    % --- defect positions ---
    DefectMask = MaskData.findBit(Args.Bits, 'Method','any', 'OutType','mat');
    if ~any(DefectMask(:))
        Info.Reason = sprintf('no %s pixels', strjoin(Args.Bits,'/'));
        return
    end

    CC    = bwconncomp(DefectMask, 8);
    Stats = regionprops(CC, 'Centroid', 'Area');
    Area  = [Stats.Area].';
    XY    = cat(1, Stats.Centroid);

    Info.NumComp = CC.NumObjects;

    Keep = (Area <= Args.MaxArea) ...
         & XY(:,1) > HalfSize+1 & XY(:,1) < SizeIm(2)-HalfSize-1 ...
         & XY(:,2) > HalfSize+1 & XY(:,2) < SizeIm(1)-HalfSize-1;

    XYuse = XY(Keep,:);
    Nuse  = size(XYuse,1);

    if Nuse > Args.MaxNumDefects
        % Deterministic subsample, so one coadd always gives one template.
        XYuse = XYuse(round(linspace(1, Nuse, Args.MaxNumDefects)), :);
    end
    if size(XYuse,1) < Args.MinNumDefects
        Info.Reason = sprintf('only %d usable components', size(XYuse,1));
        return
    end

    X0 = round(XYuse(:,1));
    Y0 = round(XYuse(:,2));

    % --- common offset, in the frame the catalogue uses ---
    %  regionprops gives the centroid of the smeared mask footprint, not of
    %  its flux, so the cut positions sit a pixel or two off the smear.
    %  Candidate positions are X1,Y1, which findTransients takes from
    %  imUtil.image.moment2 on Dbs, so the same moment is measured here and
    %  the median applied as one shift to every position. Every defect in a
    %  coadd shares the same shift history, so the offset is common mode.
    %  Aligning cutouts individually must not be done: the smear track has
    %  more than one lobe, and per-cutout alignment folds them together.
    M1 = imUtil.image.moment2(Obj.Dbs, X0, Y0, ...
                              'MomRadius', Args.MomRadiusFactor.*Fwhm);

    Offset = [median(M1.X - X0, 'omitnan'), median(M1.Y - Y0, 'omitnan')];
    Shift  = round(Offset);
    Info.Offset = Offset;

    X = X0 + Shift(1);
    Y = Y0 + Shift(2);

    InFrame = X > HalfSize+1 & X < SizeIm(2)-HalfSize-1 ...
            & Y > HalfSize+1 & Y < SizeIm(1)-HalfSize-1;
    X = X(InFrame);
    Y = Y(InFrame);

    if numel(X) < Args.MinNumDefects
        Info.Reason = sprintf('only %d positions left after the shift', numel(X));
        return
    end

    % --- reject calibrators sitting on a star ---
    %  A defect that coincides with a star gets the star's subtraction
    %  residual in its stamp, and that residual is PSF shaped. These are also
    %  the brightest members of the sample, so they set the bright end of the
    %  flux distribution. Measured on one crop, every one of the brightest 1%
    %  of stacked calibrators was within 5 pixels of a GAIA source, against a
    %  5.2% base rate, and the contaminating stars were G ~ 16.5 to 18.5,
    %  faint enough that MaxFluxPerPix does not exclude them.
    %
    %  The New and Ref source catalogues are not used for this. Ref.CatData
    %  is not in the image pixel frame, its nearest entry to a known star
    %  sits tens of pixels away, and New.CatData is too shallow to hold all
    %  the offenders.
    if Args.MinStarDistFWHM > 0
        SrcXY = Args.SrcXY;

        if isempty(SrcXY)
            SrcXY = getStarXY(Obj, SizeIm, Args.StarCatName);
        end

        if ~isempty(SrcXY)
            MinStarDistSq = (Args.MinStarDistFWHM .* Fwhm).^2;

            Isolated = true(numel(X),1);
            for Icand = 1:numel(X)
                Isolated(Icand) = ~any( (SrcXY(:,1)-X(Icand)).^2 + ...
                                        (SrcXY(:,2)-Y(Icand)).^2 < MinStarDistSq );
            end

            Info.NumNearSrc = sum(~Isolated);
            X = X(Isolated);
            Y = Y(Isolated);

            if numel(X) < Args.MinNumDefects
                Info.Reason = sprintf('only %d isolated calibrators', numel(X));
                return
            end
        end
    end

    % --- stack ---
    Cube = imUtil.cut.image2cutouts(Image, X, Y, HalfSize);
    Cube = Cube - median(median(Cube, 1, 'omitnan'), 2, 'omitnan');

    % Each cutout is normalized by its core before stacking, otherwise the
    % spread in amplitude swamps the shape.
    Norm   = squeeze(sum(sum(Cube(Cen-1:Cen+1, Cen-1:Cen+1, :), 1), 2));
    Peak   = squeeze(max(max(Cube, [], 1), [], 2));
    Finite = squeeze(all(all(isfinite(Cube), 1), 2));

    Good = isfinite(Norm) & Norm > 0 & Finite ...
         & Peak > Args.MinFluxPerPix & Peak < Args.MaxFluxPerPix;

    if sum(Good) < Args.MinNumDefects
        Info.Reason = sprintf('only %d usable cutouts', sum(Good));
        return
    end

    CubeN = Cube(:,:,Good) ./ reshape(Norm(Good), 1, 1, []);
    T     = median(CubeN, 3, 'omitnan');

    % Normalize on the core at the stamp centre, which is where this path
    % puts it: the moment offset above has already been applied to the cut
    % positions. The derived path locates the core instead, because its
    % anchor can be several pixels out while the anchoring is unresolved.
    [Template, Core] = normaliseCore(T, false);
    Info.Core = Core;

    if isempty(Template)
        % Not positive. Also the signature of an ill determined divisor:
        % with each cutout normalized to a core of 1, cancellation drives
        % the stacked core toward zero.
        Info.Reason = sprintf('template core is not positive, %.4g', Core);
        return
    end

    Info.NumUsed = sum(Good);

    Info.X = X(Good);
    Info.Y = Y(Good);

    Info.Scatter = median(reshape(std(CubeN, 0, 3, 'omitnan') ...
                                  ./ max(abs(Template(:))), [], 1));
end


% ======================================================================
% derived path
% ======================================================================

function [Template, Info] = derivedFromShifts(Obj, Args, Info)
    % Template from the coaddition shift track, no defects required.
    %   Registration is a pure translation, so a detector fixed defect lands
    %   at its detector pixel plus the per epoch shift in every frame.
    %   Pushing one delta pixel through the same imProc.transIm.register call
    %   and stacking it the way the coaddition stacks gives the New coadd
    %   response to one bad pixel. Convolving with P_deltaN maps that into D.

    Template = [];

    ShiftXY = Args.ShiftXY;
    Var     = [];

    if isempty(ShiftXY)
        if isempty(Args.VisitDir) || isempty(Args.CropID)
            Info.Reason = 'derived needs ShiftXY, or VisitDir and CropID';
            return
        end
        try
            [ShiftXY, Var] = shiftTrackFromVisit(Args);
        catch ME
            Info.Reason = sprintf('could not recover the shift track: %s', ME.message);
            return
        end
    end

    Nepoch = size(ShiftXY,1);
    if isempty(Var)
        Var = ones(Nepoch,1);
    end

    Info.Nepoch = Nepoch;
    Info.SpanX  = max(ShiftXY(:,1)) - min(ShiftXY(:,1));
    Info.SpanY  = max(ShiftXY(:,2)) - min(ShiftXY(:,2));

    PdN = deltaResponse(Obj);
    if isempty(PdN)
        Info.Reason = 'P_deltaNhat is empty, subtractionD did not populate it';
        return
    end

    % Room for the whole track, the interpolation support, the P_deltaN
    % footprint and the anchor offset.
    Pad   = ceil(max(Info.SpanX, Info.SpanY)) + size(PdN,1) + ...
            2.*Args.MaxSearchShift + 12;
    Stamp = 2.*Pad + 1;

    Kernel = smearKernel(Obj, ShiftXY, Var, Stamp, Args.CoaddMode);

    % Kernel is the defect as it appears in the New coadd. PdN maps it into
    % D. Despite the name, PdN is not the New image PSF, see deltaResponse.
    Big = conv2(Kernel, PdN, 'same');

    % What the catalogue would call the position of this response, so the cut
    % lands in the same frame the measured path cuts in.
    Offset      = momentOffset(Big, Args.MomRadiusFactor .* Obj.PSFData.fwhm);
    Info.Offset = Offset;

    [Template, Core] = normaliseCore(cropCentre(Big, Args.HalfSize, round(Offset)), true);
    Info.Core = Core;

    if isempty(Template)
        Info.Reason = sprintf('derived template core is not positive, %.4g', Core);
        return
    end
end


function [ShiftXY, Var] = shiftTrackFromVisit(Args)
    % Per epoch shifts from the archived MergedMat.

    MS = pipeline.last.load.loadMergedMat('MergedMatDir',Args.VisitDir, ...
                                          'CropsToAnalyze',Args.CropID, ...
                                          'Verbose',false);
    if numel(MS)~=1
        error('expected one MatchedSources for crop %d, got %d', Args.CropID, numel(MS));
    end

    [~, DriftInfo] = lcUtil.positionDrift(MS, 'ColX',Args.ColX, 'ColY',Args.ColY, ...
                                              'ColSN',Args.ColSN, 'MinSN',Args.MinSN);
    ShiftXY = DriftInfo.ShiftXY;

    % procCoadd passes ZP empty, so the flux match is unity and the weight is
    % 1/Var with a scalar variance per epoch.
    if isfield(MS.Data,'VAR_IM')
        Var = median(MS.Data.VAR_IM, 2, 'omitnan');
        Var = Var(:);
    else
        Var = ones(size(ShiftXY,1),1);
    end
end


function K = smearKernel(Obj, ShiftXY, Var, Stamp, Mode)
    % Coadd response to one detector fixed delta pixel.
    %   The interpolation and the rejection both come from the code the
    %   coaddition itself runs, so no convention has to be restated here.

    Nepoch = size(ShiftXY,1);
    Cen    = (Stamp+1)./2;

    Delta           = zeros(Stamp, Stamp, 'single');
    Delta(Cen, Cen) = 1;

    AI = AstroImage([Nepoch 1]);
    for Iep=1:1:Nepoch
        AI(Iep) = AstroImage({Delta});
    end

    Reg = imProc.transIm.register(AI, ShiftXY, 'WCS',Obj.New, 'DataProp',{'ImageData'});

    Cube = zeros(Stamp, Stamp, Nepoch, 'single');
    for Iep=1:1:Nepoch
        C = Reg(Iep).ImageData.Data;
        C(~isfinite(C)) = 0;      % outer border only, far from the response
        Cube(:,:,Iep) = C;
    end

    switch lower(Mode)
        case 'mean'
            W = (1./Var)./sum(1./Var);
            K = sum(Cube .* reshape(W(:),1,1,[]), 3);
        case 'wrobust'
            % The same rejection coadd_WRobust applies. Modelling it takes
            % the agreement with a measured template from about 0.87 to
            % about 0.98. On a noiseless cube the min/max rejection always
            % strips the two largest contributors, which is the extreme
            % rather than the typical bright case.
            VarS = cast(Var(:), 'like',Cube);
            OneS = ones(numel(Var), 1, 'like',Cube);
            K = imUtil.stack.wcoaddRobust(Cube, [], 'Var',VarS, 'F',OneS, ...
                                          'ZP',[], 'ZP0',[], 'RemoveMinMax',true, ...
                                          'Niter',1, 'SigmaClip',[2.5 2.5], 'StdMethod',2);
        otherwise
            error('unknown CoaddMode option %s', Mode);
    end

    K = double(K);
end


function PdN = deltaResponse(Obj)
    % The D image response to a single delta pixel in the New image.
    %
    %   This is NOT the New image PSF, which the name P_deltaN might suggest.
    %   imUtil.properSub.subtractionD builds it as
    %
    %       P_deltaNhat = Pr_hat .* F_num ./ D_denSqrt
    %
    %   so it is the *Ref* PSF divided by the noise weighted denominator. It
    %   is the transfer function that takes anything added to New into D:
    %   subtractionD forms D_hat from Fr.*Pr_hat.*N_hat./D_denSqrt, the same
    %   operator up to the scalar Fr against F_num, and the core
    %   normalisation of the template removes that scalar.
    %
    %   It is the right operator here, rather than Pd, because a bad pixel is
    %   not light. It never went through the optics, so it carries no PSF,
    %   and in New it is a bare delta rather than a Pn shaped source. Pd is
    %   the D response to a point source, which in New is already Pn, so
    %   using Pd would convolve the New PSF in a second time and give a
    %   template that is too broad.
    %
    %   The consistency check is Pn convolved with P_deltaN being
    %   proportional to Pd, which follows from the two expressions above.

    PdN = [];

    if isempty(Obj.P_deltaNhat)
        return
    end

    P = fftshift(real(ifft2(Obj.P_deltaNhat)));

    [NPy, NPx] = size(Obj.New.PSFData.getPSF);
    Half       = floor((max(NPy,NPx) - 1)./2);

    PdN = cropCentre(P, Half, [0 0]);
    PdN = PdN ./ sum(PdN(:));
end


% ======================================================================
% shared
% ======================================================================

function Out = cropCentre(Image, HalfSize, Offset)
    % Stamp of half size HalfSize about the centre, displaced by Offset [dx dy].

    Cen = (size(Image)+1)./2;
    Ry  = round(Cen(1)) + Offset(2) + (-HalfSize:1:HalfSize);
    Rx  = round(Cen(2)) + Offset(1) + (-HalfSize:1:HalfSize);
    Out = Image(Ry, Rx);
end


function Offset = momentOffset(Image, MomRadius)
    % Windowed first moment about the stamp centre, the same estimator
    % imProc.sub.findTransients uses to place X1 and Y1.

    Cen    = round((size(Image)+1)./2);
    M1     = imUtil.image.moment2(Image, Cen(2), Cen(1), 'MomRadius',MomRadius);
    Offset = [M1.X - Cen(2), M1.Y - Cen(1)];
end


function [T, Core] = normaliseCore(T, LocateCore)
    % Divide by the core, the sum of a central 3x3. Returns empty T when the
    % core is not positive, since dividing by it would flip the sign of the
    % template, and a sign flip survives the later rstd normalisation and
    % inverts the statistic: the better the match, the more negative the
    % response.
    %
    %   LocateCore false takes the core at the stamp centre, which is correct
    %   when the cut has already been placed on it. True finds it first, from
    %   the positive flux centroid, which the derived path needs because its
    %   anchor can sit several pixels off the flux while the anchoring is
    %   unresolved. Measured on one crop, the core at the anchor was 0.073
    %   against 0.536 at the centroid, so the result came out 7.4 times too
    %   large, and an anchor landing on a negative lobe would flip the sign.

    Sz = size(T);
    Cx = (Sz(2)+1)./2;
    Cy = (Sz(1)+1)./2;

    if LocateCore
        [Yg, Xg] = ndgrid(1:Sz(1), 1:Sz(2));
        W  = max(T, 0);
        Sw = sum(W(:));

        if isfinite(Sw) && Sw>0
            % Kept one pixel inside the border so the 3x3 fits.
            Cx = min(max(round(sum(W(:).*Xg(:))./Sw), 2), Sz(2)-1);
            Cy = min(max(round(sum(W(:).*Yg(:))./Sw), 2), Sz(1)-1);
        end
    end

    Core = sum(sum(T(Cy-1:Cy+1, Cx-1:Cx+1)));

    if ~isfinite(Core) || Core<=0
        T = [];
        return
    end
    T = T ./ Core;
end


function SrcXY = getStarXY(Obj, SizeIm, CatName)
    % Star positions over the subimage, in image pixels.
    %   A failed query is not fatal. Returning empty leaves the caller
    %   without the star cut rather than stopping the subtraction, which
    %   matters on a machine with no local catalogue or for a field the
    %   catalogue does not cover.

    SrcXY = [];

    try
        [RAcen, Deccen] = Obj.WCS.xy2sky(SizeIm(2)./2, SizeIm(1)./2);
        [RAcor, Deccor] = Obj.WCS.xy2sky(1, 1);

        % 20 per cent margin so the corners are covered
        Radius = 1.2 .* 3600 .* sqrt( ((RAcor-RAcen).*cosd(Deccen)).^2 + ...
                                      (Deccor-Deccen).^2 );

        [StarCat, StarCol] = catsHTM.cone_search(CatName, ...
            RAcen./180.*pi, Deccen./180.*pi, Radius);

        if isempty(StarCat)
            return
        end

        IcolRA  = find(strcmp(StarCol, 'RA'));
        IcolDec = find(strcmp(StarCol, 'Dec'));

        [SrcX, SrcY] = Obj.WCS.sky2xy(StarCat(:,IcolRA)./pi.*180, ...
                                      StarCat(:,IcolDec)./pi.*180);

        SrcXY = [SrcX(:), SrcY(:)];
        SrcXY = SrcXY(all(isfinite(SrcXY), 2), :);

    catch ME
        warning('smearTemplate:starCat', ...
                ['star catalogue query failed, template built without the ' ...
                 'star cut: %s'], ME.message);
    end
end

function [VisitDir, CropID] = resolveVisit(Obj, Args)
    % Visit directory and crop, from the arguments when given and from the
    % object otherwise.
    %
    %   The directory comes from the New image file name. AstroImage fills
    %   that in only when the object was constructed from a path, see
    %   AstroImage.m:439, and ImageComponent.FileName is marked FFU, so it
    %   can be empty or relative. The isfolder test is what makes this safe
    %   rather than the field being trusted.
    %
    %   The crop comes from the CROPID header keyword, which is reliable.
    %   The header stores it as a double, so it is rounded.

    VisitDir = '';
    CropID   = Args.CropID;

    if ischar(Args.VisitDir) && ~isempty(Args.VisitDir) && isfolder(Args.VisitDir)
        VisitDir = Args.VisitDir;
    else
        try
            FileName = Obj.New.ImageData.FileName;
            if ischar(FileName) || isstring(FileName)
                Candidate = fileparts(char(FileName));
                if ~isempty(Candidate) && isfolder(Candidate)
                    VisitDir = Candidate;
                end
            end
        catch
            % leave empty, the caller falls back to the measured path
        end
    end

    if isempty(CropID)
        try
            Val = Obj.New.HeaderData.getVal('CROPID');
            if isscalar(Val) && isfinite(Val)
                CropID = round(Val);
            end
        catch
            % leave empty
        end
    end
end