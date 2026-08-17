function [Template, Info] = smearTemplate(Obj, Args)
    % Measure the registration-smeared bad-pixel kernel from a difference image.
    %   Detector-fixed defects are shifted by the per-epoch registration before
    %   coaddition, so in a coadd they appear as a small blob rather than a
    %   single pixel. A defect that holds a constant value while only its
    %   position moves produces the same shape for every such pixel in one
    %   coadd, set by the per-epoch shifts, so the shape can be measured by
    %   stacking them.
    % Input  : - A single element AstroZOGY, or AstroDiff, in which the
    %            difference Image, Dbs, PSFData, WCS and New are populated.
    %          * ...,key,val,...
    %            'Bits' - Cell of mask bit names marking the defects, read
    %                   from the New image mask.
    %                   Must be sign consistent and additive. 'HighRN' and
    %                   'FlatHighStd' were both tested and are much worse:
    %                   they contribute 10 to 30 times more components, so the
    %                   median stack becomes their population, and neither has
    %                   a fixed sign so the stack does not converge.
    %                   Default is {'DarkHighVal'}.
    %            'HalfSize' - Cutout half size. Default is 7.
    %            'MaxArea' - Max mask-component area, in pixels. Default is 25.
    %            'MinFluxPerPix' - Reject cutouts whose peak is below this.
    %                   Default is 0.
    %            'MaxFluxPerPix' - Reject cutouts whose peak exceeds this.
    %                   Bright residuals ring and distort the template.
    %                   Default is 50.
    %            'MaxNumDefects' - Cap on the number of cutouts stacked.
    %                   Default is 3000.
    %            'MinNumDefects' - Below this, return an empty template.
    %                   Default is 50.
    %            'MomRadiusFactor' - MomRadius for the offset measurement, in
    %                   units of FWHM. Matches imProc.sub.findTransients.
    %                   Default is 1.7.
    %            'StarCatName' - catsHTM catalogue used to reject calibrators
    %                   sitting on a star. Default is 'GAIADR3'.
    %            'MinStarDistFWHM' - Reject a calibrator closer than this many
    %                   FWHM to a catalogue star. Zero disables the cut.
    %                   Default is 2.5.
    %            'SrcXY' - Two column [X,Y] of source positions in image
    %                   pixels. When given, the catalogue query is skipped and
    %                   these are used instead.
    %                   Default is [].
    % Output : - The normalized template, or [] if it could not be measured.
    %          - A struct with NumComp, NumUsed, Scatter, Core, Offset,
    %            NumNearSrc, X, Y and Reason. X and Y are the positions
    %            actually stacked. Reason is '' on success and says why
    %            otherwise.
    % Author : Ruslan Konno + Claude (Aug 2026)
    % Example: [T,I] = imUtil.properSub.smearTemplate(AD);

    arguments
        Obj(1,1)
        Args.Bits cell            = {'DarkHighVal'};
        Args.HalfSize             = 7;
        Args.MaxArea              = 25;
        Args.MinFluxPerPix        = 0;
        Args.MaxFluxPerPix        = 70;
        Args.MaxNumDefects        = 3000;
        Args.MinNumDefects        = 50;
        Args.MomRadiusFactor      = 1.7;
        Args.StarCatName          = 'GAIADR3';
        Args.MinStarDistFWHM      = 2.5;
        Args.SrcXY                = [];
    end

    Template = [];
    Info     = struct('NumComp',0, 'NumUsed',0, 'Scatter',NaN, ...
                      'Core',NaN, 'Offset',[NaN NaN], 'NumNearSrc',0, ...
                      'X',[], 'Y',[], 'Reason','');

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

    % Normalize on the core, not the total. The total can be negative when a
    % positive core sits in a negative bowl, and dividing by it would flip
    % the sign of the template. A sign flip survives the later rstd
    % normalization, since rstd is positive, and it inverts the statistic:
    % the better the match, the more negative the response.
    Core = sum(sum(T(Cen-1:Cen+1, Cen-1:Cen+1)));
    Info.Core = Core;

    if ~isfinite(Core) || Core <= 0
        % Also the signature of an incoherent population: with each cutout
        % normalized to a core of 1, a shared shape gives a stacked core
        % near 1, and cancellation drives it toward zero.
        Info.Reason = sprintf('template core is not positive, %.4g', Core);
        return
    end

    Template = T ./ Core;
    Info.NumUsed = sum(Good);

    Info.X = X(Good);
    Info.Y = Y(Good);

    Info.Scatter = median(reshape(std(CubeN, 0, 3, 'omitnan') ...
                                  ./ max(abs(Template(:))), [], 1));
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