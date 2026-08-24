function [BinCen, BinThr, Info] = smearThreshold(Obj, Args)
    % Calibrate the SCORE - SN_smear threshold for one difference image.
    %   Injects two populations of sources into a copy of the difference
    %   image, one shaped like the D-image PSF and one like the smear
    %   template, measures both statistics on each, and returns the contour
    %   that keeps a requested fraction of the PSF-shaped population.
    %
    %   Sources go into D rather than into New, so no subtraction has to be
    %   rerun. At this point the template already exists and both statistics
    %   are matched filters on D, so injecting there gives the same threshold
    %   for two filter calls instead of two subtractions.
    %
    %   Both populations use the same positions and the same fluxes, so every
    %   PSF-shaped injection has a smear-shaped counterpart on the same pixel
    %   with the same local noise and neighbours, and the branches differ only
    %   in shape.
    %
    %   This calibrates the threshold *given* the template. It does not test
    %   whether the template is right: the smear population is drawn from the
    %   template under test, so agreement with it is assumed, not measured.
    %
    % Input  : - A single element AstroZOGY, or AstroDiff, with Image, Fd,
    %            PSFData and SmearTemplate populated.
    %          * ...,key,val,...
    %            'Ninj' - Injections per population. Default is 1000.
    %            'FluxRng' - Log-uniform injected flux range. Wide by
    %                   default, since what matters is where the injections
    %                   land in SCORE and that depends on the coadd depth.
    %                   Default is [3e1 3e4].
    %            'MinSep' - Grid spacing, in pixels, so stamps do not
    %                   overlap. Default is 30.
    %            'BinEdges' - |SCORE| bins the threshold is fitted in. Starts
    %                   at 5 because imProc.sub.findTransients detects at
    %                   threshold 5, so nothing fainter reaches the
    %                   catalogue. The top bin is bounded rather than open
    %                   ended, since an open bin collects several hundred in
    %                   |SCORE| and its robust scale becomes meaningless.
    %                   Default is [5 7.5 10 14 20 30 45 70 120].
    %            'KeepFraction' - Fraction of PSF-shaped sources to keep.
    %                   Default is 0.99.
    %            'MinPerBin' - Injections needed to use a bin. Default is 20.
    %            'RadiusTS' - Peak search radius when sampling, matching
    %                   imProc.sub.measureTransients. Default is 1.
    %            'Seed' - rng seed, for a reproducible threshold. Empty
    %                   leaves the generator alone. Default is [].
    % Output : - Bin centres, the median |SCORE| in each used bin.
    %          - Bin thresholds, the contour keeping KeepFraction.
    %          - A struct with Ninj, NumPerBin, the fitted branch slopes, a
    %            Fun handle that evaluates the threshold at any |SCORE|, and
    %            Reason, which is '' on success.
    % Author : Ruslan Konno (Aug 2026)
    % Example: [BinCen, BinThr, Info] = imUtil.properSub.smearThreshold(AD);
    %          Thresh  = Info.Fun(abs(Score));
    %          IsSmear = (Score - SN_smear) < Thresh;

    arguments
        Obj(1,1)
        Args.Ninj              = 1000;
        Args.FluxRng           = [3e1 3e4];
        Args.MinSep            = 30;
        Args.BinEdges          = [5 7.5 10 14 20 30 45 70 120];
        Args.KeepFraction      = 0.99;
        Args.MinPerBin         = 20;
        Args.RadiusTS          = 1;
        Args.InjectSmear logical = false;
        Args.Seed              = [];
    end

    BinCen = [];
    BinThr = [];
    Info   = struct('Ninj',0, 'NumPerBin',[], 'PsfSlope',NaN, ...
                    'SmearSlope',NaN, 'Fun',[], 'Reason','');

    Template = Obj.SmearTemplate;
    if isempty(Template) || isempty(Obj.Image)
        Info.Reason = 'no smear template or no difference image';
        return
    end

    if ~isempty(Args.Seed)
        rng(Args.Seed);
    end

    % --- the two shapes ---
    Kpsf = Obj.PSFData.getPSF;
    Kpsf = Kpsf ./ sum(Kpsf(:));
    Ksmr = Template ./ sum(Template(:));

    Hp = (size(Kpsf,1)-1)./2;
    Hs = (size(Ksmr,1)-1)./2;
    Hm = max(Hp, Hs);

    SizeIm = size(Obj.Image);

    % --- positions ---
    %  A coarse grid so nothing overlaps, kept clear of NaN by the whole
    %  stamp rather than the central pixel: a stamp touching one has no
    %  usable statistic at its centre and would enter as a spurious low
    %  outlier in whichever branch it belongs to.
    [Gx, Gy] = meshgrid(Hm+Args.MinSep : Args.MinSep : SizeIm(2)-Hm-Args.MinSep, ...
                        Hm+Args.MinSep : Args.MinSep : SizeIm(1)-Hm-Args.MinSep);
    Gxy = [Gx(:), Gy(:)];

    if ~isempty(Obj.MaskData) && ~Obj.MaskData.isemptyImage
        BD_IM   = BitDictionary('BitMask.Image.Default');
        NaNmask = BD_IM.findBit(Obj.MaskData.Image, 'NaN');
        Blocked = imdilate(NaNmask, strel('square', 2.*ceil(Hm)+1));
        Gxy     = Gxy(~Blocked(sub2ind(SizeIm, Gxy(:,2), Gxy(:,1))), :);
    end

    Ninj = min(Args.Ninj, size(Gxy,1));
    if Ninj < 10.*Args.MinPerBin
        Info.Reason = sprintf('only %d clear grid positions', size(Gxy,1));
        return
    end

    Gxy  = Gxy(randperm(size(Gxy,1), Ninj), :);
    Finj = 10.^(log10(Args.FluxRng(1)) + rand(Ninj,1).*diff(log10(Args.FluxRng)));

    Info.Ninj = Ninj;

    % --- inject and measure ---
    Kern = {Kpsf, Ksmr};
    Hker = [Hp, Hs];
    Sc   = cell(1,2);
    Sm   = cell(1,2);
    Npop = 1 + Args.InjectSmear;

    for Ipop=1:1:Npop
        Img = Obj.Image;
        Hk  = Hker(Ipop);

        for Iinj=1:1:Ninj
            X = Gxy(Iinj,1);
            Y = Gxy(Iinj,2);
            Img(Y-Hk:Y+Hk, X-Hk:X+Hk) = Img(Y-Hk:Y+Hk, X-Hk:X+Hk) + ...
                                        Finj(Iinj).*Kern{Ipop};
        end

        Simg = normStat(Obj.Fd .* imUtil.filter.filter2_fast(Img, Kpsf));
        Mimg = normStat(Obj.Fd .* imUtil.filter.filter2_fast(Img, Template));

        % Sampled with the peak search measureTransients applies to real
        % candidates, so the threshold is fitted on the same quantity it is
        % later compared against.
        [Sc{Ipop}, ~, ~] = imUtil.properSub.findNearestPeakSig(Simg, ...
            Gxy(:,1), Gxy(:,2), 1, 'RadiusTS', Args.RadiusTS);
        [Sm{Ipop}, ~, ~] = imUtil.properSub.findNearestPeakSig(Mimg, ...
            Gxy(:,1), Gxy(:,2), 1, 'RadiusTS', Args.RadiusTS);
    end

    ScorePsf = Sc{1};
    Dpsf     = Sc{1} - Sm{1};

    Ppsf          = polyfit(abs(ScorePsf), Dpsf, 1);
    Info.PsfSlope = Ppsf(1);

    if Args.InjectSmear
        Psmr            = polyfit(abs(Sc{2}), Sc{2} - Sm{2}, 1);
        Info.SmearSlope = Psmr(1);
    end
    
    % --- the contour ---
    %  Estimated from a robust location and scale rather than the percentile
    %  itself. At a 99 per cent keep fraction the wanted percentile is the
    %  1st, and with of order a hundred injections per bin that order
    %  statistic is effectively the bin minimum, so a single injection
    %  landing on a real source would set the threshold for that bin.
    Kkeep = norminv(1 - Args.KeepFraction);

    for Ibin=1:1:numel(Args.BinEdges)-1
        Sel = abs(ScorePsf) >= Args.BinEdges(Ibin) & ...
              abs(ScorePsf) <  Args.BinEdges(Ibin+1);

        if sum(Sel) >= Args.MinPerBin
            Dbin = Dpsf(Sel);

            BinCen(end+1,1)       = median(abs(ScorePsf(Sel)), 'omitnan'); %#ok<AGROW>
            BinThr(end+1,1)       = median(Dbin, 'omitnan') + ...
                                    Kkeep .* 1.4826 .* mad(Dbin, 1);       %#ok<AGROW>
            Info.NumPerBin(end+1,1) = sum(Sel);                            %#ok<AGROW>
        end
    end

    if numel(BinCen) < 2
        Info.Reason = sprintf('only %d usable bins', numel(BinCen));
        BinCen = [];
        BinThr = [];
        return
    end

    Info.Fun = @(AbsScore) evalThreshold(AbsScore, BinCen, BinThr);
end


function S = normStat(S)
    % The same normalization AstroZOGY/subtractionS applies to S and S_smear,
    % so the injected statistics are on the footing the catalogue values are.

    S = imUtil.image.normalize(S, 'PreDef','norm_robust_rstd1', ...
                                  'K',1, 'Fun2Prob',[], 'Prob2Sig',false);
end


function Thresh = evalThreshold(AbsScore, BinCen, BinThr)
    % Linear interpolation between the per bin thresholds, with |SCORE|
    % clamped to the range the bins cover so nothing is extrapolated into a
    % regime no injection constrained.
    %
    %   Deliberately not capped at zero. Both branches can sit well above
    %   zero while remaining cleanly separated, and capping there puts the cut
    %   below the smear population entirely: on one image it took the catch
    %   rate from 87 to 98 per cent down to 8 to 34 per cent.

    Clamped = min(max(AbsScore, BinCen(1)), BinCen(end));
    Thresh  = interp1(BinCen, BinThr, Clamped, 'linear');
end