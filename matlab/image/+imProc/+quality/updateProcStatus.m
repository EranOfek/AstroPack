function [AllSI, Coadd] = updateProcStatus(AllSI, Coadd, MS, CoaddPC, Args)
    % Update the PSTATUS (processed status) bit mask in image headers.
    %   For every epoch image and every coadd, set the PSTATUS header
    %   keyword to the decimal bit mask of failed processing steps.
    %   Bit names are defined in config/BitMask.ImageQuality.Default.yml.
    %   A value of 0 means every listed step succeeded.
    %
    %   Image-local bits (from that AstroImage):
    %     NO_BKG, NO_SRC, NO_PSF, NO_ASTR
    %   Sub image group bits (copied onto every epoch of the group and the coadd):
    %     NO_PHOTCAL, NO_MERGE, NO_COADD, NO_RELZP
    %   Epoch bits (set on the epoch images only, never on the coadd):
    %     NOT_GOOD, FEW_SRC, HIGH_BKGRAD
    %   The epoch bits are quality decisions taken by the caller (the
    %   thresholds live there), so they are supplied as arguments; a bit whose
    %   argument is not given is never set. A coadd PSTATUS of 0 therefore
    %   does not mean that every epoch of the group was good.
    %
    % Input  : - AllSI: AstroImage array, [Nepoch x Ncrop].
    %          - Coadd: AstroImage array, one coadd per crop. May be [].
    %          - MS: MatchedSources array, one object per crop. May be [].
    %          - CoaddPC: PhotCalibTrans array, one object per crop. May be [].
    %          * ...,key,val,...
    %            'IsGood' - Logical array which is true for the sub images that
    %                   took part in the coaddition and in the matched sources.
    %                   NOT_GOOD is set where it is false. May be a scalar, a
    %                   per epoch column, a per sub image group row, or an
    %                   [Nepoch x Ncrop] array. Default is [] (bit never set).
    %            'FewSrc' - Logical array, true where the number of sources is
    %                   below the coaddition threshold. Sets FEW_SRC.
    %                   Shapes as in IsGood. Default is [].
    %            'HighBkgGrad' - Logical array, true where the background
    %                   gradient over the epoch is above the threshold.
    %                   Sets HIGH_BKGRAD. Shapes as in IsGood. Default is [].
    %            'NoRelZP' - Logical array, true for the sub image groups with
    %                   no relative photometric zero point. Sets NO_RELZP on
    %                   every epoch of the group and on its coadd.
    %                   Shapes as in IsGood. Default is [].
    %            'KeyProcStatus' - Header keyword. Default is 'PSTATUS'.
    %            'BitDictionary' - BitDictionary, or a dictionary name.
    %                   Default is 'BitMask.ImageQuality.Default'.
    % Output : - AllSI with PSTATUS written into each header.
    %          - Coadd with PSTATUS written into each existing coadd header.
    % Author : Eran Ofek (2026 Sep)
    % Example: [AllSI, Coadd] = imProc.quality.updateProcStatus(AllSI, Coadd, MS, PC);
    %          [AllSI, Coadd] = imProc.quality.updateProcStatus(AllSI, Coadd, MS, PC, 'IsGood',IsGood);

    arguments
        AllSI
        Coadd
        MS
        CoaddPC
        Args.IsGood         = [];
        Args.FewSrc         = [];
        Args.HighBkgGrad    = [];
        Args.NoRelZP        = [];
        Args.KeyProcStatus  = 'PSTATUS';
        Args.BitDictionary  = 'BitMask.ImageQuality.Default';
    end

    % The order of the names is the order of the flags in writePstatus
    BitNames = {'NO_BKG','NO_SRC','NO_PSF','NO_ASTR', ...
                'NO_PHOTCAL','NO_MERGE','NO_COADD','NO_RELZP', ...
                'NOT_GOOD','FEW_SRC','HIGH_BKGRAD'};

    if ischar(Args.BitDictionary) || isstring(Args.BitDictionary)
        BitDict = BitDictionary(char(Args.BitDictionary));
    else
        BitDict = Args.BitDictionary;
    end

    % Resolve the bit names once, for speed and because name2bit sums with
    % 'omitnan': a name missing from the dictionary contributes 0, so with an
    % outdated dictionary a broken image would be reported as clean.
    [BitInd, BitDec] = BitDict.name2bit(BitNames);
    UnknownBit = isnan(BitInd);
    if any(UnknownBit)
        BitDec(UnknownBit) = 0;
        warning('imProc:quality:updateProcStatus:UnknownBitName', ...
                'Bit name(s) %s are missing from the dictionary %s - these status bits are never set', ...
                strjoin(BitNames(UnknownBit), ', '), BitDict.BitDictName);
    end

    [Nepoch, Ncrop] = size(AllSI);
    Ncoadd = numel(Coadd);
    Nms    = numel(MS);
    Npc    = numel(CoaddPC);

    if isempty(Args.IsGood)
        NotGood = false(Nepoch, Ncrop);
    else
        NotGood = ~expandFlag(Args.IsGood, Nepoch, Ncrop, 'IsGood');
    end
    FewSrc      = expandFlag(Args.FewSrc,      Nepoch, Ncrop, 'FewSrc');
    HighBkgGrad = expandFlag(Args.HighBkgGrad, Nepoch, Ncrop, 'HighBkgGrad');
    NoRelZP     = expandFlag(Args.NoRelZP,     Nepoch, Ncrop, 'NoRelZP');

    for Icrop = 1:1:Ncrop
        NoCoadd     = Icrop > Ncoadd || coaddImageIsEmpty(Coadd(Icrop));
        NoMerge     = Icrop > Nms    || mergedIsEmpty(MS(Icrop));
        NoPhotCal   = Icrop > Npc    || photZPMissing(CoaddPC(Icrop));
        NoRelZPcrop = NoRelZP(1, Icrop);

        for Iep = 1:1:Nepoch
            writePstatus(AllSI(Iep, Icrop), BitDec, Args.KeyProcStatus, ...
                [NoPhotCal, NoMerge, NoCoadd, NoRelZPcrop, ...
                 NotGood(Iep,Icrop), FewSrc(Iep,Icrop), HighBkgGrad(Iep,Icrop)]);
        end

        if Icrop <= Ncoadd
            % The last three bits describe single epochs and are left unset
            % on the coadd.
            writePstatus(Coadd(Icrop), BitDec, Args.KeyProcStatus, ...
                [NoPhotCal, NoMerge, NoCoadd, NoRelZPcrop, false, false, false]);
        end
    end

end


function writePstatus(AI, BitDec, Key, Flags)
    % Write the PSTATUS decimal mask into one AstroImage header.
    %   Flags is [NO_PHOTCAL, NO_MERGE, NO_COADD, NO_RELZP, NOT_GOOD,
    %   FEW_SRC, HIGH_BKGRAD]; the image-local bits are measured here.
    [NoBack, NoVar] = AI.isemptyImage({'Back','Var'});
    % A failed background estimation (issue #1226) leaves an all NaN Back and
    % Var rather than an empty one, so both tests are needed.
    NoBkg  = NoBack || NoVar || imProc.background.isFailedBack(AI);
    NoSrc  = AI.isemptyCatalog || AI.sizeCatalog == 0;
    NoPsf  = AI.isemptyPSF;
    NoAstr = isempty(AI.WCS) || ~AI.WCS.Success;

    % Written as an integer so that the card is an integer and not a real
    BitMask = int32(sum(BitDec([NoBkg, NoSrc, NoPsf, NoAstr, logical(Flags)])));
    AI.HeaderData.replaceVal(Key, BitMask, 'Comment', {'Processing status bit mask'});
end


function Flag = expandFlag(Flag, Nepoch, Ncrop, Name)
    % Bring a quality flag to the [Nepoch x Ncrop] shape.
    %   Accepts a scalar, a per epoch column, a per sub image group row, or
    %   the full array. An empty flag means "not evaluated" and gives false.
    if isempty(Flag)
        Flag = false(Nepoch, Ncrop);
    elseif isscalar(Flag)
        Flag = repmat(logical(Flag), Nepoch, Ncrop);
    elseif isequal(size(Flag), [Nepoch, Ncrop])
        Flag = logical(Flag);
    elseif numel(Flag)==Nepoch && iscolumn(Flag)
        Flag = repmat(logical(Flag), 1, Ncrop);
    elseif numel(Flag)==Ncrop && isrow(Flag)
        Flag = repmat(logical(Flag), Nepoch, 1);
    else
        error('imProc:quality:updateProcStatus:BadFlagSize', ...
              '%s must be a scalar, a %d element column, a %d element row, or a [%d %d] array', ...
              Name, Nepoch, Ncrop, Nepoch, Ncrop);
    end
end


function Tf = coaddImageIsEmpty(AI)
    % True when the coadd AstroImage has no science image.
    Tf = AI.isemptyImage('Image');
end


function Tf = mergedIsEmpty(M)
    % True when the matched-sources product has no sources.
    if isempty(M)
        Tf = true;
        return;
    end
    Nsrc = M.Nsrc;
    Tf = isempty(Nsrc) || Nsrc == 0;
end


function Tf = photZPMissing(PC)
    % True when the coadd photometric ZP was not measured.
    if isempty(PC)
        Tf = true;
        return;
    end
    Zp = PC.PhotZP;
    Tf = isempty(Zp) || ~isscalar(Zp) || ~isfinite(Zp);
end
