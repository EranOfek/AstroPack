function Mask = setCoaddOverlap(Mask, Section, Args)
    % Reset the Overlap bit of coadd masks and set it outside a given section
    %   In the single epoch sub images the Overlap bit marks the border ring
    %   lying outside a section of the sub image. The epochs are dithered,
    %   so after registration and bitor coaddition of the masks the ring is
    %   smeared by the registration shifts and no longer follows the
    %   geometry of the coadd. This function drops the propagated bit and
    %   re-sets it from the coadd own geometry, i.e. on
    %   imUtil.ccdsec.flag_ccdsec(size(Mask), Section, false).
    %   The bit is cleared over the whole mask and set on four contiguous
    %   strips, so only the border ring is written.
    %   Under the policy of issue #1180 the section to pass is the
    %   exclusive (single-coverage) section (the EXCLSEC header keyword),
    %   so that the bit marks the full overlap region and a pixel covered
    %   by several crops is flagged in all of them; ownership is recorded
    %   in the catalog 'primary' column (imProc.cat.addPrimary). Passing
    %   the unique section (UNIQSEC) reproduces the older asymmetric
    %   flagging, in which only the non-owning crops flag the pixel.
    % Input  : - Mask images of an integer class: a 2-D array, a 3-D cube
    %            (image index in the 3rd dimension), or a cell array of 2-D
    %            arrays. Empty input is returned untouched.
    %          - Section [Xmin Xmax Ymin Ymax] outside of which the bit is
    %            set, given in the image own frame (e.g., the EXCLSEC or
    %            UNIQSEC header keyword).
    %            Either a 1x4 vector applied to all the images, or an
    %            Nimage-by-4 matrix, line per image.
    %            Sections are clipped to the image size.
    %          * ...,key,val,...
    %            'BitInd' - Index (starting from 0) of the Overlap bit. If
    %                   empty, it is resolved from 'BitName' and 'BitDict'.
    %                   Supplying it avoids constructing a BitDictionary.
    %                   Default is [].
    %            'BitName' - Bit name used when 'BitInd' is empty.
    %                   Default is 'Overlap'.
    %            'BitDict' - A BitDictionary object used when 'BitInd' is
    %                   empty. If empty, then use
    %                   BitDictionary('BitMask.Image.Default').
    %                   Default is [].
    % Output : - The input mask images, with the Overlap bit cleared and
    %            re-set according to the given section.
    % Author : A.M. Krassilchtchikov (2026 Aug)
    % Example: M = imUtil.mask.setCoaddOverlap(zeros(100,120,'uint32'), [11 110 6 95], 'BitInd',25);
    %          % equivalent to, but faster than:
    %          % Flag = imUtil.ccdsec.flag_ccdsec([100 120], [11 110 6 95], false);

    arguments
        Mask
        Section
        Args.BitInd        = [];
        Args.BitDict       = [];
        Args.BitName       = 'Overlap';
    end

    if isempty(Mask)
        return
    end

    % resolve the bit index once, so that the recursive cell branch and the
    % per image loop do not touch the dictionary
    BitInd = Args.BitInd;
    if isempty(BitInd)
        BitDict = Args.BitDict;
        if isempty(BitDict)
            BitDict = BitDictionary('BitMask.Image.Default');
        end
        BitInd = BitDict.name2bit(Args.BitName);
    end

    Nsec = size(Section,1);
    if size(Section,2)~=4
        error('Section must be a 4 column matrix of [Xmin Xmax Ymin Ymax]');
    end
    Section = double(Section);   % the sections may come as integers (e.g., from imUtil.cut.gridSubImage)

    % a cell array of masks: treat the elements one by one
    if iscell(Mask)
        Nim = numel(Mask);
        if Nsec~=1 && Nsec~=Nim
            error('Section must contain either a single line or a line per image');
        end
        for Iim=1:1:Nim
            Isec = min(Iim, Nsec);
            Mask{Iim} = imUtil.mask.setCoaddOverlap(Mask{Iim}, Section(Isec,:), 'BitInd',BitInd);
        end
        return
    end

    if ~isinteger(Mask)
        error('Mask must be of an integer class (e.g., uint32)');
    end

    [SizeI, SizeJ, Nim] = size(Mask);
    if Nsec~=1 && Nsec~=Nim
        error('Section must contain either a single line or a line per image');
    end

    BitVal   = cast(2.^double(BitInd), 'like',Mask);
    ClearVal = bitcmp(BitVal);

    % drop the bit propagated from the single epoch masks (single pass)
    Mask = bitand(Mask, ClearVal);

    % set the bit outside the given section of every image
    for Iim=1:1:Nim
        Isec = min(Iim, Nsec);
        Xmin = max(1,     round(Section(Isec,1)));
        Xmax = min(SizeJ, round(Section(Isec,2)));
        Ymin = max(1,     round(Section(Isec,3)));
        Ymax = min(SizeI, round(Section(Isec,4)));

        % the four strips covering the complement of the section:
        % X is the 2nd dimension (columns), Y is the 1st one (rows)
        StripRow = {':',            ':',              1:(Ymin-1), (Ymax+1):SizeI};
        StripCol = {1:(Xmin-1),     (Xmax+1):SizeJ,   Xmin:Xmax,  Xmin:Xmax};

        for Istrip=1:1:4
            RowInd = StripRow{Istrip};
            ColInd = StripCol{Istrip};
            if ~isempty(RowInd) && ~isempty(ColInd)
                Mask(RowInd, ColInd, Iim) = bitor(Mask(RowInd, ColInd, Iim), BitVal);
            end
        end
    end

end
