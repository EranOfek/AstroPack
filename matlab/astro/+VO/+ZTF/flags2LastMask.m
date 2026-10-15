function [Mask, Map] = flags2LastMask(Flags, Args)
    % Translate SExtractor FLAGS into a LAST bit mask.
    % Description: ZTF's two catalogs carry two different flag schemes, and
    %              neither is the LAST one. A column literally named FLAGS is
    %              decoded by AstroPack (PhotCalibTrans.applyCalibQuality,
    %              calcAperCorr) against BitMask.Image.Default, so ZTF's raw
    %              values must never be exposed under that name. This
    %              translates the conditions that DO have a LAST equivalent
    %              and leaves everything else at zero; the untranslated
    %              original travels alongside as SEX_FLAGS / ZTF_FLAGS.
    %
    %              Translated (SExtractor's documented bits):
    %                 4 (bit 2) saturated pixel in the aperture -> Saturated
    %                 8 (bit 3) truncated by an image boundary  -> NearEdge
    %              NOT translated, by design: 1 neighbour-biased, 2 deblended,
    %              16/32 corrupted aperture/isophotal data, 64/128 memory
    %              overflow. They describe the extraction, not the pixels,
    %              and LAST has no bit meaning the same thing.
    %
    %              The psfcat flag column is deliberately NOT supported: its
    %              bits (observed values 1, 16, 256, 4096 and -1) are not
    %              documented in the file and guessing them would write
    %              wrong Saturated/NearEdge bits into a column the
    %              calibration then trusts.
    % Input  : - Flags - vector of SExtractor FLAGS values.
    %          * ...,key,val,...
    %            'Source' - Flag scheme of the input. Only 'sextractor' is
    %                   supported. Default is 'sextractor'.
    %            'DictionaryName' - LAST bit dictionary. Default is
    %                   'BitMask.Image.Default'.
    % Output : - Mask - uint32 vector of LAST bit values, same size as Flags.
    %          - Map  - struct describing what was done: .Pairs (cell of
    %                   {SExtractor value, LAST bit name}), .Nset (how many
    %                   rows got each bit), .Nbad (rows whose flag value was
    %                   negative, NaN or non-integer, mapped to 0).
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Mask, Map] = VO.ZTF.flags2LastMask(Cat.getCol('SEX_FLAGS'));

    arguments
        Flags
        Args.Source (1,:) char {mustBeMember(Args.Source, {'sextractor'})} = 'sextractor'
        Args.DictionaryName (1,:) char = 'BitMask.Image.Default'
    end

    Pairs = {4, 'Saturated'; 8, 'NearEdge'};

    F    = double(Flags(:));
    Bad  = ~isfinite(F) | F < 0 | F ~= floor(F);
    F(Bad) = 0;
    Fu   = uint32(F);

    BD   = BitDictionary(Args.DictionaryName);
    Mask = zeros(size(Fu), 'uint32');
    Nset = zeros(1, size(Pairs,1));
    for Ip = 1:size(Pairs,1)
        [~, BitDec] = BD.name2bit(Pairs{Ip,2});
        Hit = bitand(Fu, uint32(Pairs{Ip,1})) > 0;
        Mask(Hit) = bitor(Mask(Hit), uint32(BitDec));
        Nset(Ip)  = sum(Hit);
    end

    Mask = reshape(Mask, size(Flags));
    Map  = struct('Pairs', {Pairs}, 'Nset', Nset, 'Nbad', sum(Bad));
end
