function [AllSI, Coadd] = updateProcStatus(AllSI, Coadd, MS, CoaddPC, Args)
    % Update the PSTATUS (processed status) bit mask in image headers.
    %   For every epoch image and every coadd, set the PSTATUS header
    %   keyword to the decimal bit mask of failed processing steps.
    %   Bit names are defined in config/BitMask.ImageQuality.Default.yml.
    %   A value of 0 means every listed step succeeded.
    %
    %   Image-local bits (from that AstroImage):
    %     NO_BKG, NO_SRC, NO_PSF, NO_ASTR
    %   Crop-level bits (copied onto every epoch of the crop and the coadd):
    %     NO_PHOTCAL, NO_MERGE, NO_COADD
    %
    % Input  : - AllSI: AstroImage array, [Nepoch x Ncrop].
    %          - Coadd: AstroImage array, one coadd per crop. May be [].
    %          - MS: MatchedSources array, one object per crop. May be [].
    %          - CoaddPC: PhotCalibTrans array, one object per crop. May be [].
    %          * ...,key,val,...
    %            'KeyProcStatus' - Header keyword. Default is 'PSTATUS'.
    %            'BitDictionary' - BitDictionary, or a dictionary name.
    %                   Default is 'BitMask.ImageQuality.Default'.
    % Output : - AllSI with PSTATUS written into each header.
    %          - Coadd with PSTATUS written into each existing coadd header.
    % Author : Eran Ofek (2026 Sep)
    % Example: [AllSI, Coadd] = imProc.quality.updateProcStatus(AllSI, Coadd, MS, PC);

    arguments
        AllSI
        Coadd
        MS
        CoaddPC
        Args.KeyProcStatus  = 'PSTATUS';
        Args.BitDictionary  = 'BitMask.ImageQuality.Default';
    end

    if ischar(Args.BitDictionary) || isstring(Args.BitDictionary)
        BitDict = BitDictionary(char(Args.BitDictionary));
    else
        BitDict = Args.BitDictionary;
    end

    [Nepoch, Ncrop] = size(AllSI);
    Ncoadd = numel(Coadd);
    Nms    = numel(MS);
    Npc    = numel(CoaddPC);

    for Icrop = 1:1:Ncrop
        NoCoadd   = Icrop > Ncoadd || coaddImageIsEmpty(Coadd(Icrop));
        NoMerge   = Icrop > Nms    || mergedIsEmpty(MS(Icrop));
        NoPhotCal = Icrop > Npc    || photZPMissing(CoaddPC(Icrop));

        for Iep = 1:1:Nepoch
            writePstatus(AllSI(Iep, Icrop), BitDict, Args.KeyProcStatus, ...
                NoPhotCal, NoMerge, NoCoadd);
        end

        if Icrop <= Ncoadd
            writePstatus(Coadd(Icrop), BitDict, Args.KeyProcStatus, ...
                NoPhotCal, NoMerge, NoCoadd);
        end
    end

end


function writePstatus(AI, BitDict, Key, NoPhotCal, NoMerge, NoCoadd)
    % Write the PSTATUS decimal mask into one AstroImage header.
    [NoBack, NoVar] = AI.isemptyImage({'Back','Var'});
    NoBkg  = NoBack || NoVar;
    NoSrc  = AI.isemptyCatalog || AI.sizeCatalog == 0;
    NoPsf  = AI.isemptyPSF;
    NoAstr = isempty(AI.WCS) || ~AI.WCS.Success;

    Names = {'NO_BKG','NO_SRC','NO_PSF','NO_ASTR','NO_PHOTCAL','NO_MERGE','NO_COADD'};
    Names = Names([NoBkg, NoSrc, NoPsf, NoAstr, NoPhotCal, NoMerge, NoCoadd]);
    if isempty(Names)
        BitDec = 0;
    else
        [~,~,BitDec] = BitDict.name2bit(Names);
    end
    AI.HeaderData.replaceVal(Key, BitDec, 'Comment', {'Processing status bit mask'});
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
