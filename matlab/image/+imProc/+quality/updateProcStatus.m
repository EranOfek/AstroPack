function [Result] = updateProcStatus(AllSI, Coadd, MS, CoaddPC, Args)
    % Update the PSTATUS (Processed status) in the image header.
    %   Add/update PSTATUS header keyword. This keyword is a bit mask
    %   of flags indicating which processing steps were failed.
    %   The default bit mask is defined in the config/ dir:
    %   BitMask.ImageQuality.Default.yml
    % Input  : - An array of AstroImage object
    %          * ...,key,val,... 
    %            'KeyProcStatus' - Proc. status header keyword name.
    %                   Default is 'PSTATUS'.
    %
    % Output : - The array of AstroIage object in which the header was
    %            updated.
    % Author : Eran Ofek (2026 Sep) 
    % Example: 

    arguments
        AllSI  % Epoxh X Crop
        Coadd
        MS
        CoaddPC
        Args.KeyProcstatus     = 'PSTATUS';
        Args.BitDictionary     = BitDictionary('BitMask.ImageQuality.Default.yml');
    end

    SizeSI = size(AllSI);
    Ncoadd = numel(Coadd);
    Nms    = numel(MS);
    Npc    = numel(CoaddPC);

    for Icrop=1:1:SizeSI(2)
        % for each crop
        
        % check Coadd
        if Icrop>Ncoadd
            % No coadd image
        else
            if isempty(Coadd(Icrop).ImageData.Data)
                % No Coadd image
            else
                % Coadd image exist
                [NoBck, NoVar, NoSrc, NoPsf, NoAstr, NoPhotCal, NoMerged, NoCaodd] = checkSingleImage(AllSI,PC);
            end






        Coadd(Icoadd)
        % for each image
        Nsrc   = size(AI(Iai).CatData.catalog,1);
        IsPSF  = ~isempty(AI(Iai).PSFData.Data);
        IsAstr = AI(Iai).WCS.Success;
        IsPhot = AI(Iai). - where is it?

        % create bit mask

        % update bit mask in header


    end
end


function [NoBck, NoVar, NoSrc, NoPsf, NoAstr, NoPhotCal, NoMerged, NoCaodd] = checkSingleImage(AI,PC)
    % Check single image

    IsBackEmpty = isempty(AI.BackData.Data);
    IsVarEmpty  = isempty(AI.VarData.Data); 
    IsCatEmpty  = isempty(AI.CatData.Catalog);
    IsPsfEmpty  = isempty(AI.PSFData.Data);
    IsAstrOK    = AI.WCS.Success;
    IsPhotCalib = ~(isempty(PC.PhotZP) || isnan(PC.PhotZP));

end