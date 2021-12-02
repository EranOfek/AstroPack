function Obj = updateHeaderCCDSEC(Obj, Args)
    % Update the NAXIS and CCDSEC related keywords in the header
    %       NAXIS1, NAXIS2 and CCDSEC are always added.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'EdgesCCDSEC' - A 4 column matrix, with line per
    %                   AstroImage element, with the CCDSEC of the subimage
    %                   in the full image. If empty do not add to header.
    %                   Default is [].
    %            'NoOverlapCCDSEC' - Like EdgesCCDSEC, but for the CCDSEC
    %                   of the non-overlapping sub image in full image.
    %                   Default is [].
    %            'NewNoOverlap' - Like EdgesCCDSEC, but for the CCDSEC
    %                   of the non-overlapping sub image in new sub image.
    %                   Default is [].
    %            'KeyCCDSEC' - Keywirds of CCDSEC. Default is 'CCDSEC'.
    %            'KeyORIGSEC' - Keyword of EdgesCCDSEC.
    %                   Default is 'ORIGSEC.
    %            'KeyORIGUSEC' - Keyword of NoOverlapCCDSEC.
    %                   Default is 'ORIGUSEC'.
    %            'KeyUNIQSEC' - Keyword of NewNoOverlap.
    %                   Default is 'UNIQSEC'.
    % Output : - The input AstroImage object with the updated header.
    %            New copy is not generated.
    % Author : Eran Ofek (Nov 2021)
    % Example: Obj = imProc.transIm.updateHeaderCCDSEC(Obj)
    
    arguments
        Obj AstroImage
        
                                      % CCDSEC of current image
        Args.EdgesCCDSEC     = [];    % ORIGSEC : SEC of subimage in full image
        Args.NoOverlapCCDSEC = [];    % ORIGUSEC : SEC of non-overlapping sub image in full image
        Args.NewNoOverlap    = [];    % UNIQSEC : SEC of non-overlapping sub image in new sub image
        
        Args.KeyCCDSEC       = 'CCDSEC';
        Args.KeyORIGSEC      = 'ORIGSEC';
        Args.KeyORIGUSEC     = 'ORIGUSEC';
        Args.KeyUNIQSEC      = 'UNIQSEC';
    end
    
    KeyNames = {'NAXIS1','NAXIS2', Args.KeyCCDSEC, Args.KeyORIGSEC, Args.KeyORIGUSEC, Args.KeyUNIQSEC};
    
    Nsub = numel(Obj);
    KeyVals  = cell(size(KeyNames));
    for Isub=1:1:Nsub
        
    
        % update the header
        KeyVals{1} = size(Obj(Isub).ImageData.Image,2);  % NAXIS1
        KeyVals{2} = size(Obj(Isub).ImageData.Image,1);  % NAXI2
        KeyVals{3} = imUtil.ccdsec.ccdsec2str([1, KeyVals{1}, 1, KeyVals{2}]); % CCDSEC of current image
        if ~isempty(Args.EdgesCCDSEC )
            KeyVals{4} = imUtil.ccdsec.ccdsec2str(Args.EdgesCCDSEC(Isub,:));            % ORIGSEC : SEC of subimage in full image
        end
        if ~isempty(Args.NoOverlapCCDSEC)
            KeyVals{5} = imUtil.ccdsec.ccdsec2str(Args.NoOverlapCCDSEC(Isub,:));        % ORIGUSEC : SEC of non-overlapping sub image in full image
        end
        if ~isempty(Args.NewNoOverlap)
            KeyVals{6} = imUtil.ccdsec.ccdsec2str(Args.NewNoOverlap(Isub,:));           % UNIQSEC : SEC of non-overlapping sub image in new sub image
        end

        Obj(Isub).HeaderData.replaceVal(KeyNames, KeyVals);
    end

    
end
