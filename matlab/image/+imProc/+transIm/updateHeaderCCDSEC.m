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
    %            'ExclusiveCCDSEC' - Like EdgesCCDSEC, but for the CCDSEC
    %                   of the exclusive (single-coverage) section of the
    %                   sub image in the full image frame, i.e. the part
    %                   of the sub image that no other sub image covers.
    %                   Default is [].
    %            'NewExclusive' - Like ExclusiveCCDSEC, but measured in the
    %                   new sub image frame.
    %                   Its complement within the sub image is the full
    %                   overlap region (pixels covered by 2+ sub images).
    %                   A dedicated keyword is needed because this section
    %                   is bounded by the NEIGHBOUR footprint edges, which
    %                   are not recoverable from this image own keywords:
    %                   the unique-section boundary is the floor-rounded
    %                   midpoint of the shared strip, so inverting it
    %                   leaves the neighbour edge ambiguous by 1 pixel
    %                   whenever the overlap width is odd (as in the LAST
    %                   grid, 159 px in X split 79/80).
    %                   Default is [].
    %            'KeyCCDSEC' - Keywirds of CCDSEC. Default is 'CCDSEC'.
    %            'KeyORIGSEC' - Keyword of EdgesCCDSEC.
    %                   Default is 'ORIGSEC.
    %            'KeyORIGUSEC' - Keyword of NoOverlapCCDSEC.
    %                   Default is 'ORIGUSEC'.
    %            'KeyUNIQSEC' - Keyword of NewNoOverlap.
    %                   Default is 'UNIQSEC'.
    %            'KeyORIGESEC' - Keyword of ExclusiveCCDSEC.
    %                   Default is 'ORIGESEC'.
    %            'KeyEXCLSEC' - Keyword of NewExclusive.
    %                   Default is 'EXCLSEC'.
    % Output : - The input AstroImage object with the updated header.
    %            New copy is not generated.
    % Author : Eran Ofek (Nov 2021)
    % Example: Obj = imProc.transIm.updateHeaderCCDSEC(Obj)
    
    arguments
        Obj
        
                                      % CCDSEC of current image
        Args.EdgesCCDSEC     = [];    % ORIGSEC : SEC of subimage in full image
        Args.NoOverlapCCDSEC = [];    % ORIGUSEC : SEC of non-overlapping sub image in full image
        Args.NewNoOverlap    = [];    % UNIQSEC : SEC of non-overlapping sub image in new sub image
        Args.ExclusiveCCDSEC = [];    % ORIGESEC : SEC of the single-coverage (exclusive) sub image section in full image
        Args.NewExclusive    = [];    % EXCLSEC : SEC of the single-coverage (exclusive) sub image section in new sub image

        Args.KeyCCDSEC       = 'CCDSEC';
        Args.KeyORIGSEC      = 'ORIGSEC';
        Args.KeyORIGUSEC     = 'ORIGUSEC';
        Args.KeyUNIQSEC      = 'UNIQSEC';
        Args.KeyORIGESEC     = 'ORIGESEC';
        Args.KeyEXCLSEC      = 'EXCLSEC';
    end

    KeyNames = {'NAXIS1','NAXIS2', Args.KeyCCDSEC, Args.KeyORIGSEC, Args.KeyORIGUSEC, Args.KeyUNIQSEC, Args.KeyEXCLSEC, Args.KeyORIGESEC};
    
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
        if ~isempty(Args.NewExclusive)
            KeyVals{7} = imUtil.ccdsec.ccdsec2str(Args.NewExclusive(Isub,:));           % EXCLSEC : SEC of the single-coverage section in new sub image
        end
        if ~isempty(Args.ExclusiveCCDSEC)
            KeyVals{8} = imUtil.ccdsec.ccdsec2str(Args.ExclusiveCCDSEC(Isub,:));        % ORIGESEC : SEC of the single-coverage section in full image
        end

        Obj(Isub).HeaderData.replaceVal(KeyNames, KeyVals);
    end

    
end
