function [Result, AI] = aperFrac(AI, Args)
    % Calculate fraction of light within aperture in PSF, and write to header. 
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'AperRadius' - Aperture radius (pix).
    %                   Default is [3, 5, 6, 7]
    %            'KeyAperFrac' - Prefix of header keyword that will contain
    %                   the aperture flux fraction.
    %                   If empty, then do not write to header.
    %                   Default is 'PSF_AF'.
    % Output : - A structure array with 'Frac' info per AstroImage PSF.
    %          - The updated AstroImage.
    % Author : Eran Ofek (2026 Jun) 
    % Example: [~,AI] = imProc.psf.aperFrac(AI);

    arguments
        AI
       
        Args.AperRadius             = [3, 5, 6, 7];
        Args.KeyAperFrac            = 'PSF_AF';

    end

    Naper = numel(Args.AperRadius);
    if ~isempty(Args.KeyAperFrac)
        Keys = tools.cell.cellNumericSuffix(Args.KeyAperFrac, (1:1:Naper));
    end
    Nai = numel(AI);
    Result = struct('Frac',cell(Nai,1));

    for Iai=1:1:Nai
        Result(Iai).Frac = imUtil.sources.mex.aper_phot_cube_simple(AI(Iai).PSFData.Data, 0, 0, 0, Args.AperRadius);

        if ~isempty(Args.KeyAperFrac)
            % replaceVal (not insertKey): the keywords may already be in the
            % header - a coadd inherits them from the single epoch it was
            % built from, and this function runs on both, so appending gave
            % two sets of PSF_AF_* in every coadd (issue #1269). replaceVal
            % updates in place and still adds the keys when absent.
            AI(Iai).HeaderData.replaceVal(Keys(:), num2cell(Result(Iai).Frac(:)), 'AddPos','end-1');
        end
    end


end
