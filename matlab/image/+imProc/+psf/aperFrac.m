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
            Data = [Keys(:), num2cell(Result(Iai).Frac(:))];
            AI(Iai).HeaderData.insertKey(Data);
        end
    end


end
