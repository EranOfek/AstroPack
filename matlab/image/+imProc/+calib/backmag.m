function [AI, Result] = backmag(AI, Args)
    % Estimate background surface brightness and add it to the image header.
    %
    % Calculate the sky/background surface brightness in mag arcsec^-2 using
    % the image background level, photometric zero point, and pixel scale:
    %
    %   MagBack = ZP - 2.5.*log10(BackVal./(PixScale.^2))
    %
    % where BackVal is in image counts per pixel, ZP is the photometric
    % zero point, and PixScale is in arcsec/pixel. The result is written to
    % the AstroImage header using Args.KeyBackMag.
    %
    % Input  : - AI, an AstroImage object or array of AstroImage objects.
    %          * ...,key,val,...
    %            'BackMethod' - Method by which to estimate the background.
    %                   Options are:
    %                   'Header' - Read background value from the header
    %                              keyword Args.KeyBackVal.
    %                   'Back'   - Estimate background as the median of
    %                              AI.BackData.Data sampled every
    %                              Args.MedianDilute pixels.
    %                   'Var'    - Estimate background as the median of
    %                              AI.VarData.Data sampled every
    %                              Args.MedianDilute pixels.
    %                   'Image'  - Estimate background as the median of
    %                              AI.ImageData.Data sampled every
    %                              Args.MedianDilute pixels.
    %                   Default is 'Header'.
    %            'MedianDilute' - Sampling step used when estimating the
    %                   median from BackData, VarData, or ImageData.
    %                   For example, 11 uses every 11th pixel.
    %                   Default is 11.
    %            'KeyBackVal' - Header keyword containing the background
    %                   value, used when Args.BackMethod='Header'.
    %                   Default is 'MEDBCK'.
    %            'KeyZP' - Header keyword containing the photometric zero
    %                   point.
    %                   Default is 'PH_ZP'.
    %            'KeyPixScale' - Header keyword containing the pixel scale
    %                   in arcsec/pixel.
    %                   Default is 'PIXSCALE'.
    %            'KeyBackMag' - Header keyword in which to store the
    %                   background surface brightness. If empty, the header
    %                   is not updated.
    %                   Default is 'BACKMAG'.
    %            'UpdateHeader' - Update header with BackMag.
    %                   Default is true.
    % Output : - AI, AstroImage object with updated background surface
    %                   brightness header keyword.
    %          - Result, structure array with one element per AstroImage.
    %                   Fields:
    %                   .BackVal  - Background value used in the calculation.
    %                   .ZP       - Photometric zero point.
    %                   .PixScale - Pixel scale in arcsec/pixel.
    %                   .MagBack  - Background surface brightness in
    %                               mag arcsec^-2.
    % Author : Eran Ofek (2026 Jun)
    % Example: [AI,Res] = imProc.calib.backmag(AI);
    %          [AI,Res] = imProc.calib.backmag(AI, 'BackMethod','Back');

    arguments
        AI
        Args.BackMethod        = 'Header'; % 'Header'|'Back'|'Var'|'Image'
        Args.MedianDilute      = 11;
        Args.KeyBackVal        = 'MEDBCK';
        Args.KeyZP             = 'PH_ZP';
        Args.KeyPixScale       = 'PIXSCALE';

        Args.KeyBackMag        = 'BACKMAG';
    end

    Nai = numel(AI);

    if nargout > 1
        Result = struct( ...
            'BackVal',   cell(size(AI)), ...
            'ZP',        cell(size(AI)), ...
            'PixScale',  cell(size(AI)), ...
            'MagBack',   cell(size(AI)));
    end

    for Iai = 1:1:Nai
        % get image background
        switch Args.BackMethod
            case 'Header'
                BackVal = AI(Iai).HeaderData.getValSimple(Args.KeyBackVal);
            case 'Back'
                BackVal = fast_median(AI(Iai).BackData.Data(1:Args.MedianDilute:end));
            case 'Var'
                BackVal = fast_median(AI(Iai).VarData.Data(1:Args.MedianDilute:end));
            case 'Image'
                BackVal = fast_median(AI(Iai).ImageData.Data(1:Args.MedianDilute:end));
            otherwise
                error('Unknown BackMethod option');
        end

        % get photometric ZP
        ZP = AI(Iai).HeaderData.getValSimple(Args.KeyZP);

        % get PixScale [arcsec/pix]
        PixScale = AI(Iai).HeaderData.getValSimple(Args.KeyPixScale);

        MagBack = ZP - 2.5.*log10(BackVal./(PixScale.^2));

        if nargout>1
            Result(Iai).BackVal  = BackVal;
            Result(Iai).ZP       = ZP;
            Result(Iai).PixScale = PixScale;
            Result(Iai).MagBack  = MagBack;
        end

        if ~isempty(Args.KeyBackMag)
            AI(Iai).HeaderData.replaceVal(Args.KeyBackMag, MagBack);
        end

    end

end
