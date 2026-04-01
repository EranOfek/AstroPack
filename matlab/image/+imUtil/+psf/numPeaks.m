function [PeaksHeight,Dist] = numPeaks(PSF, Args)
    % Number of peaks, and their height, in PSF stamp.
    % Input  : - A PSF stamp.
    %          * ...,key,val,... 
    %            'Conn' - local max. connectivity: 4|8. Default is 8.
    %            'Sort' - If true, sort the output by descending order.
    % Output : - A vector of peaks heights.
    %          - Distance in pixels between two height peaks.
    %            Return NaN if only one peak is found.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [PH,Dist]=imUtil.psf.numPeaks(PSF);

    arguments
        PSF
        Args.Conn  = 8;
        Args.Sort  = true;
    end

    BW = imregionalmax(PSF, Args.Conn);
    PeaksHeight = PSF(BW(:));
    if Args.Sort
        PeaksHeight = sort(PeaksHeight, 'descend');
    end

    if nargout>1
        if numel(PeaksHeight)==1
            Dist = NaN;
        else
            OnlyPeaks = BW.*PSF;
            [~,Isort] = sort(OnlyPeaks(:), 'descend');
            [I,J] = ind2sub(size(PSF), Isort(1:2));
            Dist = sqrt((I(1)-I(2)).^2 + (J(1)-J(2)).^2);
        end
    end

end
