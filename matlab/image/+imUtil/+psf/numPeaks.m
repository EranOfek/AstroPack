function [PeaksHeight] = numPeaks(PSF, Args)
    % Number of peaks, and their height, in PSF stamp.
    % Input  : - A PSF stamp.
    %          * ...,key,val,... 
    %            'Conn' - local max. connectivity: 4|8. Default is 8.
    %            'Sort' - If true, sort the output by descending order.
    % Output : - A vector of peaks heights.
    % Author : Eran Ofek (2026 Mar) 
    % Example: PH=imUtil.psf.numPeaks(PSF);

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

end
