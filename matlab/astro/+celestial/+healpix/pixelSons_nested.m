function [Result] = pixelSons_nested(NSide, PixInd)
    % Given a nested pixel index, returns the 4 pixels in the next Nside level.
    % Input  : - NSide
    %          - Pixel indices (column vector).
    % Output : - A 4 column matrix of pixel indices in the next NSide
    %            level. Each row corresponds to one element in the input
    %            pixel indices.
    % Author : Eran Ofek (2025 Jan) 
    % Example: celestial.healpix.pixelSons_nested(2.^16,[0;1])

    Result = 4.*PixInd(:) + (0:1:3);
    
end
