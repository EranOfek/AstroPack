function [Central, IsCentral] = centralCrops(TileOrder, Ncrop)
    % Central-crop indices for a LAST focal-plane tiling.
    % Description: Returns the indices of the eight central crops of a 24-crop
    %              LAST mosaic for the requested tile ordering. Replaces the
    %              tile-order switch block duplicated across the photCalib
    %              plotting functions.
    % Input  : - TileOrder - 'colmajor' | 'rowmajor'. Any other value yields
    %            an empty central set. Default 'rowmajor'.
    %          - Ncrop     - Number of crops; indices above Ncrop are
    %            dropped. Default 24.
    % Output : - Central   - row vector of central-crop indices.
    %          - IsCentral - 1xNcrop logical, true at central-crop indices.
    % Author : photCalib package refactor (2026-05)
    % Example: [C, IsC] = centralCrops('rowmajor', 24);

    arguments
        TileOrder {mustBeTextScalar} = 'rowmajor'
        Ncrop     (1,1) double       = 24
    end

    switch lower(char(TileOrder))
        case 'colmajor'
            Central = [8 9 10 11 14 15 16 17];
        case 'rowmajor'
            Central = [6 7 10 11 14 15 18 19];
        otherwise
            Central = [];
    end

    Central = Central(Central >= 1 & Central <= Ncrop);

    IsCentral = false(1, Ncrop);
    IsCentral(Central) = true;
end
