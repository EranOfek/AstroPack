function Result=bitwise_cutouts(MaskImage, X, Y, HalfSize, IsOr)
    % Calculate the bitwise and/or for cutouts in an uint image.
    %   For each cutout coordinate (round of given coordinate) construct a
    %   stamp around the coordinate. Execute a bitwise and/or for all
    %   values in cutout and return a vector of result (one element per
    %   requested coordinates).
    %   This function uses the mex files:
    %       imUtil.cut.mex.mex_bitwise_cutouts_int32
    %       imUtil.cut.mex.mex_bitwise_cutouts_int16
    % Input  : - A bit mask image. Either unit16 or uint32 class.
    %          - Vector of X coordinates.
    %          - Vector of Y coordinates, the same length as the X vector.
    %          - Half size of cutout stamp. The stamp is of size
    %            2*HalfSize + 1. Default is 7.
    %          - A logical indicating if to calculate the bitwise or
    %            (true), or the bitwise and (false).
    %            Default is true.
    % Output : - A vector of results. A bitwise and/or per each stamp.
    % Author : Dan Elhanati (Aug 2023)
    % Example: MaskImage=uint32(rand(1700,1700).*2.^12);
    %          X = rand(1000,1).*1700; Y = rand(1000,1).*1700;
    %          R = imUtil.cut.bitwise_cutouts(MaskImage, X, Y);
    
    arguments
        MaskImage
        X
        Y
        HalfSize       = 7;
        IsOr logical   = true;
    end
    X = double(X);
    Y = double(Y);
    
    switch class(MaskImage)
        case 'uint32'
            Result = imUtil.cut.mex.mex_bitwise_cutouts_int32(MaskImage, X, Y, HalfSize, IsOr).';
        case 'uint16'
            Result = imUtil.cut.mex.mex_bitwise_cutouts_int16(MaskImage, X, Y, HalfSize, IsOr).';
        otherwise
            error('Input MaskImage must be of class uint16 or uint32');
    end
    
    
end
