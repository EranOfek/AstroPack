function [SubMat] = cropMat(Array, CCDSEC)
    % Crop matrix using mex code. In some cases this is faster. Depands on size and type.
    %     
    %     Performing:
    %     SubMat=Array(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2))
    % Input  : - 2D array. Either single, double, uint16 or uint32
    %          - A CCDSEC (double) [Xmin Xmax Ymin Ymax].
    %            If empty, return input matrix without cropping.
    %            Default is [].
    % Output : - Cropped matrix
    % Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp -mavx" cropMat_double.cpp
    % Author : Eran Ofek (2024 Aug) 
    % Example: SubMat=tools.array.cropMat(Array, CCDSEC);

    arguments
        Array
        CCDSEC    = [];
    end
    
    if isempty(CCDSEC)
        SubMat = Array;
    else
        switch class(Array)
            case 'single'
                SubMat = tools.array.mex.cropMat_single(Array, CCDSEC);
            case 'double'
                SubMat = tools.array.mex.cropMat_double(Array, CCDSEC);
            case 'uint32'
                SubMat = tools.array.mex.cropMat_uint32(Array, CCDSEC);
            case 'uint16'
                SubMat = tools.array.mex.cropMat_uint16(Array, CCDSEC);
            otherwise
                error('Unknown Array class option');
        end
    end
end
