function writeImageData(FID, Data, ForceClass)
    % Write FITS image data matrix to an open file
    %   Format the data in vector of size of 2880*N blocks and add to file.
    % Input  : - An open file identifier.
    %          - Data to write.
    %          - Force data to class. If empty, use data class.
    % Output : null
    % Author : Eran Ofek (Jan 2022)
    % Example: 

    arguments
        FID
        Data
        ForceClass   = [];
    end

    BYTE_PER_BLOCK = 2880;

    if isempty(ForceClass)
       ForceClass = class(Data);
    else
        Data      = cast(Data, ForceClass);
    end
    
    DataLen = numel(Data);
    Size    = ceil(DataLen./BYTE_PER_BLOCK);

    DataVec = zeros(Size,1);
    DataVec(1:DataLen) = reshape(Data.', DataLen);

    fwrite(FID, DataVec, ForceClass, 0, 'b');

end
