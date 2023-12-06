function writeImageData(FID, Data, ForceClass)
    % Write FITS image data matrix to an open file
    %   Format the data in vector of size of 2880*N blocks and add to file.
    % Input  : - An open file identifier.
    %          - Data to write.
    %          - Force data to class. If empty, use data class.
    % Output : null
    % Author : Eran Ofek (Jan 2022)
    % Example: io.fits.writeImageData(FID, Data, ForceClass)

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
    Size    = BYTE_PER_BLOCK.*ceil(DataLen./BYTE_PER_BLOCK);

    %DataVec = zeros(Size, 1, ForceClass);
    %DataVec(1:DataLen) = reshape(Data.', DataLen, 1);

    switch ForceClass
        case {'double','int64','uint64'}
            Order = 's';
        otherwise
            Order = 'b';
    end
    %fwrite(FID, DataVec, ForceClass, 0, Order);
    fwrite(FID, Data.', ForceClass, 0, Order);
    % pad with zeros to the end of the 2880 bytes block
    fwrite(FID, zeros(Size-DataLen,1), ForceClass, 0, Order);
end
