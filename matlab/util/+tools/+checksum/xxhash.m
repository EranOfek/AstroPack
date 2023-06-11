function Result = xxhash(Args)
    % Compute xxHash value of given data or file.
    %
    % Input  : - 'Data' - Data array to process
    %          - 'FileName' - Filename to process
    %          - 'Seed' - Seed value
    %          - 'Hex' - True to return hex string, False return int64
    %
    % Output : - The xxHash digest of the input.
    %
    % Author : Chen Tishler (May 2023)
    % Example: 
    %    h64 = xxhash('Data', MyMat);
    %    h64 = xxhash('FileName', MyFileName);    
    %
    %----------------------------------------------------------------------
    arguments
        Args.Data = [];            % Input array
        Args.FileName = [];        % Input filename
        Args.Seed = 0;             % Seed value
        Args.Hex = true;           % True to return value as hex string (lowercase), otherwise return int64
    end

    if ~isempty(Args.Data)
        Result = tools.checksum.mex.mex_xxhash(Args.Data, int64(Args.Seed));
    elseif ~isempty(Args.FileName)
        Result = tools.checksum.mex.mex_xxhashFile(Args.FileName, int64(Args.Seed));
    else
        error('xxhash - Invalid argument');
    end
    
    % Convert to Hex string
    if Args.Hex
        Result = sprintf('%016x', Result);
    end
end
