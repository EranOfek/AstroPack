function Result = xxhash(Args)
    %
    % Input  : - 'Data' - 
    %          - 'FileName' - 
    %          - 'Seed' - 
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
        Args.Seed = 0;             %
        Args.Hex = true;
    end

    if ~isempty(Args.Data)
        Result = tools.checksum.mex_xxhash(Args.Data, int64(Args.Seed));
    elseif ~isempty(Args.FileName)
        Result = tools.checksum.mex_xxhashFile(Args.FileName, int64(Args.Seed));
    else
        error('xxhash - Invalid argument');
    end
    
    % Convert to Hex string
    if Args.Hex
        Result = sprintf('%016x', Result);
    end
end
