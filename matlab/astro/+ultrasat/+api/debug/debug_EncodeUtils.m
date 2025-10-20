%==========================================================================
% ULTRASAT
%
% File:   debug_EncodeUtils.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%==========================================================================
%
% Debug function for ultrasat.api.BaseModel class
% Run by: ultrasat.api.debug_EncodeUtils()
%

function debug_EncodeUtils()
    % Main debug function for EncodeUtils
    debug_base64();
    debug_compression();
    debug_large_data_compression();
end


function debug_base64()
    % Tests Base64 serialization and deserialization.
    disp('Testing Base64 encoding and decoding...');
    Utils = ultrasat.api.EncodeUtils();

    sampleData = struct('Field1', 123, 'Field2', 'Test');
    encoded = Utils.saveObjectToBase64(sampleData);
    disp('Base64 Encoded Data:');
    disp(encoded);

    decoded = Utils.loadObjectFromBase64(encoded);
    disp('Decoded Data:');
    disp(decoded);

    % Validate original and decoded data
    assert(api.ModelBase.cmpstruct(sampleData, decoded), 'Base64 encoding-decoding failed!');
    disp('[PASS] Base64 encoding-decoding validation successful.');
end


function debug_compression()
    % Tests compression and decompression using 7z.
    disp('Testing 7z compression and decompression...');
    Utils = ultrasat.api.EncodeUtils();

    sampleData = struct('Key', 'Value', 'Number', 42);
    compressed = Utils.serializeToBase64_7z(sampleData);
    disp('Compressed Base64 Data:');
    disp(compressed);

    decompressed = Utils.deserializeFromBase64_7z(compressed);
    disp('Decompressed Data:');
    disp(decompressed);

    % Validate original and decompressed data
    assert(api.ModelBase.cmpstruct(sampleData, decompressed), '7z compression-decompression failed!');
    disp('[PASS] 7z compression-decompression validation successful.');
end



function debug_large_data_compression()
    % Tests encoding and decoding of a large dataset (at least 1MB).
    disp('Testing 7z compression and decompression on large data...');
    Utils = ultrasat.api.EncodeUtils();

    % Generate large struct (1MB+ of random data)
    largeData = struct();
    for i = 1:500000  % Create 500,000 key-value pairs
        fieldName = sprintf('Field%d', i);
        largeData.(fieldName) = rand();  % Assign random double
    end

    disp('Original Large Data Size (MB):');
    disp(whos('largeData').bytes / (1024 * 1024));

    % Compress & Encode
    compressedLarge = Utils.serializeToBase64_7z(largeData);
    disp('Compressed Large Data Size (characters):');
    disp(length(compressedLarge));

    % Decode & Decompress
    decompressedLarge = Utils.deserializeFromBase64_7z(compressedLarge);
    disp('Decompressed Large Data Size (MB):');
    disp(whos('decompressedLarge').bytes / (1024 * 1024));

    % Validate original and decompressed data
    assert(api.ModelBase.cmpstruct(largeData, decompressedLarge), 'Large data compression-decompression failed!');
    disp('[PASS] Large data compression-decompression validation successful.');
end
