%==========================================================================
% ULTRASAT 
%
% File:   EncodeUtils.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%
%==========================================================================

classdef EncodeUtils < handle
    % EncodeUtils - Utility class for encoding, compression, and serialization.
    %
    % This class provides utility functions for handling data serialization 
    % and compression in MATLAB. It includes methods for:
    %
    % - **Base64 encoding & decoding**: Converts MATLAB objects to/from Base64 strings.
    % - **7z compression & decompression**: Compresses and extracts `.mat` files using 7z.
    % - **Logging**: Provides a simple message logging function.
    %
    % The methods in this class allow efficient data storage, transmission, 
    % and retrieval, making it useful for APIs, cloud storage, and inter-process 
    % communication.
    %
    % Supports **Base64 serialization** for compact storage & transfer.
    % Uses **7z compression** for reducing file size.
    %    
    % ---------------------------------------------------------------------
    % Typical Usage:
    % 
    % % Base64 Encoding & Decoding
    % data = struct('Field1', 123, 'Field2', 'Test');
    % encoded = ultrasat.api.EncodeUtils().saveObjectToBase64(data);
    % decoded = ultrasat.api.EncodeUtils().loadObjectFromBase64(encoded);
    % assert(api.ModelBase.cmpstruct(data, decoded)); % Validate integrity
    %
    % % 7z Compression & Decompression
    % compressed = ultrasat.api.EncodeUtils().serializeToBase64_7z(data);
    % decompressed = ultrasat.api.EncodeUtils().deserializeFromBase64_7z(compressed);
    % assert(api.ModelBase.cmpstruct(data, decompressed)); % Validate integrity
    % 
    % See Also: jsonencode, jsondecode, base64encode, base64decode
    
    methods
        function obj = EncodeUtils()
            % Constructor for EncodeUtils
        end

        
        function base64String = saveObjectToBase64(obj, matObj)
            % Serializes a MATLAB object to a Base64 string.
            %
            % :param matObj: MATLAB object to serialize.
            % :return: Base64-encoded string.

            tempFile = [tempname, '.mat'];
            save(tempFile, 'matObj');
            
            fid = fopen(tempFile, 'rb');
            binaryData = fread(fid, inf, 'uint8=>uint8');
            fclose(fid);
            
            base64String = matlab.net.base64encode(binaryData);
            delete(tempFile);

            obj.msglog('saveObjectToBase64: len=%d', length(base64String));
        end


        function matObj = loadObjectFromBase64(obj, base64String)
            % Deserializes a Base64 string to a MATLAB object.
            %
            % :param base64String: Base64-encoded object string.
            % :return: MATLAB object.

            obj.msglog('loadObjectFromBase64: len=%d', length(base64String));

            binaryData = matlab.net.base64decode(base64String);
            tempFile = [tempname, '.mat'];
            
            fid = fopen(tempFile, 'wb');
            fwrite(fid, binaryData);
            fclose(fid);
            
            loadedData = load(tempFile, 'matObj');
            matObj = loadedData.matObj;
            delete(tempFile);
        end

        % -----------------------------------------------------------------

        function base64String = serializeToBase64_7z(obj, matObj)
            % Serializes a MATLAB object to a compressed Base64 string.
            %
            % This function converts a MATLAB object to a `.mat` file, 
            % compresses it using 7z, and encodes it as a Base64 string. 
            % This is useful for sending compact data over the network.
            %
            % :param matObj: MATLAB object to be serialized.
            % :return: Base64-encoded string of the compressed data.

            % Create a temporary file for the .mat file
            tempMatFile = [tempname, '.mat'];
            save(tempMatFile, 'matObj');
            
            % Compress the MAT file with 7z
            compressedFile = obj.compressWith7z(tempMatFile);
            
            % Read the compressed file as binary
            fid = fopen(compressedFile, 'rb');
            binaryData = fread(fid, inf, 'uint8=>uint8');  % Read as uint8
            fclose(fid);
            
            % Convert the binary data to Base64
            base64String = matlab.net.base64encode(binaryData);
            
            % Clean up temporary files
            delete(tempMatFile);
            delete(compressedFile);
        
            obj.msglog('serializeToBase64: len=%d', length(base64String));
        end
        

        function matObj = deserializeFromBase64_7z(obj, base64String)
            % Deserializes a compressed Base64 string to a MATLAB object.
            %
            % This function decodes a Base64 string, decompresses the 
            % 7z-compressed `.mat` file, and loads the stored object.
            %
            % :param base64String: Base64-encoded compressed object.
            % :return: MATLAB object restored from the compressed data.

            obj.msglog('deserializeFromBase64: len=%d', length(base64String));
            
            % Decode the Base64 string to binary
            binaryData = matlab.net.base64decode(base64String);
            
            % Create a temporary file for the compressed data
            compressedFile = [tempname, '.7z'];
            
            % Write the binary data to the compressed file
            fid = fopen(compressedFile, 'wb');
            fwrite(fid, binaryData);
            fclose(fid);
            
            % Decompress the file with 7z
            tempMatFile = obj.decompressWith7z(compressedFile);
            
            % Load the MATLAB object from the decompressed MAT file
            loadedData = load(tempMatFile, 'matObj');
            matObj = loadedData.matObj;
            
            % Clean up temporary files
            delete(compressedFile);
            delete(tempMatFile);
        end
        

        function compressedFile = compressWith7z(obj, inputFile)
            % Compresses a file using 7z.
            %
            % Uses the system-installed 7z utility to create a compressed
            % archive of the given file.
            %
            % :param inputFile: Path to the file to be compressed.
            % :return: Path to the compressed `.7z` file.

            compressedFile = [tempname, '.7z'];
            if ispc
                % Windows command
                cmd = sprintf('7za a -y "%s" "%s"', compressedFile, inputFile);
            else
                % Linux/Unix command
                cmd = sprintf('7z a -y "%s" "%s"', compressedFile, inputFile);
            end
            
            % Execute the compression command
            obj.msglog('compressWith7z: %s', cmd);
            [status, cmdout] = system(cmd);
            if status ~= 0
                error('Compression failed: %s', cmdout);
            end
        end
        

        function outputFile = decompressWith7z(obj, compressedFile)
            % Decompresses a 7z archive and extracts a `.mat` file.
            %
            % This function extracts a `.mat` file from a 7z-compressed
            % archive and returns its path.
            %
            % :param compressedFile: Path to the `.7z` file to decompress.
            % :return: Path to the extracted `.mat` file.

            outputDir = tempname; % Create a temporary directory
            mkdir(outputDir);
            
            if ispc
                % Windows command
                cmd = sprintf('7za x -y -o"%s" "%s"', outputDir, compressedFile);
            else
                % Linux/Unix command
                cmd = sprintf('7z x -y -o"%s" "%s"', outputDir, compressedFile);
            end
            
            % Execute the decompression command
            obj.msglog('decompressWith7z: %s', cmd);
            [status, cmdout] = system(cmd);
            if status ~= 0
                error('Decompression failed: %s', cmdout);
            end
            
            % Find the extracted file
            files = dir(fullfile(outputDir, '*.mat'));
            if isempty(files)
                error('No MAT file found after decompression.');
            end
            
            % Return the path to the decompressed MAT file
            outputFile = fullfile(outputDir, files(1).name);
        end

        % -----------------------------------------------------------------

        function msglog(obj, varargin)
            % Logs a formatted message to the console.
            %
            % :param varargin: Formatted message arguments.
            
            fprintf('EncodeUtils: ');
            fprintf(varargin{:});
            fprintf('\n');
        end

    end

end
