%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.EncodeUtils.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 05/10/2025
% Description : Utility class for encoding, compression, and serialization.
%==========================================================================


classdef EncodeUtils < ultrasat.api.core.Loggable
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

            base64String = "";
            tempFile = [tempname, '.mat'];

            try
                save(tempFile, 'matObj');
            catch ME
                obj.msglog(sprintf('saveObjectToBase64: failed to save object: %s', ME.message));
                return;
            end

            fid = fopen(tempFile, 'rb');
            if fid == -1
                obj.msglog(sprintf('saveObjectToBase64: failed to open %s for reading', tempFile));
                return;
            end

            try
                binaryData = fread(fid, inf, 'uint8=>uint8');
            catch ME
                obj.msglog(sprintf('saveObjectToBase64: fread failed: %s', ME.message));
                fclose(fid);
                return;
            end

            fclose(fid);

            try
                base64String = matlab.net.base64encode(binaryData);
            catch ME
                obj.msglog(sprintf('saveObjectToBase64: base64encode failed: %s', ME.message));
            end

            try
                delete(tempFile);
            catch ME
                obj.msglog(sprintf('saveObjectToBase64: failed to delete temp file %s: %s', tempFile, ME.message));
            end

            obj.msglog(sprintf('saveObjectToBase64: len=%d', strlength(base64String)));
        end


        function matObj = loadObjectFromBase64(obj, base64String)
            % Deserializes a Base64 string to a MATLAB object.
            %
            % :param base64String: Base64-encoded object string.
            % :return: MATLAB object.

            obj.msglog(sprintf('loadObjectFromBase64: len=%d', strlength(base64String)));
            matObj = [];

            try
                binaryData = matlab.net.base64decode(base64String);
            catch ME
                obj.msglog(sprintf('loadObjectFromBase64: base64decode failed: %s', ME.message));
                return;
            end

            tempFile = [tempname, '.mat'];
            fid = fopen(tempFile, 'wb');
            if fid == -1
                obj.msglog(sprintf('loadObjectFromBase64: failed to open %s for writing', tempFile));
                return;
            end

            try
                fwrite(fid, binaryData);
            catch ME
                obj.msglog(sprintf('loadObjectFromBase64: fwrite failed: %s', ME.message));
                fclose(fid);
                return;
            end

            fclose(fid);

            try
                loadedData = load(tempFile, 'matObj');
                matObj = loadedData.matObj;
            catch ME
                obj.msglog(sprintf('loadObjectFromBase64: load failed: %s', ME.message));
            end

            try
                delete(tempFile);
            catch ME
                obj.msglog(sprintf('loadObjectFromBase64: failed to delete temp file %s: %s', tempFile, ME.message));
            end
        end


        function base64String = serializeToBase64_7z(obj, matObj)
            % Serializes a MATLAB object to a compressed Base64 string.
            %
            % This function converts a MATLAB object to a `.mat` file,
            % compresses it using 7z, and encodes it as a Base64 string.
            % This is useful for sending compact data over the network.
            %
            % :param matObj: MATLAB object to be serialized.
            % :return: Base64-encoded string of the compressed data.

            base64String = "";
            tempMatFile = [tempname, '.mat'];
            compressedFile = "";

            try
                save(tempMatFile, 'matObj');
            catch ME
                obj.msglog(sprintf('serializeToBase64_7z: failed to save object: %s', ME.message));
                return;
            end

            try
                compressedFile = obj.compressWith7z(tempMatFile);
            catch ME
                obj.msglog(sprintf('serializeToBase64_7z: compression failed: %s', ME.message));
                return;
            end

            fid = fopen(compressedFile, 'rb');
            if fid == -1
                obj.msglog(sprintf('serializeToBase64_7z: failed to open %s for reading', compressedFile));
                return;
            end

            try
                binaryData = fread(fid, inf, 'uint8=>uint8');
            catch ME
                obj.msglog(sprintf('serializeToBase64_7z: fread failed: %s', ME.message));
                fclose(fid);
                return;
            end

            fclose(fid);

            try
                base64String = matlab.net.base64encode(binaryData);
            catch ME
                obj.msglog(sprintf('serializeToBase64_7z: base64encode failed: %s', ME.message));
            end

            try
                if exist(tempMatFile, 'file'), delete(tempMatFile); end
                if ~isempty(compressedFile) && exist(compressedFile, 'file'), delete(compressedFile); end
            catch ME
                obj.msglog(sprintf('serializeToBase64_7z: cleanup failed: %s', ME.message));
            end

            obj.msglog(sprintf('serializeToBase64_7z: len=%d', strlength(base64String)));
        end


        function matObj = deserializeFromBase64_7z(obj, base64String)
            % Deserializes a compressed Base64 string to a MATLAB object.
            %
            % This function decodes a Base64 string, decompresses the
            % 7z-compressed `.mat` file, and loads the stored object.
            %
            % :param base64String: Base64-encoded compressed object.
            % :return: MATLAB object restored from the compressed data.

            obj.msglog(sprintf('deserializeFromBase64_7z: len=%d', strlength(base64String)));
            matObj = [];

            try
                binaryData = matlab.net.base64decode(base64String);
            catch ME
                obj.msglog(sprintf('deserializeFromBase64_7z: base64decode failed: %s', ME.message));
                return;
            end

            compressedFile = [tempname, '.7z'];
            fid = fopen(compressedFile, 'wb');
            if fid == -1
                obj.msglog(sprintf('deserializeFromBase64_7z: failed to open %s for writing', compressedFile));
                return;
            end

            try
                fwrite(fid, binaryData);
            catch ME
                obj.msglog(sprintf('deserializeFromBase64_7z: fwrite failed: %s', ME.message));
                fclose(fid);
                return;
            end

            fclose(fid);

            try
                tempMatFile = obj.decompressWith7z(compressedFile);
            catch ME
                obj.msglog(sprintf('deserializeFromBase64_7z: decompression failed: %s', ME.message));
                return;
            end

            try
                loadedData = load(tempMatFile, 'matObj');
                matObj = loadedData.matObj;
            catch ME
                obj.msglog(sprintf('deserializeFromBase64_7z: load failed: %s', ME.message));
            end

            try
                if exist(compressedFile, 'file'), delete(compressedFile); end
                if ~isempty(tempMatFile) && exist(tempMatFile, 'file'), delete(tempMatFile); end
            catch ME
                obj.msglog(sprintf('deserializeFromBase64_7z: cleanup failed: %s', ME.message));
            end
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
                cmd = sprintf('7za a -y "%s" "%s"', compressedFile, inputFile);
            else
                cmd = sprintf('7z a -y "%s" "%s"', compressedFile, inputFile);
            end

            obj.msglog(sprintf('compressWith7z: %s', cmd));

            try
                [status, cmdout] = system(cmd);
            catch ME
                obj.msglog(sprintf('compressWith7z: system call failed: %s', ME.message));
                compressedFile = "";
                return;
            end

            if status ~= 0
                obj.msglog(sprintf('compressWith7z: compression failed: %s', cmdout));
                compressedFile = "";
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

            outputFile = "";
            outputDir = tempname;
            mkdir(outputDir);

            if ispc
                cmd = sprintf('7za x -y -o"%s" "%s"', outputDir, compressedFile);
            else
                cmd = sprintf('7z x -y -o"%s" "%s"', outputDir, compressedFile);
            end

            obj.msglog(sprintf('decompressWith7z: %s', cmd));

            try
                [status, cmdout] = system(cmd);
            catch ME
                obj.msglog(sprintf('decompressWith7z: system call failed: %s', ME.message));
                return;
            end

            if status ~= 0
                obj.msglog(sprintf('decompressWith7z: decompression failed: %s', cmdout));
                return;
            end

            try
                files = dir(fullfile(outputDir, '*.mat'));
                if isempty(files)
                    obj.msglog('decompressWith7z: no MAT file found after decompression');
                    return;
                end
                outputFile = fullfile(outputDir, files(1).name);
            catch ME
                obj.msglog(sprintf('decompressWith7z: failed to list MAT files: %s', ME.message));
            end
        end


    end

end
