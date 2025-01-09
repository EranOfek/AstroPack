%==========================================================================
% ULTRASAT 
%
% File:   ClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 06/01/2025
%
%==========================================================================

classdef ClientBase < handle
    % ClientBase - Base class for interacting with REST API services.
    % https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623
    
    properties
        BaseUrl         % Base URL of the API
        SubUrl          % Service-specific URL path
        ApiUrl          % Base URL of the API
        ApiKey          % API Key for authentication
        Timeout = 30;   % Timeout for HTTP requests (seconds)
    end
    

    methods
        function obj = ClientBase(Args)
            % Constructor for ClientBase using arguments block
        
            % Accept input arguments with defaults
            arguments          
                Args.BaseUrl    = getenv('SOC_API_BASE');       % Default BaseUrl from environment or empty if not set
                Args.SubUrl     = '';                           % Default SubUrl is an empty string
                Args.ApiKey     = getenv('SOC_API_KEY');        % Default ApiKey from environment or empty if not set           
                Args.Timeout    = getenv('SOC_API_TIMEOUT');    % Default Timeout from environment or 30 seconds
            end
        
            % Assign default timeout if environment variable is invalid
            if isempty(Args.Timeout)
                Args.Timeout = getenv('SOC_API_TIMEOUT');
            end
            if isempty(Args.Timeout)
                Args.Timeout = 30; 
            else
                Args.Timeout = str2double(Args.Timeout);
            end
            
            % Assign properties
            obj.BaseUrl = Args.BaseUrl;
            obj.SubUrl = Args.SubUrl;
            obj.ApiKey = Args.ApiKey;
            obj.Timeout = Args.Timeout;
            
            % Construct the full API URL
            if ~isempty(obj.BaseUrl) && ~isempty(obj.SubUrl)
                obj.ApiUrl = [obj.BaseUrl, obj.SubUrl];
            else
                obj.ApiUrl = obj.BaseUrl; % Use BaseUrl alone if SubUrl is empty
            end
        end

        % -------------------------------------------------------------------

        function sendFilesWithParams(obj, url, filePaths, params)
            import matlab.net.*
            import matlab.net.http.*
            import matlab.net.http.io.*
        
            % Create the MultipartFormProvider
            formProvider = MultipartFormProvider();
        
            % Attach each file in the array of file paths
            for i = 1:numel(filePaths)
                % Use a field name like "file1", "file2", etc.
                fieldName = sprintf('file%d', i);
                formProvider.addPart(FormProvider(fieldName, filePaths{i}));
            end
        
            % Add additional parameters if provided
            if nargin > 2 && ~isempty(params)
                fieldNames = fieldnames(params);
                for i = 1:numel(fieldNames)
                    formProvider.addPart(FormProvider(fieldNames{i}, params.(fieldNames{i})));
                end
            end
        
            % Create the HTTP Request
            request = RequestMessage('POST', [], formProvider);
        
            % Send the request
            response = request.send(url);
        
            % Display the response
            disp('Response Status Code:');
            disp(response.StatusCode);
            disp('Response Body:');
            disp(response.Body.Data);
        end
        
        % -------------------------------------------------------------------        

        function response = postRequest(obj, endpoint, params)
            % Sends a POST request to the API
            import matlab.net.*
            import matlab.net.http.*
            
            url = [obj.ApiUrl, endpoint];

            % Converts the Data property to a JSON string, excluding empty fields
            cleanedData = soc.api.ModelBase.removeEmptyFields(params);
            jsonData = jsonencode(cleanedData);

            % Create the HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json'), ...
                HeaderField('x-api-key', obj.ApiKey)  % Add the x-api-key header
            ];

            % Create the HTTP request
            body = MessageBody(cleanedData);
            request = RequestMessage('POST', headers, body);
            
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            % Initialize response
            response = struct();

            try
                rawResponse = send(request, url, options);
                
                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    response = rawResponse.Body.Data;
                    %response = jsondecode(rawResponse.Body.Data);
                else
                    error('HTTP Error: %s', char(rawResponse.StatusCode));
                end
            catch ME
                % Handle exceptions
                switch ME.identifier
                    case 'MATLAB:networklib:ConnectionFailed'
                        error('Connection failed. Please check the server URL or your network connection.');
                    case 'MATLAB:webservices:Timeout'
                        error('Request timed out after %d seconds.', obj.Timeout);
                    case 'MATLAB:webservices:HTTPErrorStatusCode'
                        error('HTTP error: %s. Check your API endpoint or parameters.', ME.message);
                    otherwise
                        % Re-throw unexpected errors
                        rethrow(ME);
                end
            end
        end

        % -------------------------------------------------------------------        

        function postRequestAsync(obj, endpoint, params, callback)
            % Sends an asynchronous POST request to the API
            import matlab.net.*
            import matlab.net.http.*

            url = [obj.ApiUrl, endpoint];
            
            % Clean up input params and convert to JSON
            cleanedData = soc.api.ModelBase.removeEmptyFields(params);
            jsonData = jsonencode(cleanedData);

            % Create the HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json'), ...
                HeaderField('x-api-key', obj.ApiKey)
            ];

            % Create the HTTP request
            body = MessageBody(cleanedData);
            request = RequestMessage('POST', headers, body);

            % Start an asynchronous request using a timer
            t = timer('ExecutionMode', 'singleShot', ...
                      'StartDelay', 0, ...
                      'TimerFcn', @(~,~) asyncSend(request, url, obj.Timeout, callback), ...
                      'StopFcn', @(~,~) delete(timerfind)); % Clean up timer
            start(t);
        end

        % -------------------------------------------------------------------

        function base64String = serializeToBase64(obj, matObj)
            % Create a temporary file
            tempFile = [tempname, '.mat'];
            
            % Save the MATLAB object to the MAT file
            save(tempFile, 'matObj');
            
            % Read the binary content of the file
            fid = fopen(tempFile, 'rb');
            binaryData = fread(fid, inf, 'uint8=>uint8');  % Ensures output is uint8
            fclose(fid);
            
            % Convert binary data to Base64 string
            base64String = matlab.net.base64encode(binaryData);
            
            % Delete the temporary file
            delete(tempFile);

            obj.msglog('serializeFromBase64: len=%d', length(base64String));
        end


        function matObj = deserializeFromBase64(obj, base64String)

            obj.msglog('deserializeFromBase64: len=%d', length(base64String))

            % Decode the Base64 string to binary
            binaryData = matlab.net.base64decode(base64String);
            
            % Create a temporary file
            tempFile = [tempname, '.mat'];
            
            % Write the binary data to the temporary file
            fid = fopen(tempFile, 'wb');
            fwrite(fid, binaryData);
            fclose(fid);
            
            % Load the MATLAB object from the MAT file
            loadedData = load(tempFile, 'matObj');
            matObj = loadedData.matObj;
            
            % Delete the temporary file
            delete(tempFile);
        end
       
        % -------------------------------------------------------------------

        function base64String = serializeToBase64_7z(obj, matObj)
            % Create a temporary file for the .mat file
            tempMatFile = [tempname, '.mat'];
            
            % Save the MATLAB object to the MAT file
            save(tempMatFile, 'matObj');
            
            % Compress the MAT file with 7z
            compressedFile = compressWith7z(tempMatFile);
            
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
            tempMatFile = decompressWith7z(compressedFile);
            
            % Load the MATLAB object from the decompressed MAT file
            loadedData = load(tempMatFile, 'matObj');
            matObj = loadedData.matObj;
            
            % Clean up temporary files
            delete(compressedFile);
            delete(tempMatFile);
        end
        
        % -------------------------------------------------------------------        

        function compressedFile = compressWith7z(obj, inputFile)
            compressedFile = [tempname, '.7z'];
            if ispc
                % Windows command
                cmd = sprintf('7z a -y "%s" "%s"', compressedFile, inputFile);
            else
                % Linux/Unix command
                cmd = sprintf('7z a -y "%s" "%s"', compressedFile, inputFile);
            end
            
            % Execute the compression command
            [status, cmdout] = system(cmd);
            if status ~= 0
                error('Compression failed: %s', cmdout);
            end
        end
        

        function outputFile = decompressWith7z(obj, compressedFile)
            outputDir = tempname; % Create a temporary directory
            mkdir(outputDir);
            
            if ispc
                % Windows command
                cmd = sprintf('7z x -y -o"%s" "%s"', outputDir, compressedFile);
            else
                % Linux/Unix command
                cmd = sprintf('7z x -y -o"%s" "%s"', outputDir, compressedFile);
            end
            
            % Execute the decompression command
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

        % -------------------------------------------------------------------

        function msglog(obj, varargin)
            %
            fprintf('Client: ');
            fprintf(varargin{:});
            fprintf('\n');
        end



    end


    % -------------------------------------------------------------------

    methods (Static)


    end

end
