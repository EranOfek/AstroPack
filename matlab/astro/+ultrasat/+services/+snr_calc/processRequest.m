%==========================================================================
% File Name   : processSnrJson.m
% Project     : ULTRASAT Science Operations Center (SOC)
% Subsystem   : WebApp SNR Service
% Description : 
%   This MATLAB module implements the SNR calculation service logic 
%   used by the ULTRASAT web-based SNR calculator.
%
%   It serves as the backend computation layer that receives JSON-encoded
%   requests from the Python FastAPI REST server
%   (see: ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py)
%   and returns JSON-encoded responses.
%
%   The service uses an external calculator object (UltrasatPerf2GUI) to 
%   compute the signal-to-noise ratio (SNR) and limiting magnitude 
%   for a given observational configuration.
%
% Architecture:
%   - The Python FastAPI layer writes incoming requests as JSON files.
%   - The MATLAB listener detects and processes these requests.
%   - Input JSON text is decoded, processed, and the result encoded 
%     back into an escaped JSON string inside a standard service envelope.
%
% Input  :
%   json_text : (char) JSON-encoded request string with fields:
%       ExpTime
%       NumImages
%       R
%       Source
%       PicklesModels
%       SnrMagnitude
%       CalibFilterFamily
%       CalibFilter
%       MagnitudeSystem
%       LimitingMagnitude
%
% Output :
%   Result : struct with the following fields:
%       message   - Status or error message text
%       result    - Numeric code (0 = success, negative = error)
%       json_text - Escaped JSON string containing the internal result:
%           {
%             "ResultSnr": <float>,
%             "ResultLimitingMagnitude": <float>
%           }
%
% Design Notes :
%   - This service preserves the "nested JSON" envelope design introduced
%     in 2022 to ensure robust inter-process communication between
%     heterogeneous components (Delphi, Python, MATLAB).
%   - The outer layer (ResponseMessage) ensures the service can always 
%     return a valid message and result code even if the computation fails.
%   - The inner JSON (json_text) carries the detailed calculator results.
%
% Dependencies :
%   - UltrasatPerf2GUI.m
%   - namedargs2cell.m
%   - io.msgLog / LogLevel
%
% Author  : Chen Tishler
% Created : 11/12/2022
% Updated : 02/11/2025
%==========================================================================

function Result = processSnrJson(json_text)
    % Process SNR
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    %    
    % Input   : - snr - struct 
    % 
    %                      
    % Output  : struct ResponseMessage with fields: message, result
    % Author  : Chen Tishler (2022)
    % Example : 
    
    % Decode text
    snr_input = jsondecode(json_text);
            
    out = struct;
    out.message = sprintf('MATLAB: processSnr started');
    out.result = -1;
    out.json_text = '';
    
    % Do the actual SNR processing here
    [snr_out, message] = doProcessSnr(snr_input);
    
    % Done
    out.message = message;
    snr_out.message = '';
    out.result = 0;    
    out.json_text = jsonencode(snr_out);
    out.json_text = strrep(out.json_text, '"', '\"');
    Result = out;
end

%------------------------------------------------------------------------

function [Result, Message] = doProcessSnr(Params)
    % Process SNR
    % See ultrasat.git/python/prj/src/webapps/webapp_snr/rest_snr_server1.py
    
    % Input   : - Params - struct with these fields:
	%
    %   ExpTime
    %   NumImages
    %   R
    %   Source
    %   PicklesModels
    %   SnrMagnitude
    %   CalibFilterFamily
    %   CalibFilter
    %   MagnitudeSystem
    %   LimitingMagnitude
    %    
    %                      
    % Output  : - Result - struct with fields: 
    %   ResultSnr
    %   ResultLimitingMagnitude
    %           - Message - char with text message
    %
    % Author  : Arie B. (2023)
    % Example : 

    io.msgLog(LogLevel.Debug, 'doProcessSnr: started - Params:');
    disp(Params);
 
    % Calculate
    try
        if strcmp(Params.Source, 'PicklesModels')
            Params.Source = Params.PicklesModels;
            Params = rmfield(Params, 'PicklesModels');
        end
        
        if strcmp(Params.Source, 'BlackBody')
            Params.Source = strcat('Planck spectrum T=', Params.BlackBodyTemperature, '.000000');
            Params = rmfield(Params, 'BlackBodyTemperature');
        end
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: creating UltrasatPerf2GUI');
        UsatPerf2GUI = UltrasatPerf2GUI();
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling namedargs2cell');
        ArgsCell = namedargs2cell(Params);
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling calcSNR');
        Result = UsatPerf2GUI.calcSNR(ArgsCell{:});
        
        io.msgLog(LogLevel.Debug, 'doProcessSnr: calling calcSNR done');
    catch ex
        Result.message = sprintf("doProcessSnr: error: UG threw exception identifier='%s' with message='%s'", ex.identifier, ex.message);
    end

    %
    disp(Result);
    Message = Result.message;
    Result = rmfield(Result, 'message');
    
    io.msgLog(LogLevel.Debug, 'doProcessSnr: done');    
end

