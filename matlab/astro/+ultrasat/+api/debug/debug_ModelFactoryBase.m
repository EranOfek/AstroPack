%==========================================================================
% ULTRASAT
%
% File:   debug_ModelFactoryBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%==========================================================================
%
% Debug function for ultrasat.api.ModelFactoryBase class
% Run by: ultrasat.api.debug_ModelFactoryBase()
%

function debug_ModelFactoryBase()
    % Debugging function for the ModelFactoryBase class

    % Create a sample struct to test the factory
    sampleStruct = struct(...
        'Name', 'FactoryModel', ...
        'ID', 101, ...
        'Timestamp', datetime('now'), ...
        'Attributes', struct('Color', 'Blue', 'Size', 42) ...
    );

    % Display the original struct
    disp('Original Struct:');
    disp(sampleStruct);

    % Convert the struct to JSON using the toJson method
    jsonStr = ultrasat.api.ModelBase.struct2json(sampleStruct);
    disp('JSON Representation of Struct:');
    disp(jsonStr);

    % Parse the JSON back into a MATLAB structure for validation
    decodedStruct = jsondecode(jsonStr);
    disp('Decoded Struct from JSON:');
    disp(decodedStruct);

    % Parse the JSON back into a MATLAB structure for validation
    decodedStruct = ultrasat.api.ModelBase.json2struct(jsonStr);
    disp('Decoded Struct from JSON:');
    disp(decodedStruct);

    % Convert the timestamp field back to datetime
    % decodedStruct.Timestamp = datetime(decodedStruct.Timestamp, 'InputFormat', 'dd-MMM-yyyy HH:mm:ss');

    % Validate the output matches the original input
    isEqual = strcmp(jsonencode(sampleStruct), jsonencode(decodedStruct));
    if isEqual
        disp('Validation Successful: The decoded struct matches the original.');
    else
        disp('Validation Failed: The decoded struct does not match the original.');
    end
end
