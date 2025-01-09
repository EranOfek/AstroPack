
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
    jsonStr = api.ModelFactoryBase.toJson(sampleStruct);
    disp('JSON Representation of Struct:');
    disp(jsonStr);
    
    % Parse the JSON back into a MATLAB structure for validation
    decodedStruct = jsondecode(jsonStr);
    disp('Decoded Struct from JSON:');
    disp(decodedStruct);
    
    % Validate the output matches the original input
    isEqual = isequal(sampleStruct, decodedStruct);
    if isEqual
        disp('Validation Successful: The decoded struct matches the original.');
    else
        disp('Validation Failed: The decoded struct does not match the original.');
    end
end
