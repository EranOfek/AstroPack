%==========================================================================
% ULTRASAT
%
% File:   debug_ModelBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%==========================================================================
%
% Debug function for ultrasat.api.BaseModel class
% Run by: ultrasat.api.debug_ModelBase()
%

function debug_ModelBase()
    % Debugging function for the ModelBase class

    debug_to_from_struct();

    % Create a sample struct to initialize the ModelBase object
    sampleData = struct(...
        'Name', 'SampleModel', ...
        'Value', 42, ...
        'Timestamp', datetime('now'), ...
        'Nested', struct('Field1', [], 'Field2', 'NestedValue') ...
    );

    % Display the initial sample data
    disp('Original Data:');
    disp(sampleData);

    % Instantiate the ModelBase object
    model = ultrasat.api.utils.ModelBase(sampleData);

    % Show the model's data
    disp('Model Data:');
    model.show();

    % Convert the model's data to JSON
    jsonStr = model.toJson();
    disp('JSON Representation of Model Data:');
    disp(jsonStr);

    % Test the removeEmptyFields method
    cleanedData = ultrasat.api.utils.ModelBase.removeEmptyFields(sampleData);
    disp('Cleaned Data (Empty Fields Removed):');
    disp(cleanedData);

    % Test the isoFormat method with the current timestamp
    isoTimestamp = ultrasat.api.utils.ModelBase.isoFormat(datetime('now'));
    disp('ISO Formatted Timestamp:');
    disp(isoTimestamp);

    % Verify object data after manipulation
    disp('Updated Model Data after cleaning:');
    model.Data = cleanedData; % Update the model with cleaned data
    model.show();
end


function debug_to_from_struct()
    % Create an instance of TestModel
    originalObj = ultrasat.api.debug.debug_ModelBase_MyClass();

    % Convert class to struct
    s = ultrasat.api.utils.ModelBase.class2struct(originalObj);
    disp('Converted to struct:');
    disp(s);
    disp(jsonencode(s));

    % Convert struct back to class
    reconstructedObj = ultrasat.api.utils.ModelBase.struct2class(s, 'ultrasat.api.debug.debug_ModelBase_MyClass');
    disp('Reconstructed object:');
    disp(reconstructedObj);

    % Verify if the properties match
    assert(isequal(originalObj.id, reconstructedObj.id), 'ID does not match');
    assert(isequal(originalObj.name, reconstructedObj.name), 'Name does not match');
    assert(isequal(originalObj.values, reconstructedObj.values), 'Values do not match');

    disp('Test Passed: Original and Reconstructed Objects Match!');

end

