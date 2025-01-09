function debug_ModelBase()
    % Debugging function for the ModelBase class

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
    model = api.ModelBase(sampleData);
    
    % Show the model's data
    disp('Model Data:');
    model.show();
    
    % Convert the model's data to JSON
    jsonStr = model.toJson();
    disp('JSON Representation of Model Data:');
    disp(jsonStr);
    
    % Test the removeEmptyFields method
    cleanedData = api.ModelBase.removeEmptyFields(sampleData);
    disp('Cleaned Data (Empty Fields Removed):');
    disp(cleanedData);
    
    % Test the isoFormat method with the current timestamp
    isoTimestamp = api.ModelBase.isoFormat(datetime('now'));
    disp('ISO Formatted Timestamp:');
    disp(isoTimestamp);
    
    % Verify object data after manipulation
    disp('Updated Model Data after cleaning:');
    model.Data = cleanedData; % Update the model with cleaned data
    model.show();
end
