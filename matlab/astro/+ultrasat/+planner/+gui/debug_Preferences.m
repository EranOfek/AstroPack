%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/Preferences.m
% Author:  Chen Tishler
% Created: 20/01/2025
% Updated: 20/03/2025
% Title:   
%==========================================================================

function debug_Preferences()
    % Test script for the Preferences class.
    %   This script tests the save and load functionality by:
    %   1. Creating a Preferences object and populating its properties.
    %   2. Saving the object's state to a temporary JSON file.
    %   3. Creating a new, empty Preferences object.
    %   4. Loading the state from the JSON file into the new object.
    %   5. Comparing the properties to ensure they match.

    fprintf('============================================\n');
    fprintf('Starting debug script for Preferences class\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('============================================\n\n');

    % --- Setup ---
    % Define a temporary file for the preferences.
    prefsFilePath = fullfile(tempdir, 'user_preferences_test.json');
    fprintf('Using temporary preferences file: %s\n\n', prefsFilePath);
    
    % --- Test 1: Create and Save Preferences ---
    fprintf('--- Step 1: Creating and populating a Preferences object ---\n');
    
    % Create the first instance.
    prefs_to_save = Preferences(prefsFilePath);
    
    % Populate with sample data.
    prefs_to_save.UserName = 'TestUser';
    prefs_to_save.UniqueTargetsFileName = 'targets.csv';
    prefs_to_save.UniqueTargetsFolder = 'C:\soc\data\targets';
    prefs_to_save.LocalPlanFileName = 'my_plan.json';
    prefs_to_save.LocalPlanFolder = 'C:\soc\plans\local';

    fprintf('Object properties to be saved:\n');
    disp(prefs_to_save);
    
    fprintf('--- Step 2: Saving preferences to JSON file ---\n');
    try
        prefs_to_save.save();
        fprintf('  [SUCCESS] Save method executed without errors.\n');
        
        % Verify file was created
        if isfile(prefsFilePath)
            fprintf('  [SUCCESS] Preferences file was created.\n');
            % Optional: Display file content for verification
            fprintf('--- File Content ---\n');
            disp(fileread(prefsFilePath));
            fprintf('--------------------\n');
        else
            fprintf('  [FAIL] Preferences file was NOT created.\n');
            return;
        end
    catch ME
        fprintf('  [FAIL] An error occurred during save: %s\n', ME.message);
        return;
    end

    % --- Test 2: Load Preferences ---
    fprintf('\n--- Step 3: Creating a new, empty Preferences object ---\n');
    
    % Create a second, empty instance to load data into.
    prefs_to_load = Preferences(prefsFilePath);
    fprintf('New object properties before loading:\n');
    disp(prefs_to_load);

    fprintf('--- Step 4: Loading preferences from JSON file ---\n');
    try
        prefs_to_load.load();
        fprintf('  [SUCCESS] Load method executed without errors.\n');
    catch ME
        fprintf('  [FAIL] An error occurred during load: %s\n', ME.message);
        cleanup(prefsFilePath); % Clean up before exiting
        return;
    end

    fprintf('Object properties after loading:\n');
    disp(prefs_to_load);

    % --- Test 3: Verification ---
    fprintf('\n--- Step 5: Verifying loaded data ---\n');
    
    is_match = strcmp(prefs_to_save.UserName, prefs_to_load.UserName) && ...
                strcmp(prefs_to_save.UniqueTargetsFolder, prefs_to_load.UniqueTargetsFolder) && ...
                strcmp(prefs_to_save.LocalPlanFileName, prefs_to_load.LocalPlanFileName);

    if is_match
        fprintf('  [SUCCESS] Loaded properties match the original saved properties.\n');
    else
        fprintf('  [FAIL] Loaded properties DO NOT match the original properties.\n');
    end

    % --- Cleanup ---
    cleanup(prefsFilePath);
    
    fprintf('\n============================================\n');
    fprintf('Debug script for Preferences finished.\n');
    fprintf('============================================\n');
end


function cleanup(filePath)
    % Deletes the temporary file.
    fprintf('\n--- Cleaning up temporary file ---\n');
    if isfile(filePath)
        delete(filePath);
        fprintf('Deleted: %s\n', filePath);
    end
end
