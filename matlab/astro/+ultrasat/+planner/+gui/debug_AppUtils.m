%==========================================================================
% ULTRASAT Planner
%
% File:   AppUtils.m
% Author: Chen Tishler
% Created: 07/01/2025
% Updated: 02/09/2025
%
%==========================================================================

function debug_AppUtils()
    %DEBUG_APPUTILS Test script for the AppUtils class.
    %   This script tests the various message and dialog functions.
    %   Since these functions require a parent UI Figure, this script:
    %   1. Creates a simple, temporary UI Figure.
    %   2. Creates a mock "MainModule" and "App" structure to satisfy dependencies.
    %   3. Instantiates AppUtils with the mock objects.
    %   4. Calls each dialog function, requiring user interaction.
    %   5. Displays the result of the user's choice in the command window.

    fprintf('============================================\n');
    fprintf('Starting interactive debug script for AppUtils\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('============================================\n\n');
    
    % --- Setup: Create a mock UI environment ---
    fprintf('--- Step 1: Creating a temporary UI Figure for testing ---\n');
    
    % Create a simple figure to act as the parent for dialogs.
    testFig = uifigure('Name', 'AppUtils Test Parent', 'Position', [100 100 400 200]);
    movegui(testFig, 'center'); % Center it on the screen
    
    % Create mock objects that AppUtils expects.
    mockApp.UIFigure = testFig;
    mockApp.MsgBoxApp = []; % Initialize as empty
    mockMainModule.MainApp = mockApp;
    
    % Create an instance of the class to be tested.
    try
        utils = AppUtils(mockMainModule);
        fprintf('  [SUCCESS] AppUtils object created successfully.\n\n');
    catch ME
        fprintf('  [FAIL] Could not create AppUtils object: %s\n', ME.message);
        if isvalid(testFig); delete(testFig); end
        return;
    end
    
    % --- Test Suite: Call each function ---
    % The script will pause at each step for user interaction.
    
    fprintf('--- Step 2: Testing logging ---\n');
    utils.msglog('This is a test log message.');
    fprintf('  [COMPLETE] Check command window for the log message.\n\n');

    % NOTE: Testing msgOk, msgError, msgDebug requires a custom 'MsgBox' app
    % which is not provided. We will skip these and focus on the 'ask' functions
    % that use standard MATLAB dialogs.
    
    fprintf('--- Step 3: Testing askYesNo ---\n');
    fprintf('--> Please click "Yes" or "No" in the dialog box...\n');
    result_yesno = utils.askYesNo('Do you want to proceed?', 'Test Yes/No');
    fprintf('  [RESULT] You clicked: %s\n\n', result_yesno);
    
    fprintf('--- Step 4: Testing askYesNoCancel ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_ync = utils.askYesNoCancel('Do you want to save changes?', 'Test Yes/No/Cancel');
    fprintf('  [RESULT] You clicked: %s\n\n', result_ync);
    
    fprintf('--- Step 5: Testing askSaveDiscard ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_sd = utils.askSaveDiscard('Unsaved changes will be lost.', 'Test Save/Discard');
    fprintf('  [RESULT] You clicked: %s\n\n', result_sd);

    fprintf('--- Step 6: Testing askSaveDiscardCancel ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_sdc = utils.askSaveDiscardCancel('Save changes before closing?', 'Test Save/Discard/Cancel');
    fprintf('  [RESULT] You clicked: %s\n\n', result_sdc);
    
    % --- Cleanup ---
    fprintf('--- Step 7: Cleaning up ---\n');
    if isvalid(testFig)
        delete(testFig);
        fprintf('  [SUCCESS] Temporary UI Figure closed.\n');
    end
    
    fprintf('\n============================================\n');
    fprintf('Debug script for AppUtils finished.\n');
    fprintf('============================================\n');
end
    