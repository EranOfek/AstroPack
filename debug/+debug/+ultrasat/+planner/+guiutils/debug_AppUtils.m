%==========================================================================
% Project     : ULTRASAT Planner
% Filename    : +debug/+ultrasat/+planner/+guiutils/debug_AppUtils.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 17/06/2026
% Description : Interactive debug script for AppUtils dialog and log helpers.
%
% Run by      : debug.ultrasat.planner.guiutils.debug_AppUtils()
%==========================================================================

function debug_AppUtils()
    % Exercise AppUtils logging and ask* dialog helpers with a mock UI parent.

    fprintf('============================================\n');
    fprintf('Starting interactive debug script for AppUtils\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('============================================\n\n');
    
    % --- Step 1: Create mock UI environment ---
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
    
    % --- Step 2: Logging ---
    fprintf('--- Step 2: Testing logging ---\n');
    utils.msglog('This is a test log message.');
    fprintf('  [COMPLETE] Check command window for the log message.\n\n');

    % NOTE: Testing msgOk, msgError, msgDebug requires a custom 'MsgBox' app
    % which is not provided. We will skip these and focus on the 'ask' functions
    % that use standard MATLAB dialogs.
    
    % --- Step 3: Yes/No dialog ---
    fprintf('--- Step 3: Testing askYesNo ---\n');
    fprintf('--> Please click "Yes" or "No" in the dialog box...\n');
    result_yesno = utils.askYesNo('Do you want to proceed?', 'Test Yes/No');
    fprintf('  [RESULT] You clicked: %s\n\n', result_yesno);
    
    % --- Step 4: Yes/No/Cancel dialog ---
    fprintf('--- Step 4: Testing askYesNoCancel ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_ync = utils.askYesNoCancel('Do you want to save changes?', 'Test Yes/No/Cancel');
    fprintf('  [RESULT] You clicked: %s\n\n', result_ync);
    
    % --- Step 5: Save/Discard dialog ---
    fprintf('--- Step 5: Testing askSaveDiscard ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_sd = utils.askSaveDiscard('Unsaved changes will be lost.', 'Test Save/Discard');
    fprintf('  [RESULT] You clicked: %s\n\n', result_sd);

    % --- Step 6: Save/Discard/Cancel dialog ---
    fprintf('--- Step 6: Testing askSaveDiscardCancel ---\n');
    fprintf('--> Please click a button in the dialog box...\n');
    result_sdc = utils.askSaveDiscardCancel('Save changes before closing?', 'Test Save/Discard/Cancel');
    fprintf('  [RESULT] You clicked: %s\n\n', result_sdc);
    
    % --- Step 7: Cleanup ---
    fprintf('--- Step 7: Cleaning up ---\n');
    if isvalid(testFig)
        delete(testFig);
        fprintf('  [SUCCESS] Temporary UI Figure closed.\n');
    end
    
    fprintf('\n============================================\n');
    fprintf('Debug script for AppUtils finished.\n');
    fprintf('============================================\n');
end
    
