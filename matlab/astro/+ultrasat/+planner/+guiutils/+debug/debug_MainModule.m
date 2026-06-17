%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/+debug/debug_MainModule.m
% Author      : Chen Tishler
% Created     : 07/01/2025
% Updated     : 17/06/2026
% Description : Debug MainModule construction and interactive login/logout.
%==========================================================================

function debug_MainModule()
    % Construct MainModule and run login/logout smoke tests.

    fprintf('Testing MainModule class...\n');

    % --- Step 1: Construct MainModule ---
    mainModule = ultrasat.planner.guiutils.MainModule();
    fprintf('MainModule instance created successfully.\n');

    % --- Step 2: Login/logout smoke tests ---
    debug_login(mainModule);
end


function debug_login(mainModule)
    % Exercise correct login, logout, wrong password, and no-op logout paths.

    % --- Step 2a: List available namespaces ---
    disp('Available namespaces:');
    disp(mainModule.NamespaceDisplayList);

    % --- Step 2b: Login with correct credentials ---
    fprintf('Trying to login with correct password...\n');
    ok = mainModule.login('chen', '123', mainModule.NamespaceDisplayList{1});
    if ok
        disp('Login with correct password succeeded.');
    else
        disp('Login with correct password failed.');
    end

    % --- Step 2c: Logout after successful login ---
    fprintf('Trying to logout...\n');
    logoutResult = mainModule.logout();
    if logoutResult
        disp('Logout succeeded.');
    else
        disp('Logout failed.');
    end

    % --- Step 2d: Reject wrong password ---
    fprintf('Trying to login with WRONG password...\n');
    ok_wrong = mainModule.login('chen', 'wrong_pass', mainModule.NamespaceDisplayList{1});
    if ok_wrong
        disp('Login with WRONG password succeeded (should not happen).');
    else
        disp('Login with WRONG password failed (expected).');
    end

    % --- Step 2e: Logout when not logged in ---
    fprintf('Trying to logout after failed login...\n');
    logoutResult2 = mainModule.logout();
    if logoutResult2
        disp('Logout (after failed login) succeeded.');
    else
        disp('Logout (after failed login) failed.');
    end

end
