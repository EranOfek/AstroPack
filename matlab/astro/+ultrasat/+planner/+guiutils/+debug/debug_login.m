%==========================================================================
% Project     : ULTRASAT Planner
% File        : +planner/+guiutils/+debug/debug_login.m
% Author      : Chen Tishler
% Created     : 26/04/2026
% Updated     : 17/06/2026
% Description : Debug login flow via NamespaceManager, UserManager, and MainModule.
%==========================================================================
%
% Run:
%   ultrasat.planner.guiutils.debug.debug_login()
%
% Prerequisites:
%   SOC_PATH    - must point to a SOC deployment with config/services.json
%   SOC_API_KEY - must be set
%   Services namespace_manager and user_manager must be reachable.

function debug_login()
    % Run three-tier login tests and print aggregate pass/fail counts.

    fprintf('\n========================================\n');
    fprintf('debug_login: start\n');
    fprintf('========================================\n\n');

    passed = 0;
    failed = 0;

    % --- Step 1: NamespaceManagerClient ---
    [p, f] = test_namespace_client();
    passed = passed + p;
    failed = failed + f;

    % --- Step 2: UserManagerClient ---
    [p, f] = test_user_client();
    passed = passed + p;
    failed = failed + f;

    % --- Step 3: MainModule.login (GUI path) ---
    [p, f] = test_main_module_login();
    passed = passed + p;
    failed = failed + f;

    fprintf('========================================\n');
    fprintf('debug_login: done  PASSED=%d  FAILED=%d\n', passed, failed);
    fprintf('========================================\n\n');
end


% =========================================================================

function [passed, failed] = test_namespace_client()
    % Verify ClientFactory URL resolution and getNamespaceList response.

    fprintf('--- [1] NamespaceManagerClient ---\n');
    passed = 0;
    failed = 0;

    try
        factory = ultrasat.api.clients.ClientFactory();
        url = factory.getServiceBaseUrl('namespace_manager');
        fprintf('  URL: %s\n', url);

        client = ultrasat.api.clients.NamespaceManagerClient(url);
        response = client.getNamespaceList();

        if isfield(response, 'namespaces') && ~isempty(response.namespaces)
            fprintf('  Namespaces returned: %d\n', numel(response.namespaces));
            try
                t = struct2table(response.namespaces);
                disp(t);
            catch
                disp(response.namespaces);
            end
            fprintf('  display_list:\n');
            for i = 1:numel(response.display_list)
                fprintf('    [%d] %s\n', i, response.display_list{i});
            end
            result_check(true, 'getNamespaceList returned namespaces');
            passed = passed + 1;
        else
            result_check(false, 'getNamespaceList returned empty/no namespaces');
            failed = failed + 1;
        end

    catch ME
        result_check(false, sprintf('NamespaceManagerClient error: %s', ME.message));
        failed = failed + 1;
    end

    fprintf('\n');
end


% =========================================================================

function [passed, failed] = test_user_client()
    % Exercise UserManagerClient login, logout, and wrong-password rejection.

    fprintf('--- [2] UserManagerClient ---\n');
    passed = 0;
    failed = 0;

    try
        factory = ultrasat.api.clients.ClientFactory();
        url = factory.getServiceBaseUrl('user_manager');
        fprintf('  URL: %s\n', url);
        client = ultrasat.api.clients.UserManagerClient(url);
    catch ME
        result_check(false, sprintf('Failed to create UserManagerClient: %s', ME.message));
        failed = failed + 1;
        fprintf('\n');
        return;
    end

    % --- login with correct password
    try
        fprintf('  login chen/123 ... ');
        response = client.login('chen', '123');
        if response.ok
            result_check(true, sprintf('login OK (status=%s)', response.status));
            passed = passed + 1;
        else
            result_check(false, sprintf('login failed (status=%s, message=%s)', ...
                response.status, safe_str(response, 'message')));
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('login threw: %s', ME.message));
        failed = failed + 1;
    end

    % --- logout
    try
        fprintf('  logout chen ... ');
        response = client.logout('chen');
        if response.ok
            result_check(true, 'logout OK');
            passed = passed + 1;
        else
            result_check(false, sprintf('logout failed (status=%s)', response.status));
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('logout threw: %s', ME.message));
        failed = failed + 1;
    end

    % --- login with wrong password (must be rejected)
    try
        fprintf('  login chen/WRONG_PASSWORD ... ');
        response = client.login('chen', 'WRONG_PASSWORD_xyz_99');
        if ~response.ok
            result_check(true, sprintf('correctly rejected (status=%s)', response.status));
            passed = passed + 1;
        else
            result_check(false, '[UNEXPECTED] wrong password was accepted!');
            failed = failed + 1;
        end
    catch ME
        % A thrown exception also counts as rejection
        result_check(true, sprintf('rejected via exception (ok): %s', ME.message));
        passed = passed + 1;
    end

    fprintf('\n');
end


% =========================================================================

function [passed, failed] = test_main_module_login()
    % Exercise MainModule.login/logout using the same path as Login.mlapp.

    fprintf('--- [3] MainModule login/logout (GUI path) ---\n');
    passed = 0;
    failed = 0;

    % Create MainModule - this also calls NamespaceManagerClient internally
    fprintf('  Creating MainModule ...\n');
    try
        mainModule = ultrasat.planner.guiutils.MainModule();
        fprintf('  MainModule created.\n');
    catch ME
        result_check(false, sprintf('MainModule() constructor failed: %s', ME.message));
        failed = failed + 1;
        fprintf('\n');
        return;
    end

    % Verify namespace list was populated
    if isempty(mainModule.NamespaceDisplayList)
        result_check(false, 'NamespaceDisplayList is empty - cannot test login');
        failed = failed + 1;
        fprintf('\n');
        return;
    end

    Namespace = mainModule.NamespaceDisplayList{1};
    fprintf('  Namespaces: %s\n', strjoin(mainModule.NamespaceDisplayList, ' | '));
    fprintf('  Using:      %s\n', Namespace);

    % --- correct credentials
    try
        fprintf('  MainModule.login chen/123 [%s] ... ', Namespace);
        ok = mainModule.login('chen', '123', Namespace);
        if ok
            result_check(true, sprintf('login OK (UserName=%s, NamespaceId=%s)', ...
                mainModule.UserName, mainModule.NamespaceId));
            passed = passed + 1;
        else
            result_check(false, sprintf('login failed (StatusText: %s)', ...
                safe_char(mainModule.StatusText)));
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('login threw: %s', ME.message));
        failed = failed + 1;
    end

    % --- logout after successful login
    try
        fprintf('  MainModule.logout ... ');
        ok = mainModule.logout();
        if ok
            result_check(true, 'logout OK');
            passed = passed + 1;
        else
            result_check(false, 'logout returned false');
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('logout threw: %s', ME.message));
        failed = failed + 1;
    end

    % --- wrong credentials (must be rejected)
    try
        fprintf('  MainModule.login chen/WRONG [%s] ... ', Namespace);
        ok = mainModule.login('chen', 'WRONG_PASSWORD_xyz_99', Namespace);
        if ~ok
            result_check(true, 'correctly rejected');
            passed = passed + 1;
        else
            result_check(false, '[UNEXPECTED] wrong password was accepted by MainModule!');
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('login (wrong pw) threw: %s', ME.message));
        failed = failed + 1;
    end

    % --- logout when not logged in (should be a no-op, return true)
    try
        fprintf('  MainModule.logout (not logged in) ... ');
        ok = mainModule.logout();
        if ok
            result_check(true, 'no-op logout returned true (expected)');
            passed = passed + 1;
        else
            result_check(false, 'no-op logout returned false (unexpected)');
            failed = failed + 1;
        end
    catch ME
        result_check(false, sprintf('no-op logout threw: %s', ME.message));
        failed = failed + 1;
    end

    fprintf('\n');
end


% =========================================================================
% Helpers
% =========================================================================

function result_check(ok, message)
    % Print [PASS] or [FAIL] for a single assertion result.
    if ok
        fprintf('[PASS] %s\n', message);
    else
        fprintf('[FAIL] %s\n', message);
    end
end


function val = safe_str(s, field)
    % Return struct field as char, or '(n/a)' when missing or empty.
    if isfield(s, field) && ~isempty(s.(field))
        v = s.(field);
        if ischar(v)
            val = v;
        else
            val = char(string(v));
        end
    else
        val = '(n/a)';
    end
end


function val = safe_char(v)
    % Coerce value to char for fprintf, using '(empty)' when absent.
    if isempty(v)
        val = '(empty)';
    elseif ischar(v)
        val = v;
    else
        val = char(string(v));
    end
end
