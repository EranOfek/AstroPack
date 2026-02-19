function debug_MainModule()

    fprintf('Testing MainModule class...\n');

    % Create an instance of MainModule
    mainModule = ultrasat.planner.guiutils.MainModule();
    fprintf('MainModule instance created successfully.\n');

    % Debug login
    debug_login(mainModule);
end


function debug_login(mainModule)

    % Display the NamespaceDisplayList
    disp('Available namespaces:');
    disp(mainModule.NamespaceDisplayList);

    % Try logging in with correct password ("1234" assumed demo pass)
    fprintf('Trying to login with correct password...\n');
    ok = mainModule.login('chen', '123', mainModule.NamespaceDisplayList{1});
    if ok
        disp('Login with correct password succeeded.');
    else
        disp('Login with correct password failed.');
    end

    % Try logging out
    fprintf('Trying to logout...\n');
    logoutResult = mainModule.logout();
    if logoutResult
        disp('Logout succeeded.');
    else
        disp('Logout failed.');
    end

    % Try logging in with a wrong password
    fprintf('Trying to login with WRONG password...\n');
    ok_wrong = mainModule.login('chen', 'wrong_pass', mainModule.NamespaceDisplayList{1});
    if ok_wrong
        disp('Login with WRONG password succeeded (should not happen).');
    else
        disp('Login with WRONG password failed (expected).');
    end

    % Try logging out again
    fprintf('Trying to logout after failed login...\n');
    logoutResult2 = mainModule.logout();
    if logoutResult2
        disp('Logout (after failed login) succeeded.');
    else
        disp('Logout (after failed login) failed.');
    end

end

