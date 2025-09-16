function debug_PathUtils()
    % debug_PathUtils - Tests the path utilities.
    fprintf('--- Testing Path Utilities ---\n');

    % Set up test base path and namespace
    ultrasat.api.PathUtils.setBasePath('/tmp/ultrasat');
    ultrasat.api.PathUtils.setNamespaceId('SIM1');

    % Test combinations: with/without module, with/without namespace

    % 1. getGlobalDataPath
    fprintf('Testing getGlobalDataPath...\n');
    disp('  No module:');
    disp(ultrasat.api.PathUtils.getGlobalDataPath());
    disp('  With module:');
    disp(ultrasat.api.PathUtils.getGlobalDataPath('mod1'));
    disp('  With module and relativePath:');
    disp(ultrasat.api.PathUtils.getGlobalDataPath('mod1', 'file1.txt'));
    disp('  No module, with relativePath:');
    disp(ultrasat.api.PathUtils.getGlobalDataPath('', 'file2.txt'));

    % 2. getNamespaceDataPath
    fprintf('Testing getNamespaceDataPath...\n');
    disp('  No module:');
    disp(ultrasat.api.PathUtils.getNamespaceDataPath());
    disp('  With module:');
    disp(ultrasat.api.PathUtils.getNamespaceDataPath('mod2'));
    disp('  With module and relativePath:');
    disp(ultrasat.api.PathUtils.getNamespaceDataPath('mod2', 'file3.txt'));
    disp('  No module, with relativePath:');
    disp(ultrasat.api.PathUtils.getNamespaceDataPath('', 'file4.txt'));

    % 3. getGlobalLogPath
    fprintf('Testing getGlobalLogPath...\n');
    disp('  No module:');
    disp(ultrasat.api.PathUtils.getGlobalLogPath());
    disp('  With module:');
    disp(ultrasat.api.PathUtils.getGlobalLogPath('mod3'));
    disp('  With module and relativePath:');
    disp(ultrasat.api.PathUtils.getGlobalLogPath('mod3', 'log1.txt'));
    disp('  No module, with relativePath:');
    disp(ultrasat.api.PathUtils.getGlobalLogPath('', 'log2.txt'));

    % 4. getNamespaceLogPath
    fprintf('Testing getNamespaceLogPath...\n');
    disp('  No module:');
    disp(ultrasat.api.PathUtils.getNamespaceLogPath());
    disp('  With module:');
    disp(ultrasat.api.PathUtils.getNamespaceLogPath('mod4'));
    disp('  With module and fileName:');
    disp(ultrasat.api.PathUtils.getNamespaceLogPath('mod4', 'log3.txt'));
    disp('  No module, with fileName:');
    disp(ultrasat.api.PathUtils.getNamespaceLogPath('', 'log4.txt'));

    % Test with namespace unset (should use default)
    ultrasat.api.PathUtils.setNamespaceId('');
    fprintf('Testing getNamespaceDataPath with default namespace...\n');
    disp(ultrasat.api.PathUtils.getNamespaceDataPath('mod5', 'file5.txt'));

    fprintf('Testing getNamespaceLogPath with default namespace...\n');
    disp(ultrasat.api.PathUtils.getNamespaceLogPath('mod6', 'log5.txt'));

    fprintf('--- Path Utilities tests complete ---\n');
    fprintf('----------------------------------------\n\n');
end

