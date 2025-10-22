function debug_PathUtils()
    % debug_PathUtils - A comprehensive test script for the PathUtils class.

    import ultrasat.api.PathUtils;

    fprintf('--- Testing PathUtils Class ---\n\n');

    % --- Setup ---
    % Use a temporary directory for testing to avoid cluttering real paths.
    basePath = fullfile('c:/soc/debug', 'matlab_PathUtils');
    if exist(basePath, 'dir')
        %rmdir(basePath, 's'); % Clean up from previous runs
    end
    mkdir(basePath);

    fprintf('1. SETUP: Configuring static properties...\n');
    PathUtils.setBasePath(basePath);

    PathUtils.setNamespaceId('SIM1'); % Set a test namespace

    % Create a specific datetime for predictable results in daily functions
    testDate = datetime(2025, 12, 30, 10, 0, 0, 'TimeZone', 'UTC');

    fprintf('   Base Path set to: %s\n', PathUtils.BasePath);
    fprintf('   NamespaceId set to: %s\n', PathUtils.NamespaceId);
    fprintf('   Test Date set to: %s\n\n', string(testDate));

    % --- Test Folder Methods ---
    fprintf('2. TESTING FOLDER METHODS...\n');
    disp('  a) getGlobalDataFolder(module, subfolder):');
    disp(PathUtils.getGlobalDataFolder('mission', 'images'));

    disp('  b) getNamespaceDataFolder(module, subfolder) -> uses default NS "SIM1":');
    disp(PathUtils.getNamespaceDataFolder('mission', 'products'));

    disp('  c) getNamespaceDataFolder(..., NamespaceId=...) -> override NS:');
    disp(PathUtils.getNamespaceDataFolder('mission', 'products', NamespaceId='TEST'));

    disp('  d) getGlobalDailyDataFolder(..., DT=...):');
    disp(PathUtils.getGlobalDailyDataFolder('telemetry', 'raw', DT=testDate));

    disp('  e) getNamespaceDailyDataFolder(..., DT=...):');
    disp(PathUtils.getNamespaceDailyDataFolder('telemetry', 'processed', DT=testDate));
    fprintf('\n');

    % --- Test Filename Methods ---
    fprintf('3. TESTING FILENAME METHODS...\n');
    disp('  a) getGlobalDataFilename(module, subfolder, file):');
    disp(PathUtils.getGlobalDataFilename('config', '', 'settings.json'));

    disp('  b) getNamespaceDataFilename(module, subfolder, file):');
    disp(PathUtils.getNamespaceDataFilename('planning', 'targets', 'schedule.csv'));

    disp('  c) getGlobalDailyDataFilename(..., IncludeTimestampInFilename=true):');
    disp(PathUtils.getGlobalDailyDataFilename('images', 'raw', 'frame.fits', ...
                                                           DT=testDate, IncludeTimestampInFilename=true));

    disp('  d) getGlobalDailyDataFilename(..., IncludeTimestampInFilename=false):');
    disp(PathUtils.getGlobalDailyDataFilename('images', 'raw', 'frame.fits', ...
                                                           DT=testDate, IncludeTimestampInFilename=false));

    disp('  e) getNamespaceDailyDataFilename(..., IncludeTimestampInFilename=true):');
    disp(PathUtils.getNamespaceDailyDataFilename('images', 'processed', 'calibrated.fits', ...
                                                              DT=testDate, IncludeTimestampInFilename=true));
    fprintf('\n');

    % --- Test Log Filename Methods ---
    fprintf('4. TESTING LOG FILENAME METHODS...\n');
    disp('  a) getGlobalLogFilename(module, file) -> uses current date:');
    disp(PathUtils.getGlobalLogFilename('scheduler', 'run_summary')); % Note: date will be today's date

    disp('  b) getNamespaceLogFilename(module, file, DT=...):');
    disp(PathUtils.getNamespaceLogFilename('processor', 'image_proc.log', DT=testDate));
    fprintf('\n');

    % --- Test Default Namespace Fallback ---
    fprintf('5. TESTING DEFAULT NAMESPACE FALLBACK...\n');
    PathUtils.setNamespaceId(''); % Unset the static property
    fprintf('   NamespaceId cleared. Now calling namespace functions...\n');

    disp('  a) getNamespaceDataFolder -> should fall back to "OPER":');
    disp(PathUtils.getNamespaceDataFolder('final', 'reports'));

    disp('  b) getNamespaceLogFilename -> should fall back to "OPER":');
    disp(PathUtils.getNamespaceLogFilename('housekeeping', 'health.log', DT=testDate));
    fprintf('\n');

    fprintf('--- Path Utilities tests complete ---\n');
end
