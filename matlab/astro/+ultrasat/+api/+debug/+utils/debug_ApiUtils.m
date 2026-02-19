%==========================================================================
% ULTRASAT
%
% File:   debug_ApiUtils.m
% Author: Chen Tishler
% Created: 20/02/2025
% Updated: 20/02/2025
%==========================================================================
%
% Debug function for ultrasat.api.ApiUtils class
% Run by: ultrasat.api.debug_ApiUtils()
%

function debug_ApiUtils()
    % Main debug function for ApiUtils
    %
    % This function calls other debug functions to test the logging utilities.

    fprintf('=== Running ApiUtils Debug Tests ===\n');

    debug_msglog();
    debug_logException();

    fprintf('=== All ApiUtils Debug Tests Completed ===\n');
end

% ------------------------------------------------------------------------

function debug_msglog()
    % Tests msglog function of ApiUtils

    disp('Testing msglog...');

    if ispc
        LogFileName = 'c:/temp/debug_ApiUtils.log';
    else
        LogFileName = '/tmp/debug_ApiUtils.log';
    end

    Prefix = 'TestLogger';

    % Test simple logging
    % TODO: target not found - ultrasat.api.ApiUtils does not exist
    ultrasat.api.ApiUtils.msglog(LogFileName, Prefix, 'This is a test log message.');
    ultrasat.api.ApiUtils.msglog(LogFileName, Prefix, 'Another message with a number: %d', 42);

    disp('msglog test completed. Check console and log file.');
end

% ------------------------------------------------------------------------
% Test: Exception Logging
function debug_logException()
    % Tests logException function of ApiUtils

    disp('Testing logException...');

    if ispc
        LogFileName = 'c:/temp/debug_ApiUtils.log';
    else
        LogFileName = '/tmp/debug_ApiUtils.log';
    end

    Prefix = 'ErrorHandler';

    try
        % Force an error
        error('TestError:InvalidOperation', 'This is a test exception.');
    catch ME
        % Log with stack trace
        % TODO: target not found - ultrasat.api.ApiUtils does not exist
        ultrasat.api.ApiUtils.logException(LogFileName, Prefix, ME, true, 'Caught an exception with stack trace.');

        % Log without stack trace
        ultrasat.api.ApiUtils.logException(LogFileName, Prefix, ME, false, 'Caught an exception without stack trace.');
    end

    disp('logException test completed. Check console and log file.');
end

