%
% Create Standalone Application from MATLAB
% https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
%
% https://www.mathworks.com/matlabcentral/answers/97204-how-can-i-pass-input-parameters-when-running-matlab-in-batch-mode-in-windows
% matlab /r "myfunc(2)"
%
% https://www.mathworks.com/matlabcentral/answers/456241-how-to-apply-command-line-arguments-in-matlab
% To pass command-line arguments to a MATLAB executable, you define a single 
% MATLAB function in the executable. The arguments to the function are taken 
% from the command line parameters (the first command-line parameter is the 
% first argument, and so on).
%--------------------------------------------------------------------------

function Result = Pipeline(InputFileName)
    % Command-line entry point for PipelineMain class
    io.msgLog(LogLevel.Debug, 'Pipeline started: %s', InputFileName);            
    try
        Main = PipelineMain;
        Result = Main.run(InputFileName);        
    catch
        io.msgLog(LogLevel.Error, 'Pipeline exception');
        Result = false;
    end
    io.msgLog(LogLevel.Debug, 'Pipeline done: %s', InputFileName);            
end
