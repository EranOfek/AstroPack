function Ok = saveResult(FilePath, Result, Args)
    % Save a result variable to a .mat file (non-fatal on failure).
    % Description: Writes Result to FilePath as a single named variable using
    %              the v7.3 format. A save failure is downgraded to a warning
    %              rather than an error, matching the photCalib convention
    %              that a failed cache write must not abort an analysis run.
    % Input  : - FilePath - destination .mat path.
    %          - Result   - value to save.
    %          * ...,key,val,...
    %            'VarName' - Variable name inside the .mat file.
    %                        Default 'Result'.
    %            'Verbose' - Print a confirmation line. Default false.
    % Output : - Ok - logical, true if the file was written.
    % Author : photCalib package refactor (2026-05)
    % Example: saveResult('~/out/run.mat', R, 'VarName', 'R');

    arguments
        FilePath      {mustBeTextScalar}
        Result
        Args.VarName  {mustBeTextScalar} = 'Result'
        Args.Verbose  logical            = false
    end

    Ok = false;
    S  = struct();
    S.(Args.VarName) = Result;
    try
        save(char(FilePath), '-struct', 'S', '-v7.3');
        Ok = true;
        if Args.Verbose
            fprintf('saveResult: saved %s\n', char(FilePath));
        end
    catch ME
        warning('photCalib:saveResult:SaveFailed', ...
            'Failed to save %s: %s', char(FilePath), ME.message);
    end
end
