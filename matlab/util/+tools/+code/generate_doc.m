%
%--------------------------------------------------------------------------

function Result = generate_doc(Args)
    arguments
        Args.Folder = '';
    end

    Result = false;
    if ~isempty(Args.Folder)
    end
    
    PWD = pwd;
    try

        [Path, FName] = fileparts(XlsFileName);
        cd(Path);
        PyScript = fullfile('python', 'utils', 'matlab_utils', 'get_matlab_functions.py');
        Py = fullfile(tools.os.getAstroPackPath, PyScript);
        if ~isfile(Py)
            return;
        end

        % Prepare command line, assume we have 'python3' installed
        Cmd = sprintf('python3 %s -f %s', Py, XlsFileName);
        io.msgLog(LogLevel.Info, 'xlsx2sql.py: %s', Cmd);
        [Status, Output] = system(Cmd);
        io.msgLog(LogLevel.Info, '%d', Status);
        io.msgLog(LogLevel.Info, '%s', Output);

    catch Ex
    end
    cd(PWD);
end

