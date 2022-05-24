%
%--------------------------------------------------------------------------

function Result = generate_mlx(Args)
    arguments
        Args.File = '';
        Args.outputPath = '';
    end
    
    Result = false;
    
    if isempty(Args.File)
        error('No file found');
    end
    if isempty(Args.outputPath)
        error('No output path');
    end
    
    Args.File = strrep(Args.File,'\','/');
    Args.outputPath = strrep(Args.outputPath,'\','/');
    
    
    try
        PyScript = fullfile('python', 'utils', 'matlab_utils', 'matlab_docgen.py');
        Py = fullfile(tools.os.getUltrasatPath(), PyScript)
        if ~isfile(Py)
            return;
        end

        % Prepare command line, assume we have 'python3' installed
        Cmd = sprintf('python3 %s -f %s -op %s -min -sf', Py, Args.File, Args.outputPath);
        io.msgLog(LogLevel.Info, 'matlab_docgen.py: %s', Cmd);
        [Status, Output] = system(Cmd);
        io.msgLog(LogLevel.Info, '%d', Status);
        io.msgLog(LogLevel.Info, '%s', Output);

        % copy template for help function
        help_template = fullfile(tools.os.getAstroPackPath(), 'matlab', 'util', '+tools', '+code', 'help_template.m');
        [filepath,name,ext] = fileparts(Args.File);
        new_help_template = fullfile(filepath, 'help_template.m');
        copyfile(help_template, filepath);
        
        % insert new mlx path
        help_target = fullfile(filepath, 'help.m');
        S = fileread(new_help_template);
        S = regexprep(S, '__REPLACE__', Args.outputPath);
        fid = fopen(help_target, 'w');
        fwrite(fid, S);
        fclose(fid);        
       
        % remove temporary template
        delete(new_help_template);
        
    catch Ex
    end

end
