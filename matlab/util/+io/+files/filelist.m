function List=filelist(FileName, UseRegExp, ReadFromFile)
    % Generate a cell array array of files list from file name/regular expression
    % Input  : - A file name, a file name containing wild
    %            cards or regular expression, a cell array of
    %            file names, or a structure arrawy which is the
    %            output of the dir command.
    %          - A logical indicating if to use regular expression (true) or
    %            wild cards (false). Default is false.
    %          - If this argument is true, and the file name in the first
    %            argument starts with '@', then will read the file names from
    %            this files (lines start with % and # ignored).
    %            Default is true.
    % Output : - A cell array of file names.
    % Author : Eran Ofek (Apr 2020)
    % Example: List=io.files.filelist('\w*.fits',true);

    arguments
        FileName
        UseRegExp logical      = false;
        ReadFromFile logical   = true;
    end

    if ReadFromFile
        if strcmp(FileName(1),'@')
            % assume file name is a file containing file names
            FileName = FileName(2:end);
            FID = fopen(FileName);
            C = textscan(FID,'%s\n','commentStyle',{'%','#'});
            fclose(FID);
            List = C{1};
            NormalMode = false;
        else
            NormalMode = true;
        end
    else
        NormalMode = true;
    end

    if NormalMode

        if ischar(FileName) || isstring(FileName)
            if UseRegExp
                Files = dir('*');
                FilesCell = fullfile({Files.folder},{Files.name});
                Tmp  = regexp(FilesCell,FileName,'match');
                Flag = ~cellfun(@isempty,Tmp);
                List = FilesCell(Flag);    
            else
                Files = dir(FileName);
                List  = fullfile({Files.folder},{Files.name});
            end

        elseif isstruct(FileName)
            List = {FileName.name};
        elseif iscell(FileName)
            List = FileName;
        else
            error('Unknown file names input');
        end
    end
end