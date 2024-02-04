function List=filelist(FileName, Args)
    % Generate a cell array array of files list from file name/regular expression
    % Input  : - A file name, a file name containing wild
    %            cards or regular expression, a cell array of
    %            file names, or a structure arrawy which is the
    %            output of the dir command.
    %          * ...,key,val,...
    %            'UseRegExp' - A logical indicating if to use regular expression (true) or
    %                   wild cards (false). Default is false.
    %            'ReadFromFile' - If this argument is true, and the file name in the first
    %                   argument starts with '@', then will read the file names from
    %                   this files (lines start with % and # ignored).
    %                   Default is true.
    %            'AddPath' - Add path. Default is true.
    % Output : - A cell array of file names.
    % Author : Eran Ofek (Apr 2020)
    % Example: List=io.files.filelist('\w*.fits',true);

    arguments
        FileName
        Args.UseRegExp logical      = false;
        Args.ReadFromFile logical   = true;
        Args.AddPath logical        = true;
    end

    if Args.ReadFromFile
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
            if Args.UseRegExp
                Files = dir('*');
                if Args.AddPath
                    FilesCell = fullfile({Files.folder},{Files.name});
                else
                    FilesCell = {Files.name};
                end
                Tmp  = regexp(FilesCell,FileName,'match');
                Flag = ~cellfun(@isempty,Tmp);
                List = FilesCell(Flag);    
            else
                Files = dir(FileName);
                if Args.AddPath
                    List  = fullfile({Files.folder},{Files.name});
                else
                    List = {Files.name};
                end
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