function [Result] = readMsgFile(FileName, Path)
    % Read msessages and time stamps from file.
    % Input  : - File name.
    %          - Path. Default is ''.
    % Output : - Structure array with all the messages.
    %            Following fields are available:
    %            .TimeStamp - ISO time stamp.
    %            .Msg - Message.
    %            .JD - Julian day.
    % Author : Eran Ofek (2024 Jul) 
    % Example:
    % R=tools.timeStamp.readMsgFile('.status','/marvin/LAST.01.01.01/2024/07/05/proc/233146v0')

    arguments
        FileName
        Path = '';
    end

    FullFileName = fullfile(Path, FileName);
    FID = fopen(FullFileName);
    I = 0;
    Result = struct('TimeStamp','', 'Msg','', 'JD',NaN);
    while ~feof(FID)
        I = I + 1;
        L{I}=fgetl(FID);
        if numel(L{I})>19
            % context exist
            Result(I).TimeStamp = L{I}(1:19);
            Result(I).Msg       = L{I}(21:end);
            Result(I).JD        = celestial.time.julday(Result(I).TimeStamp);
        end
    end
    fclose(FID);
    


end
