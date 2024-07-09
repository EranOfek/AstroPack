function Result=writeMsg(Msg, Args)
    % Write Msg with time stamp to file.
    % Input  : - Msg to print to file with a time stamp.
    %          * ...,key,val,...
    %            'Time' - Default is [] - i.e., write current time.
    %            'FileName' - If empty, then do not write to file.
    %                   If number, then this is the FID of an open file.
    %                   Default is '.status'.
    %            'Path' - Default is ''.
    % Output : - Message with time stamp.
    % Author : Eran Ofek (Apr 2023)

    arguments
        Msg
        Args.Time     = [];
        Args.FileName = '.status';
        Args.Path     = '';
    end

    
    TimeStamp = tools.timeStamp.getTimeStamp(Args.Time);
    Result = sprintf('%s %s\n',TimeStamp, Msg);
    
    if ~isempty(Args.FileName)
        if isnumeric(Args.FileName)
            fprintf(Args.FileName,Result);
        else
            FileName = fullfile(Args.Path,Args.FileName);
            FID = fopen(FileName,'a+');
            fprintf(FID,Result);
            fclose(FID);
        end
    end

end
