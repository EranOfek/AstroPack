function [Response, Output, FileName] = wget(URL, Args)
    % A wrapper around wget. See also www.pwget
    % Input  : - A single URL string.
    %          * ...,key,val,...
    %            'Username' - Username. Default is ''.
    %            'Password' - Password. Default is ''.
    %            'OutputFile' - Output file name:
    %               NaN - output the file content into the second output
    %                       argument.
    %               [] - output the file into the default file name (i.e.,
    %                       file name in the URL).
    %               string - output the file into the file name specified
    %                       in the striing.
    %               Default is NaN.
    %            'AddPars' - A string of additional parameters to pass to
    %                   wget.
    %                   Default is '--no-check-certificate -U Mozilla'.
    % Output : - Sucess. 0 for ok.
    %          - Output content of URL, or URL file name.
    %          - File name in which the URL content was written into.
    % Author : Eran Ofek (Apr 2022)
    % Example: [Response, Output, FileName] = www.wget('http://euler1.weizmann.ac.il/catsHTM/catsHTM_catalogs.html')
    
    arguments
        URL
        Args.Username       = '';
        Args.Password       = '';
        Args.OutputFile     = NaN;  % if empty use default
        Args.AddPars        = '--no-check-certificate -U Mozilla';
    end
   
    FileName = '';
    if isempty(Args.OutputFile)
        OutputFileStr = sprintf('');
        [~,FileName,Ext] = fileparts(URL);
        FileName      = sprintf('%s%s',FileName,Ext);
    else
        if isnan(Args.OutputFile)
            OutputFileStr = sprintf('-O -');
        else
            OutputFileStr = sprintf('-O %s',Args.OutputFile);
            FileName      = Args.OutputFile;
        end
    end
    
    if ~isempty(Args.Username) && ~isempty(Args.Password)
        UserPass = sprintf('--user=%s --password=%s',Args.Username, Args.Password);
    else
        UserPass = '';
    end
    
    Command = sprintf('wget %s %s %s %s',Args.AddPars, UserPass, OutputFileStr, URL);
    [Response, Output] = system(Command); 
    
end