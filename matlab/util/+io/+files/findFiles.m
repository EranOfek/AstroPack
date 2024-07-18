function [F] = findFiles(Template, Args)
    % Find files using various criteria (UNIX only).
    %   Criterai including:
    %   File were not modified in the past Args.NotModified minutes
    %   Were created after the TouchFile
    %   Size is in some range.
    % Input  : - File Template name. Default is "".
    %          * ...,key,val,...
    %            'BasePath' - Base pth on which to conduct the search.
    %                   Default is '.' (current dir).
    %            'Newer' - A reference file. If given, then only files
    %                   newer then this file will be returned.
    %                   Default is [].
    %            'IgnoreHidden' - Ignore hidden files.
    %                   Default is true.
    %            'NotModified' - If not 0, then check that the file
    %                   was not modified in the past X minutes.
    %                   Default is 0.1 (minutes).
    %            'MinSize' - Min. file size [bytes].
    %                   Default is 1e5.
    %            'MaxSize' - Max. file size [bytes].
    %                   Default is 1e7.
    %            'SortByTime' - A logical indicating if to sort the
    %                   output structure by time. Default is true.
    % Output : - A dir structure with all the found files.
    %            This will also be stored in the NewFiles property.
    % Author : Eran Ofek (Jul 2024)
    % Example: F=io.files.findFiles;
    
    arguments
        Template                     = [];
        Args.BasePath                = '.';
        Args.Newer                   = [];
        Args.IgnoreHidden logical    = true;
        Args.NotModified             = 0.1;
        Args.MinSize                 = 1e5;
        Args.MaxSize                 = 1e7;
        Args.SortByTime logical      = true;
    end

    if ~(isunix || ismac)
        error('io.files.WaitForNewFile class works only in linux based machines');
    end
    
    PWD = pwd;
    cd(Args.BasePath)
    
    
    if Args.IgnoreHidden
        Extra = "-not -path '*/.*'";
    else
        Extra = "";
    end
    if ~isempty(Template)
        ExtraName = sprintf("-name '%s'",Template);
    else
        ExtraName = "";
    end
    if Args.NotModified>0
        % make sure that the file was not modified in the last few
        % seconds
        ExtraMod = sprintf("-not -mmin %f",Args.NotModified);
    else
        ExtraMod = "";
    end
    if ~isempty(Args.MinSize)
        ExtraMinSize = sprintf("-size +%dc",Args.MinSize);
    else
        ExtraMinSize = "";
    end
    if ~isempty(Args.MaxSize)
        ExtraMaxSize = sprintf("-size -%dc",Args.MaxSize);
    else
        ExtraMaxSize = "";
    end
    
    
    
    if isempty(Args.Newer)
        [Stat, Ans] = system(sprintf("find . %s %s %s %s %s",Extra, ExtraName, ExtraMod, ExtraMinSize, ExtraMaxSize));
    else
        [Stat, Ans] = system(sprintf("find . %s %s %s %s %s -newer %s",Extra, ExtraName, ExtraMod, ExtraMinSize, ExtraMaxSize, Args.Newer));
    end
    
    %find . -not -path '*/.*' -type f -name '*some text*'
    
    SpAns = splitlines(Ans);
    SpAns = SpAns(1:end-1);
    Nsp   = numel(SpAns);
    F = struct('name',cell(Nsp,1), 'folder',cell(Nsp,1), 'date',cell(Nsp,1), 'bytes',cell(Nsp,1), 'isdir',cell(Nsp,1), 'datenum',cell(Nsp,1));
    
        
    for Isp=1:1:Nsp
        F(Isp) = dir(SpAns{Isp});
    end
    
    if Args.SortByTime
        F = io.files.dirSortedByDate(F);
    end
          
    cd(PWD);





end
