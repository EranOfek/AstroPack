function [OutFiles] = uncompress(FileName, Keep, Args)
    % Uncompress gzip/bzip2/fz files, with an option to keep/delete compressed files
    %   Will check for the suffix of each file in the input, and uncompress
    %   it according to the action list.
    %   By default treats bz2, fz, gz.
    % Input  : - A file name, or a cell/strings of file names.
    %          - A logical indicating if to keep the compressed file.
    %            If true then will use Args.ActionKeep.
    %            If false then will use Args.ActionDel.
    %            Default is true
    %          * ...,key,val,... 
    %            'ActionKeep' - A cell array of {Suffix, Action} wil action
    %                   in case that Keep=true.
    %                   Default is {'bz2','bunzip2 -k';
    %                               'fz', 'funpack';
    %                               'gz', 'gunzip -k'}
    %            'ActionDel' - A cell array of {Suffix, Action} wil action
    %                   in case that Keep=false.
    %                   Default is {'bz2','bunzip2';
    %                               'fz', 'funpack -D';
    %                               'gz', 'gunzip'}
    % Output : - A list of uncompressed file names.
    % Author : Eran Ofek (2024 Dec) 
    % Example: io.files.uncompress('a.gz')

    arguments
        FileName
        Keep logical      = true;
        Args.ActionKeep     = {'bz2','bunzip2 -k';
                               'fz', 'funpack';
                               'gz', 'gunzip -k'}
        Args.ActionDel      = {'bz2','bunzip2';
                               'fz', 'funpack -D';
                               'gz', 'gunzip'}
    end
    
    if ischar(FileName)
        FileName = string(FileName);
    end
    
    if Keep
        KeepField = 'ActionKeep';
    else
        KeepField = 'ActionDel';
    end
    
    Ntype = size(Args.(KeepField),1);
    
    N = numel(FileName);
    OutFiles = strings(N,1);
    K = 0;
    for I=1:1:N
        Tmp = split(FileName{I}, '.');
        for Itype=1:1:Ntype
            if strcmp(Tmp{end}, Args.(KeepField){Itype,1})
                
                system(sprintf('%s %s', Args.(KeepField){Itype,2}, FileName{I}));
                K = K + 1;
                OutFiles(K) = join(Tmp(1:end-1), '.');
                break;
            end
        end
    end
    OutFiles = OutFiles(1:K);
      
end
