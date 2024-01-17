function [AbsPath] = relPath2absPath2(RelPath)
    % Generate an absolute path from relative path
    %     Given a relative path including '~' and '/../' create a clean
    %     absolute path.
    % Input  : - Relative path.
    % Output : - Absolute path
    % Author : Eran Ofek (2024 Jan) 
    % Example: FN = '~/matlab/data/spec/../a.fits'; FN1 = tools.os.relPath2absPath2(FN)
    %          DN = '~/matlab/data/../AstroPack/../data/'; DN1 = tools.os.relPath2absPath2(DN)

    FileSep = filesep;
    
    if strcmp(RelPath(1),'~')
        HomePath = getenv('HOME');
        if strcmp(RelPath(2), filesep)
            RelPath = sprintf('%s%s', HomePath, RelPath(2:end));
        else
            RelPath = sprintf('%s%s%s', HomePath, FileSep, RelPath(2:end));
        end
    end
    
    FoundDots = true;
    Splitted = split(RelPath, FileSep);
    while FoundDots
        
        I = find(strcmp(Splitted,'..'),1,'first');
        if ~isempty(I)
            Splitted{I-1} = [];
            Splitted{I}   = [];
        else
            FoundDots = false;
        end
    end
    
    % rebuilt path
    Ns = numel(Splitted);
    AbsPath = '';
    for Is=1:1:Ns
        if ~isempty(Splitted{Is})
            AbsPath = sprintf('%s%s%s', AbsPath, FileSep, Splitted{Is});
        end
    end
    
        
end
