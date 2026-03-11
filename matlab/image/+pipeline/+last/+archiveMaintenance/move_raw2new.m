function move_raw2new(Path, Args)
    % Move raw images from /YMD/raw/ dir to the new/ dir and uncompress
    % Input  : - Path from which to start the search for raw dirs.
    %          * ...,key,val,... 
    %            'NewPath' - new/ dir path name into which to move the raw
    %                   files. If empty, then choose automatically.
    %                   Default is [].
    %            'NewPathSplit' - Used for auto new/ search. Default is 'LAST'.
    %            'NewPathName' - Used for auto new/ search. Default is 'new'.
    % Output : null
    % Author : Eran Ofek (2026 Mar) 
    % Example: pipeline.last.archiveMaintenance.move_raw2new

    arguments
        Path                   = [];
        Args.NewPath           = [];
        Args.Uncompress        = true;

        Args.NewPathSplit      = 'LAST';
        Args.NewPathName       = 'new';
        
    end

    PWD = pwd;
    if ~isempty(Path)
        cd(Path);
    end
    
    %
    
    List = io.files.findDirBySubString('raw');

    if ~isempty(List)
        if isempty(Args.NewPath)
            Sp = split(List(1), Args.NewPathSplit);
            NewPath = sprintf('%s%s%s%s', Sp{1}, Args.NewPathSplit, filesep, Args.NewPathName);
        end
    
        Nl = numel(List);
        for Il=1:1:Nl
            io.files.moveFiles('LAST.*', [], List{Il}, NewPath, 'RegExp',true);
        end

        if Args.Uncompress
            cd(NewPath);

            Files = dir('*.fz');
            io.files.uncompress({Files.name}, false);

            cd(PWD);
        end

    end

    cd(PWD);

end
