function cleanVisitsVer(Args)
    % Clean visits in LAST archive according to their version (e.g., remove all non v0 versions')
    % Input  : * ...,key,val,...
    %            'BasePath' - Default is '/marvin/LAST.01.01.01'.
    %            'Template' - File name to search in order to identify
    %                   visits directories. Default is '.status'.
    %            'Method' - On of the following options:
    %                   'removeNon0' - will remove all visits with version
    %                           different then 'v0'.
    %                   Default is 'removeNon0'.
    %
    % Output : null
    % Author : Eran Ofek (2024 Jul) 
    % Example: pipeline.last.cleanVisitsVer

    arguments
        Args.BasePath                 = '/marvin/LAST.01.01.01';
        Args.Template                 = '.status'; %'LAST.*_coadd_*.fits'
        Args.Method                   = 'removeNon0'
    end

    PWD = pwd;
    cd(Args.BasePath);

    F = io.files.findFiles(Args.Template, 'IgnoreHidden',false, 'MinSize',[], 'MaxSize',[]);
    AllFolders = string({F.folder});

    switch lower(Args.Method)
        case 'removenon0'
            Flag = contains(AllFolders, 'v1') | contains(AllFolders, 'v2') | contains(AllFolders, 'v3') | contains(AllFolders, 'v4') | contains(AllFolders, 'v5') | ...
                   contains(AllFolders, 'v6') | contains(AllFolders, 'v7') | contains(AllFolders, 'v8') | contains(AllFolders, 'v9');
            RemoveFolders = AllFolders(Flag);
            Nrf = numel(RemoveFolders);
            fprintf('Found %d directories to remove out of %d directories\n', Nrf, numel(Flag));
            for Irf=1:1:Nrf
                cd(RemoveFolders{Irf});
                cd ..
                Tmp = split(RemoveFolders{Irf}, filesep);
                fprintf('Removing directory: %s\n',Tmp{end});
                system(sprintf('sudo rm -rf %s',Tmp{end}));
            end
    end

    cd(PWD);


end
