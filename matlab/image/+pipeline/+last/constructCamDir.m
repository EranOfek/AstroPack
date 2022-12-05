function Path = constructCamDir(CameraNumber, Args)
    % Construct a camera data directory name on LAST computers
    % Input  : - CameraNumber (1 to 4). Default is 1.
    %          * ...,key,val,...
    %            'Node' - Node number. Default is 1.
    %            'SubDir' - Default is 'new'.
    %            'ProjNameBase' - Default is 'LAST'.
    % Output : - Path to camera data.
    % Author : Eran Ofek (Dec 2022)
    % Example: Path = pipeline.last.constructCamDir(1)
    
    arguments
        CameraNumber       = 1;
        Args.Node          = 1;
        Args.SubDir        = 'new';
        Args.ProjNameBase  = 'LAST';
    end
   
    HostName = tools.os.get_computer;
    
    MountNumberStr = HostName(5:6);
    
    ProjName = sprintf('%s.%02d.%s.%02d',Args.ProjNameBase, Args.Node, MountNumberStr, CameraNumber);
    
    switch CameraNumber
        case 1
            DataName = 'data1';
            HostName(7) = 'e';
        case 3
            DataName = 'data1';
            HostName(7) = 'w';
        case 2
            DataName = 'data2';
            HostName(7) = 'e';
        case 4
            DataName = 'data2';
            HostName(7) = 'w';
        otherwise
            error('Unknown CameraNumber option');
    end
    Path = sprintf('%s%s%s%s%s%s%s%s%s%s',filesep,HostName,filesep,DataName,filesep,'archive',filesep,ProjName);
    
    if ~isempty(Args.SubDir)
        Path = sprintf('%s%s%s',Path,filesep,Args.SubDir);
    end
    
end
