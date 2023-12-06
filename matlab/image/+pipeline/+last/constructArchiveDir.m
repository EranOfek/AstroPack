function [BasePath, CalibDir, NewFilesDir, ProjName] = constructArchiveDir(Args)
    % Construct archive directory names for LAST local computers
    % Input  : ...,key,val,...
    %          'DataNum' - Disk number. Default is 1.
    %          'Node' - Node number. Default is 1.
    %          'ProjName' - Project name (e.g., 'LAST.01.02.03').
    %                   If empty, will be constructed.
    %                   Default is empty.
    %          'NewFilesDir' - e.g., '/last02w/data1/archive/LAST.01.02.03/new'
    %                   If empty, then will be constructed. Default is [].
    %          'CalibDir' - e.g., '/last02w/data1/archive/LAST.01.02.03/calib'
    %                   If empty, then will be constructed. Default is [].
    %          'BasePath' - e.g., '/last02w/data1/archive'.
    %                   If empty, then will be constructed. Default is [].
    % Output : - BasePath
    %          - CalibDir
    %          - NewFilesDir
    %          - ProjName
    % AUthor : Eran Ofek (Sep 2022)
    % Example: [BasePath, CalibDir, NewFilesDir, ProjName] = pipeline.last.constructArchiveDir

    
    arguments
        Args.DataNum                  = 1;   % disk data# number
        Args.Node                     = 1;
        Args.ProjName                 = [];
        
        Args.NewFilesDir              = [];  % '/last02w/data1/archive/LAST.01.02.03/new'
        Args.CalibDir                 = [];  % '/last02w/data1/archive/LAST.01.02.03/calib'
        Args.BasePath                 = [];  % '/last02w/data1/archive'
    end
    
    ComputerName = tools.os.get_computer;
    if isempty(Args.ProjName)
        ComputerNumber = str2double(ComputerName(5:6));
        ComputerSide   = ComputerName(7);
        switch ComputerSide
            case 'w'
                CamNumber = 2 + Args.DataNum;
            case 'e'
                CamNumber = Args.DataNum;
            otherwise
                error('Can not parse computer name - illegal number');
        end
        ProjName = sprintf('LAST.%02d.%02d.%02d',Args.Node, ComputerNumber, CamNumber);
    else
        ProjName = Args.ProjName;
    end
    
    if isempty(Args.BasePath)
        BasePath = sprintf('%s%s%s%s%d%s%s',filesep,ComputerName,filesep,'data',Args.DataNum,filesep,'archive');
    else
        BasePath = Args.BasePath;
    end
    if isempty(Args.CalibDir)
        CalibDir = sprintf('%s%s%s%s%s',BasePath,filesep,ProjName,filesep,'calib');
    else
        CalibDir = Args.CalibDir;
    end
    if isempty(Args.NewFilesDir)
        NewFilesDir = sprintf('%s%s%s%s%s',BasePath,filesep,ProjName,filesep,'new');
    else
        NewFilesDir = Args.NewFilesDir;
    end
    
    
    
    
end
