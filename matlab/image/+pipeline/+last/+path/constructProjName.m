function [ProjName, MountNumber, CameraNumber, HostName] = constructProjName(ProjName, HostName, MountNumber, DataNumber, NodeNumber, BaseProjName)
    % Construct full LAST ProjName for ImagePath file names
    % Input  : - ProjName. Default is [].
    %          - Host name. Default is empty.
    %          - MountNumber. Default is empty.
    %          - DataNumber. Disk number. Default is 1.
    %          - Node Number. Default is 1.
    %          - BaseProjName. Default is 'LAST'.
    % Output : - Project name. E.g., 'LAST.01.02.03'.
    % Author : Eran Ofek (Aug 2022)
    
    
    arguments
        ProjName     = [];
        HostName     = [];
        MountNumber  = [];
        DataNumber   = 1;
        NodeNumber   = 1;
        BaseProjName = 'LAST';
    end
    
    if isempty(HostName)
        HostName = tools.os.get_computer;
    end
    
    if isempty(ProjName)
        % ProjName is empty - construct the default LAST camera address:
        switch HostName(end)
            case 'e'
                % East computer controls cameras 1 & 2
                CameraNumber = DataNumber + 0;
            case 'w'
                % East computer controls cameras 3 & 4
                CameraNumber = DataNumber + 2;
            otherwise
                error('Unknown host name template')
        end
        if isempty(MountNumber)
            MountNumber = HostName(5:6);
        else
            if isnumeric(MountNumber)
                MountNumber = sprintf('%02d',MountNumber);
            end
        end
        ProjName = sprintf('%s.%02d.%s.%02d', BaseProjName, NodeNumber, MountNumber, CameraNumber);
    end
    
end