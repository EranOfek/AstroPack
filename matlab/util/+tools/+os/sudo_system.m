function [Status, Result] = sudo_system(Cmd, Pass)
    % Execute system command with sudo or without sudo.
    % Input  : - Command to execute.
    %          - Password. If [], then will execute the command without
    %            sudo. Default is [].
    % Output : - Status of executed command.
    %          - Result of executed command.
    % Author : Eran Ofek (2025 Mar) 
    % Example: [Status, Result]=tools.os.sudo_system('ls','mypass')

    arguments
        Cmd
        Pass  = [];
    end
    
    if isempty(Pass)
        [Status, Result] = system(Cmd);
    else
        [Status, Result] = system(sprintf('echo "%s" | sudo -S %s',Pass,Cmd));
    end
    
end
