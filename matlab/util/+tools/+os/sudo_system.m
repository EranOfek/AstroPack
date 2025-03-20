function [Status, Result] = sudo_system(Cmd, Pass)
    % Execute system command with sudo
    % Input  : - Command to execute.
    %          - Password.
    % Output : - Status of executed command.
    %          - Result of executed command.
    % Author : Eran Ofek (2025 Mar) 
    % Example: [Status, Result]=tools.os.sudo_system('ls','mypass')

    arguments
        Cmd
        Pass
    end
    [Status, Result] = system(sprintf('echo "%s" | sudo -S %s',Pass,Cmd));
    
end
