function Cfg = read(File)
    % Read a simulation configuration saved by ultrasat.simconfig.write
    % Input  : - File: the .mat file, or its .json twin (the .mat next to
    %               it is read, for the exact MATLAB types).
    % Output : - Cfg: the configuration, with its Record.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.simconfig.read('/Data/dana/ULTRASAT_sim/DUT/DUT-G2V-1/config/DUT-G2V-1.mat');

    arguments
        File (1,:) char
    end

    [Dir, Name] = fileparts(File);
    File = fullfile(Dir, [Name '.mat']);
    if ~isfile(File)
        error('simconfig:read:NoFile', 'No configuration file %s', File);
    end
    S   = load(File, 'Cfg');
    Cfg = S.Cfg;
end
