function Rows = headerRows(Cfg, Extra)
    % The simulation-set keywords of the image headers, for the 'ExtraHeader' argument of ultrasat.usim
    %   They tie every image to the configuration that made it:
    %     SIMTASK  - Cfg.Meta.Task
    %     SIMVER   - Cfg.Meta.Version
    %     SIMCONF  - the configuration file, <Version>.json in the config directory of the set
    %     SIMGIT   - the AstroPack commit of the saved configuration
    %     SIMLOCAL - 'T' if +ultrasat had local changes at that commit
    %   The configuration must be saved first (ultrasat.simconfig.write
    %   adds the Record with the commit). With Cfg.Header.SimKeys false the
    %   output is empty, so the keywords are switched off in one place.
    % Input  : - Cfg: a saved configuration (the third output of
    %               ultrasat.simconfig.write, or ultrasat.simconfig.read).
    %          - Extra: an N x 3 cell {key, value, comment} of per-image
    %               keywords appended to the rows (e.g. the aimpoint or the
    %               visit of the image). Default is {}.
    % Output : - Rows: an N x 3 cell {key, value, comment}.
    % Author : D. Kovaleva (Oct 2026)
    % Example: [~, ~, Cfg] = ultrasat.simconfig.write(ultrasat.dut.config, Dir);
    %          Rows = ultrasat.simconfig.headerRows(Cfg, {'SIMVISIT', 3, 'new visit number'});
    %          % ultrasat.usim(..., 'ExtraHeader', Rows);

    arguments
        Cfg (1,1) struct
        Extra cell = {}
    end

    Rows = cell(0, 3);
    if ~isfield(Cfg, 'Header') || ~isfield(Cfg.Header, 'SimKeys') || Cfg.Header.SimKeys
        if ~isfield(Cfg, 'Record')
            error('simconfig:headerRows:NotSaved', 'Save the configuration first (ultrasat.simconfig.write): it has no Record');
        end
        Version = char(Cfg.Meta.Version);
        Rows = { ...
            'SIMTASK',  char(Cfg.Meta.Task),             'simulation task'; ...
            'SIMVER',   Version,                         'simulation set version'; ...
            'SIMCONF',  [Version '.json'],               'configuration file (config dir of the set)'; ...
            'SIMGIT',   char(Cfg.Record.GitCommit),      'AstroPack git commit'; ...
            'SIMLOCAL', tf(~isempty(Cfg.Record.GitStatus)), 'local changes in +ultrasat at that commit'};
        if ~isempty(Extra)
            if size(Extra, 2) ~= 3
                error('simconfig:headerRows:Extra', 'Extra must be an N x 3 cell {key, value, comment}');
            end
            Rows = [Rows; Extra];
        end
    end
end

function S = tf(Cond)
    % 'T' or 'F'
    S = 'F';
    if Cond
        S = 'T';
    end
end
