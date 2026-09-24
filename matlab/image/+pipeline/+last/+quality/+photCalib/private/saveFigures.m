function Saved = saveFigures(OutDir, Figs, Args)
    % Save figures to disk as .fig and/or image files.
    % Description: Writes each figure to OutDir, naming the file from the
    %              figure Name (sanitised via makeValidName), falling back to
    %              fig_<Number> when the figure has no name. Opt-in helper —
    %              the photCalib plotting functions return figure handles and
    %              the caller decides whether to call this.
    % Input  : - OutDir - target directory (created if missing).
    %          - Figs   - figure handle array; [] (default) saves every
    %                     currently open figure.
    %          * ...,key,val,...
    %            'Formats' - Cell of extensions to write. 'fig' uses savefig;
    %                        any other (e.g. 'jpg','png') uses saveas.
    %                        Default {'fig','jpg'}.
    %            'Verbose' - Print a one-line summary. Default false.
    % Output : - Saved - string array of written file paths.
    % Author : photCalib package refactor (2026-05)
    % Example: H = plotPhotResiduals(PC); saveFigures('~/out', H);

    arguments
        OutDir        {mustBeTextScalar}
        Figs                            = []
        Args.Formats  cell              = {'fig','jpg'}
        Args.Verbose  logical           = false
    end

    OutDir = char(OutDir);
    if isempty(Figs)
        Figs = findall(0, 'Type', 'figure');
    end
    if ~isempty(Figs) && ~exist(OutDir, 'dir')
        mkdir(OutDir);
    end

    Saved = strings(0, 1);
    for If = 1:numel(Figs)
        Fig = Figs(If);
        if ~isgraphics(Fig) || ~isvalid(Fig); continue; end
        if isempty(Fig.Name)
            Name = sprintf('fig_%d', Fig.Number);
        else
            Name = matlab.lang.makeValidName(Fig.Name);
        end
        for Ik = 1:numel(Args.Formats)
            Fmt  = lower(char(Args.Formats{Ik}));
            Path = fullfile(OutDir, [Name '.' Fmt]);
            if strcmp(Fmt, 'fig')
                savefig(Fig, Path);
            else
                saveas(Fig, Path);
            end
            Saved(end+1, 1) = string(Path); %#ok<AGROW>
        end
    end

    if Args.Verbose
        fprintf('saveFigures: wrote %d file(s) for %d figure(s) to %s\n', ...
            numel(Saved), numel(Figs), OutDir);
    end
end
