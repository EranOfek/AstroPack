function [Str, DiskP] = df(Template)
    % Get disk storage status (for UNIX like systems)
    % Input  : - An optional disk name (e.g., 'data1')
    %            If given, then only the disk name data will be retrieved,
    %            and the second output argument will contain the disk
    %            percentage.
    % Output : - String of df output.
    %          - Percentage-full of requested disk name.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [Str, DiskP] = tools.os.df('data1')

    arguments
        Template
        
    end

    Str   = '';
    DiskP = NaN;

    if isunix || ismac
        if isempty(Args.Template)
            [~,Str] = system('df');
        else
            [~,Str] = system(sprintf('df | grep %d',Template));
            Cell = regexp(Str, 'split', '\s');
            Ind = find(contains(Cell, '%s'));
            DiskP = str2double(Cell(Ind)(1:end-1));
        end

    else
        error('Likely a windows sysytem - df does not work');
    end

end
