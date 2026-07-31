function [Str, DiskP] = df(Template)
    % Get disk storage status (for UNIX-like systems)
    % Input  : - Either a disk name (e.g., 'data1'), matched as a substring
    %            against df output (ambiguous if several mounts, local or
    %            cross-mounted from another host, share that name - the
    %            first match is used and a warning is issued), or a path
    %            (e.g., '/last02e/data1/archive/LAST.01.02.01'), resolved
    %            unambiguously to the filesystem that contains it.
    %            If given, the second output argument will contain the
    %            disk percentage.
    % Output : - String of df output.
    %          - Percentage-full of the requested disk/path.
    %            NaN if not found.
    % Author : Eran Ofek (2023 Dec)
    % Example: [Str, DiskP] = tools.os.df('data1')
    %          [Str, DiskP] = tools.os.df('/last02e/data1/archive/LAST.01.02.01')

    arguments
        Template

    end

    Str   = '';
    DiskP = NaN;

    if isunix || ismac
        if isempty(Template)
            [R,Str] = system('df');
        else
            if contains(Template, filesep)
                % path: resolve unambiguously to its containing filesystem.
                % -P (POSIX format) prevents long remote device names (e.g.
                % 'host:/path') from wrapping onto a second line
                [R,Str] = system(sprintf('df -P "%s" | tail -1',Template));
            else
                % disk name: substring match against df output
                [R,Str] = system(sprintf('df | grep %s',Template));
            end
            if isempty(Str)
                DiskP = NaN;
            else
                Cell = regexp(strtrim(Str), '\s+', 'split');
                Ind = find(contains(Cell, '%'));
                if isempty(Ind)
                    DiskP = NaN;
                else
                    if numel(Ind) > 1
                        warning('tools.os.df: template ''%s'' matched more than one line in df output - using the first match', Template);
                    end
                    DiskP = str2double(Cell{Ind(1)}(1:end-1));
                end
            end
        end

    else
        error('Likely a windows sysytem - df does not work');
    end

end
