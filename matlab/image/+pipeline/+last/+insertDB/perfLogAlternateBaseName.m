function Out = perfLogAlternateBaseName(Action, Name)
% perfLogAlternateBaseName Optional redirect for perfLogAppendLines (e.g. epoch MAT batch build).
%   perfLogAlternateBaseName('set', 'insertDB_epoch_mat_build.log')
%   perfLogAlternateBaseName('clear')
%   Name = perfLogAlternateBaseName('get')

    persistent AlternateBaseName
    if nargin < 1
        Action = 'get';
    end
    switch lower(Action)
        case 'get'
            if nargout > 0
                Out = AlternateBaseName;
            end
        case 'set'
            AlternateBaseName = Name;
        case 'clear'
            AlternateBaseName = '';
        otherwise
            error('perfLogAlternateBaseName:Action', 'Action must be set, clear, or get');
    end
end
