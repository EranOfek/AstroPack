function [Result] = dirDir(varargin)
    % dir function for directories only without '..' and '.'
    %     Optional detailed description
    % Input  : null
    % Output : - Same output as the dir function
    % Author : Eran Ofek (2024 Feb) 
    % Example: 

    Result = dir(varargin{:});

    Flag = [Result.isdir] & ~strcmp({Result.name}, '.') & ~strcmp({Result.name}, '..');
    Result = Result(Flag);

end
