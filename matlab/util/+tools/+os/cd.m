function Dir = cd(varargin)
    % Change dir function. If no arguments cd to home dir.
    % Input :  - dir name. if empty, chane to home dir.
    % Output : - current dir name.
    % Author : Eran Ofek (Apr 2022)
    % Example: tools.os.cd
    
    if nargin==0
        varargin{1} = sprintf('~%s',filesep);
    end
    
    cd(varargin{:});
    if nargout>0
        Dir = pwd;
    end
end