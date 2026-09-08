%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.ensurePath_.m
% Author      : Chen Tishler
% Created     : 07/09/2026
% Updated     : 07/09/2026
% Description : Ensure matlab/util is on path when startup did not run.
%==========================================================================

function ensurePath_()
%ENSUREPATH_  Add AstroPack matlab/util when ASTROPACK_PATH is set.
%
% Example:
%   db.sources.debug.ensurePath_();

    astroPath = getenv('ASTROPACK_PATH');
    if isempty(astroPath)
        return;
    end

    % Parent of +db package (not the + folder itself).
    utilPath = fullfile(astroPath, 'matlab', 'util');
    if isfolder(utilPath)
        addpath(utilPath);
    end
end
