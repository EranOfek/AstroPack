%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.ensurePath_.m
% Author      : Chen Tishler
% Created     : 07/09/2026
% Updated     : 09/09/2026
% Description : Ensure matlab/util is on path when startup did not run.
%==========================================================================

function ensurePath_()
%ENSUREPATH_  Add AstroPack matlab/util when ASTROPACK_PATH is set.
%
%   Used by -batch launcher scripts that may run without AstroPack startup.
%   No-op when ASTROPACK_PATH is unset (caller must have run startup).
%
% Example:
%   db.sources.debug.ensurePath_();

    astroPath = getenv('ASTROPACK_PATH');
    if isempty(astroPath)
        return;
    end

    utilPath = fullfile(astroPath, 'matlab', 'util');
    if isfolder(utilPath)
        addpath(utilPath);
    end
end
