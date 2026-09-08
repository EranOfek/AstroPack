%==========================================================================
% Project     : AstroPack
% File        : db.sources.debug.debugDataRoot_.m
% Author      : Chen Tishler
% Created     : 07/09/2026
% Updated     : 07/09/2026
% Description : Runtime data root for sources debug scripts.
%==========================================================================

function root = debugDataRoot_()
%DEBUGDATAROOT_  Return $ASTROPACK_DATA_PATH/sources/debug for debug runtime files.
%
% Example:
%   dataDir = db.sources.debug.debugDataRoot_();

    root = fullfile(tools.os.getAstroPackDataPath(), 'sources', 'debug');
end
