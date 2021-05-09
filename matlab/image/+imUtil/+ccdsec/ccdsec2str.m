function Str = ccdsec2str(CCDSEC)
% Conver a CCDSEC [Xmin Xmax Ymin Ymax] to a string.
% Example: Str = imUtil.ccdsec.ccdsec2str([1 1000 21 900])

Str = sprintf('[%d %d %d %d]',CCDSEC);
