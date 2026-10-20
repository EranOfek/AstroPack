function [keys, isMulti] = sq_cellkeys(key)
%SQ_CELLKEYS  Normalize one key or many keys (string array / cellstr) to a cellstr.
if iscell(key)
    keys = cellfun(@sq_char, key(:), 'UniformOutput', false);
    isMulti = true;
elseif isstring(key) && ~isscalar(key)
    keys = cellstr(key(:));
    isMulti = true;
else
    keys = {sq_char(key)};
    isMulti = false;
end
end
