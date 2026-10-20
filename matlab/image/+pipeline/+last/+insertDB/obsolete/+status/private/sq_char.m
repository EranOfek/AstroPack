function c = sq_char(x)
%SQ_CHAR  Convert a scalar string or char row to char (private helper).
if isstring(x)
    if ~isscalar(x)
        error('sqstatus:badInput', 'Expected a single string, got %d elements.', numel(x));
    end
    c = char(x);
elseif ischar(x)
    c = x(:).';
else
    error('sqstatus:badInput', 'Expected char or string, got %s.', class(x));
end
end
