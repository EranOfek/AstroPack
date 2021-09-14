function utAssert(Condition, varargin)
    % Wrapper for assert() - DO NOT USE at the moment. Just use assert()
    
    if ~Condition
        Msg = '';
        if numel(varargin) == 1
            Msg = varargin{1};
        end
        
        io.msgLog(LogLevel.Assert, 'Unit-Test Assert: %s', Msg);
        
        % The problem is that assert() will report issue here and not in
        % the caller! Cannot use.
        assert(Condition, Msg);
    end

end

