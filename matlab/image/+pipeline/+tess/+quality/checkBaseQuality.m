function BaseQuality = checkBaseQuality(FFI, Args)
    % TODO: Should do a more detailed report, for now just do a binary
    % pass/fail decision.

    arguments
        FFI

        Args.Logger = [];
    end
    
    BaseQuality = 1;

    % check if astrometry solution exists
    if ~FFI.HeaderData.isKeyExist('WCSAXES')
        if ~isempty(Args.Logger)
            Args.Logger.msgLog(LogLevel.Info, 'FFI has no astrometry solution');
        end
        % TODO: could derive astronometry ourselves
        % for now just fail the check

        BaseQuality = 0;
    end

end