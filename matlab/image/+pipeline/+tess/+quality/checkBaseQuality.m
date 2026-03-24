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

    % check data quality flags
    if FFI.HeaderData.isKeyExist('DQUALITY')
        % TODO: if flag doesn't exist, reject image?
        BD_TESS = BitDictionary('Header.DataFlags.TESS');

        DQUALITY = FFI.HeaderData.getVal('DQUALITY');

        if BD_TESS.findBit(DQUALITY,'AttitudeTweak')
            if ~isempty(Args.Logger)
                Args.Logger.msgLog(LogLevel.Info, 'FFI taken during an attitude tweak.');
            end
            
            BaseQuality = 0;
        end

        if BD_TESS.findBit(DQUALITY,'CoarsePoint')
            if ~isempty(Args.Logger)
                Args.Logger.msgLog(LogLevel.Info, 'FFI taken during coarse pointing.');
            end
            
            BaseQuality = 0;
        end

        if BD_TESS.findBit(DQUALITY,'WheelDesaturation')
            if ~isempty(Args.Logger)
                Args.Logger.msgLog(LogLevel.Info, 'FFI taken during reaction wheel desaturation.');
            end
            
            BaseQuality = 0;
        end

        if BD_TESS.findBit(DQUALITY,'Straylight')
            if ~isempty(Args.Logger)
                Args.Logger.msgLog(LogLevel.Info, 'FFI predicted to contain Earth or Moon stray light in FoV.');
            end
            
            BaseQuality = 0;
        end

    end

end