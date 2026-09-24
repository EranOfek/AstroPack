function [Result] = getSwiftXRT_RawCorners(Instrument, Window)
    % Get RAW coordinates CCDSEC for Swift-XRT given file name literals.
    %   The Instrument and Winow are the output of:
    %   imUtil.swiftXRT.parseSwiftXRTFilenames
    % Input  : - Instrument - string, e.g., 'xpc', 'xwt'
    %          - Window     - string, e.g., 'w1', 'w2', 'w3', 'w4'
    % Output : - [Xmin, Xmax, Ymin, Ymax]
    % Author : Eran Ofek (2026 Jan) 
    % Example: 

    % Default empty output
    Result = [NaN NaN NaN NaN];

    switch lower(Instrument)
        case 'xpc'   % Photon Counting mode
            switch lower(Window)
                case 'w4'  % full-frame
                    RawXRange = [0 599];
                    RawYRange = [0 599];
                case 'w3'  % half-frame
                    RawXRange = [0 599];
                    RawYRange = [0 299];
                case 'w2'  % quarter-frame
                    RawXRange = [0 299];
                    RawYRange = [0 299];
                case 'w1'  % small calibration window (approx)
                    RawXRange = [100 199];  % placeholder, depends on CALDB
                    RawYRange = [100 199];  % placeholder
                otherwise
                    error('Unknown PC Window: %s', Window);
            end

        case 'xwt'   % Windowed Timing mode
            switch lower(Window)
                case 'w2'  % standard WT window
                    RawXRange = [0 599];
                    RawYRange = [0 1];   % collapsed to 2 rows
                case 'w1'  % small WT window (rare)
                    RawXRange = [0 299]; % placeholder
                    RawYRange = [0 1];
                otherwise
                    error('Unknown WT Window: %s', Window);
            end

        case {'xlr','xsl','xst'}  % Low-rate / Settling / special PC-like modes
            % Usually full-frame
            RawXRange = [0 599];
            RawYRange = [0 599];

        otherwise
            error('Unknown Instrument: %s', Instrument);
    end
    Result = [RawXRange, RawYRange];
end

