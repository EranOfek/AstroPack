function Result = unitTest()
    % UltrasatPerf2GUI unitTest
    
    io.msgLog(LogLevel.Test, 'UltrasatPerf2GUI test started');

    % Create object and load UP from file (P90_UP_test_60_ZP_Var_Cern_21.mat)
    UG = UltrasatPerf2GUI();
    assert(~isempty(UG.UP));
    
    % getSources()
    [Pickles, BlackBodyTemperature] = UG.getSources();
    assert(numel(Pickles) > 0);
    assert(numel(BlackBodyTemperature) > 0);
    
    %
    Rdeg = UG.getRdeg();
    assert(numel(Rdeg) > 0);

    % Prepare Args
    Args = struct;
    Args.ExpTime = 300;
    Args.NumImages = 3;
    Args.R = 1;                 % Note that this is the Index and not the value
    Args.Source = 'A 0.0 V';
    Args.SnrMagnitude = 22;
    Args.LimitingMagnitude = 5;
            
    % Calculate SNR
    ArgsCell = namedargs2cell(Args);
    Result = UG.calcSNR(ArgsCell{:});

    % Compare results with the expected values for Args above
    % We use '>' to avoid double comparison issues
    assert(Result.ResultSnr > 6.41);
    assert(Result.ResultLimitingMagnitude > 20.73);
    
    % Make sure we don't have errors
    assert(~startsWith(Result.message, 'error: '));
            
    io.msgLog(LogLevel.Test, 'UltrasatPerf2GUI test passed');
    Result = true;
end
