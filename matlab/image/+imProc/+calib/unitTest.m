function Result = unitTest
    % unitTest for imProc.calib
    % Example: imProc.calib.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'imProc.calib test started')
            
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);
    
    % gainCorrect
    AI = AstroImage({rand(10,10)});
    imProc.calib.gainCorrect(AI);
    
    AI = AstroImage({ones(10,10)});
    GainVal = 1.6;
    AI.setKeyVal('GAIN', GainVal);
    imProc.calib.gainCorrect(AI);
    if ~all(AI.Image == 1./GainVal)
        error('Problem with gainCorrect');
    end
    
    
    cd(PWD);

    io.msgStyle(LogLevel.Test, '@passed', 'imProc.calib test passed')
    Result = true;

end
