function Result = perfTestMat
    % Low level perfTest for the Matlab matrix
    io.msgStyle(LogLevel.Test, '@sart', 'AstroImage perfTestMat started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);



    cd(PWD);           
    io.msgStyle(LogLevel.Test, '@passed', 'AstroImage perfTestMat passed')
    Result = true;
end
