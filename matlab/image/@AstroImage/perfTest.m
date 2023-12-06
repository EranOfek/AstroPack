function Result = perfTest
    % perfTest for the AstroImage class
    io.msgStyle(LogLevel.Test, '@sart', 'AstroImage perfTest started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);



    cd(PWD);           
    io.msgStyle(LogLevel.Test, '@passed', 'AstroImage perfTest passed')
    Result = true;
end
