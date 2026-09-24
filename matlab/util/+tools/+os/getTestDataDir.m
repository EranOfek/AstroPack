function Result = getTestDataDir()
    % Return the path containing the test images used by the unitTest
    % Author : Eran Ofek (May 2022)
    % Eaxmple: DataSampleDir = tools.os.getTestDataDir
   
    Dir     = fileparts(mfilename('fullpath'));
    Result  = fullfile(Dir,'..','..','..','..','..','data','TestImages','unitTest');
    %DataDir = sprintf('%s%s%s%s%s','data',filesep,'test_images',filesep,'fits_samples');
    %Result  = sprintf('%s%s..%s..%s..%s..%s%s',Dir,filesep,filesep,filesep,filesep,filesep,DataDir);
    Result  = tools.os.relPath2absPath(Result);
    
end