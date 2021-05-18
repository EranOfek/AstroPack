function Result = getTestDataDir()
    %
    % Eaxmple: DataSampleDir = tools.os.getTestDataDir
   
    Dir     = fileparts(mfilename('fullpath'));
    DataDir = sprintf('%s%s%s%s%s','data',filesep,'test_images',filesep,'fits_samples');
    Result  = sprintf('%s%s..%s..%s..%s..%s%s',Dir,filesep,filesep,filesep,filesep,filesep,DataDir);
    
end