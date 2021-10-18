function Result = unitTest()
    % ImagePath.unitTest        
    io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');

    % genFile
     ip = ImagePath();
     fprintf('%s\n', ip.needUuid());
     [ExpectedPath, ExpectedFileName] = ip.setTestData();
     FileName = ip.genFile('Time', ip.Time, 'FullPath', false);
     assert(strcmp(FileName, ExpectedFileName),'parsed FileName and expected FileName are different');
     disp(FileName);
    
    % Test copying Handle class
    fprintf('ip.Uuid = %s\n', ip.needUuid());        
    ip1 = ip;
    ip2 = ip.copy();
    fprintf('ip1.Uuid = %s\n', ip1.needUuid());    
    fprintf('ip2.Uuid = %s\n', ip2.needUuid());    
    
    % genPath
     ip = ImagePath();
     [ExpectedPath, ExpectedFileName] = ip.setTestData();
     ip.Level = 'proc';
     ip.SubDir = 'subdir';
     Path = ip.genPath('Time', ip.Time);
     disp(Path);
     %assert(strcmp(Path, ExpectedPath), 'parsed Path and expected Path are different');

     Full = ip.genFile('Time', ip.Time, 'FullPath', true);
     ExpectedFull = [ExpectedPath, ExpectedFileName];
     %assert(strcmp(Full, ExpectedFull), 'parsed FullPath and expected FullPath are different');
            
%     % writeToStruct not implemented yet
%     IP = ImagePath();
%     FileName = IP.makeFileName();
%     assert(~isempty(FileName));
%     IP.setTestData();
%     s = IP.writeToStruct();
%     disp(s);

    testProjName     = {'LAST','USAT','USAT','USAT','USAT','USAT','USAT','USAT','USAT','USAT','USAT','USAT','USAT'};
    testTime         = {'2021-09-09T12:34:56.789'};
    testTimeZone     = {2};
    testFilter       = {'clear'};
    testFieldID      = {'fld'};
    testCounter      = {'cnt'};
    testCCDID        = {'ccdid'};
    testCropID       = {'crop'};
    testType         = {'sci','sci','sci','sci','domeflat','twflat','bias','dark','skyflat','fringe','sci','sci','sci'};
    testLevel        = {'raw','log','proc','stacked','ref','coadd','calib','calib','calib','calib','proc','proc','proc'};
    testSubLevel     = {'','n','s','sp','d','t','r','m','Fn','Rn','Ln','Gn','Hn','n'};
    testProduct      = {'im','back','var','imflag','exp','nim','psf','cat','spec','im','im','im','im'};
    testVersion      = {'1'};
    testFileType     = {'fits','hdf5','fits.gz','fits','fits','fits','fits','fits','fits','fits','fits','fits','fits'};
    testSubDir       = {'subdir'};
    
    testBasePath     = {'/home/last'};
    testDataDir      = {'data'};
    
    testExpectedPath     = {...
        '/home/last/data/2021/09/09/raw/subdir/',...
        '/home/last/data/2021/09/09/log/subdir/',...
        '/home/last/data/2021/09/09/proc/subdir/',...
        '/home/last/data/2021/09/09/stacked/subdir/',...
        '/home/last/data/ref/1/subdir/',...
        '/home/last/data/coadd/1/subdir/',...
        '/home/last/data/calib/r/subdir/',...
        '/home/last/data/calib/m/subdir/',...
        '/home/last/data/calib/Fn/subdir/',...
        '/home/last/data/calib/Rn/subdir/',...
        '/home/last/data/2021/09/09/proc/subdir/',...
        '/home/last/data/2021/09/09/proc/subdir/',...
        '/home/last/data/2021/09/09/proc/subdir/',...
    };

    testExpectedFileName = {...
        'LAST_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_raw_im_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_log.n_back_1.hdf5',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_proc.s_var_1.fits.gz',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_stacked.sp_imflag_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_domeflat_ref.d_exp_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_twflat_coadd.t_nim_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_bias_calib.r_psf_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_dark_calib.m_cat_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_skyflat_calib.Fn_spec_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_fringe_calib.Rn_im_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_proc.Ln_im_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_proc.Gn_im_1.fits',...
        'USAT_20210909.123456.789_clear_fld_cnt_ccdid_crop_sci_proc.Hn_im_1.fits',...
    };

    
    for i=1:length(testExpectedPath)
        ip = ImagePath();
        
        ip.ProjName        = testProjName{i};
        ip.Time            = testTime{1}; 
        ip.TimeZone        = testTimeZone{1}; 
        ip.Filter          = testFilter{1}; 
        ip.FieldID         = testFieldID{1}; 
        ip.Counter         = testCounter{1}; 
        ip.CCDID           = testCCDID{1}; 
        ip.CropID          = testCropID{1}; 
        ip.Type            = testType{i}; 
        ip.Level           = testLevel{i}; 
        ip.SubLevel        = testSubLevel{i}; 
        ip.Product         = testProduct{i}; 
        ip.Version         = testVersion{1};
        ip.FileType        = testFileType{i};
        ip.SubDir          = testSubDir{1};
        
        ip.BasePath        = testBasePath{1};
        ip.DataDir         = testDataDir{1};
        
        ExpectedFileName   = testExpectedFileName{i};
        ExpectedPath       = testExpectedPath{i};
        ExpectedFull       = strcat(ExpectedPath, ExpectedFileName);
        
        FileName = ip.genFile('FullPath', false);
        assert(strcmp(FileName, ExpectedFileName), 'parsed FileName and expected FileName are different');
        
        Path = ip.genPath();
        assert(strcmp(Path, ExpectedPath), 'parsed Path and expected Path are different');
        
        Full = ip.genFile('FullPath', true);
        assert(strcmp(Full, ExpectedFull), 'parsed FullPath and expected FullPath are different');
    end
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
    Result = true;
end

