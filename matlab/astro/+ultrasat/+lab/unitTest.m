function Result = unitTest()
    % unitTest for the ultrasat.lab package (DESY lab TIFF/sidecar readers)
    % Example: ultrasat.lab.unitTest
    io.msgStyle(LogLevel.Test, '@start', 'ultrasat.lab test started');

    % parseNum
    assert(ultrasat.lab.parseNum('-50,0')==-50);
    assert(ultrasat.lab.parseNum('1,0E-05')==1e-5);
    assert(ultrasat.lab.parseNum(7)==7);
    [V, IsNum] = ultrasat.lab.parseNum('disable');
    assert(strcmp(V,'disable') && ~IsNum);
    [V, IsNum] = ultrasat.lab.parseNum('0x21000101');
    assert(ischar(V) && ~IsNum);

    % parseFrameName
    P = ultrasat.lab.parseFrameName({'/a/b/Ultrasat_BSI_L_TH02954_W08_D02_PTC_int_hr_#12_ZE_0005.tif', 'other.tif'});
    assert(P(1).Match && strcmp(P(1).Lot,'TH02954') && P(1).Wafer==8 && P(1).Device==2);
    assert(strcmp(P(1).Test,'PTC_int_hr') && P(1).Step==12 && strcmp(P(1).FrameType,'ZE') && P(1).FrameIndex==5);
    assert(~P(2).Match);

    % synthetic device directory
    TmpDir = tempname;
    Base   = 'Ultrasat_BSI_L_TH00001_W03_D07';
    Dev    = fullfile(TmpDir, 'LOT_TH00001_W03_D07');
    Test   = 'PTC_int_hr';
    mkdir(fullfile(Dev, Test));
    mkdir(fullfile(Dev, 'Calib'));
    Cleanup = onCleanup(@() rmdir(TmpDir, 's'));

    Fid = fopen(fullfile(Dev, [Base, '_Result.txt']), 'w');
    fprintf(Fid, 'Product Name\tUltrasat_BSI\t\t\t\t\t\n');
    fprintf(Fid, 'Time\t27.08.26 06:33:10\t\t\t\t\t\n');
    fprintf(Fid, 'Operator Name\tXYZ\t\t\t\t\t\n');
    fprintf(Fid, 'Run No\t3\t\t\t\t\t\n');
    fprintf(Fid, 'LOT ID\tTH00001\t\t\t\t\t\n');
    fprintf(Fid, 'Wafer ID\t03\t\t\t\t\t\n');
    fprintf(Fid, 'Tester Temperature\t-50,0\t\t\t\t\t\n');
    fprintf(Fid, 'Device No\t00007\t\t\t\t\t\n');
    fprintf(Fid, '0000\tDate\t1\t1\tdisable\t27.08.2026\tdisable\n');
    fprintf(Fid, '1200\tVDD_GND_Shorts_VDDD_U [V]\t8\t1\t-0,300\t-0,249\t-0,200\n');
    fprintf(Fid, '16200\tPTC_int_hr_VDDD_U [V]\t1\t1\tdisable\t3,302\tdisable\n');
    fprintf(Fid, 'Total Time\t8870\t\t\t\t\t\n');
    fprintf(Fid, 'Soft Bin\t1\t\t\t\t\t\n');
    fprintf(Fid, 'Pass\t1\t\t\t\t\t\n');
    fclose(Fid);

    Fid = fopen(fullfile(Dev, [Base, '.log']), 'w');
    fprintf(Fid, 'Time\tID\tText\n');
    fprintf(Fid, '04:05:20 27.08.2026\t   0\tTest of LOT#TH00001 Wafer#3 DUT#000007 started\n');
    fprintf(Fid, '04:07:13 27.08.2026\t16200\tPTC_int_hr started\n');
    fprintf(Fid, '06:33:10 27.08.2026\t16200\tPTC_int_hr finished\n');
    fclose(Fid);

    Fid = fopen(fullfile(Dev, 'Calib', [Base, '_Calib_adc.txt']), 'w');
    fprintf(Fid, 'Description:\tVDDA@3,301 Clock@44,44\nTarget Value:\t-0,000140\nBest Match:\t2\nMeasured Values:\n-0,000098\n-0,000141\n-0,000294\n');
    fclose(Fid);

    Config = {'Time', '2026.08.27 06:33:07';
              'Chuck Temperature', '-50,0';
              'PTC_ExpTime', '[1]{15}';
              'Dark_ExpTime', '[3]{15;30;60}';
              'Bright_Intensity', '[2]{1,0E-05;2,0E-05}';
              'Dark_Frames', 2;
              'Bright_Frames', 1;
              'ZeroExp_Frames', 1;
              'zVDDA', '3,3';
              'zDUT_ADC_CAP', 5};
    writecell(Config, fullfile(Dev, Test, 'PTC_Config.xlsx'));

    [X, Y] = meshgrid(1:20, 1:10);
    Im = uint16(100*Y + X);
    Names = {'#02_B_0001', '#01_B_0001', '#03_D_0002', '#03_D_0001', '#01_D_0001', '#01_ZE_0001'};
    for In=1:1:numel(Names)
        imwrite(In*Im, fullfile(Dev, Test, sprintf('%s_%s_%s.tif', Base, Test, Names{In})), 'tif', 'Compression','none');
    end

    % sidecar readers
    R = ultrasat.lab.readResult(fullfile(Dev, [Base, '_Result.txt']));
    assert(strcmp(R.Info.LOTID,'TH00001') && R.Info.WaferID==3 && R.Info.DeviceNo==7 && R.Info.TesterTemperature==-50);
    assert(height(R.Tests)==3 && R.Tests.Value(2)==-0.249 && isnan(R.Tests.Low(3)));
    assert(R.Trailer.Pass==1 && R.Trailer.TotalTime==8870);
    L = ultrasat.lab.readLog(fullfile(Dev, [Base, '.log']));
    assert(height(L)==3 && L.ID(2)==16200 && hour(L.Time(2))==4);
    C = ultrasat.lab.readPTCConfig(fullfile(Dev, Test, 'PTC_Config.xlsx'));
    assert(C.PTC_ExpTime==15 && isequal(C.Dark_ExpTime,[15 30 60]) && isequal(C.Bright_Intensity,[1e-5 2e-5]));
    assert(C.ChuckTemperature==-50 && C.zVDDA==3.3 && C.zDUT_ADC_CAP==5);

    % readPTC: all frames, sorted B, D, ZE / step / index (raw frames, as stored)
    [AI, Frames, S] = ultrasat.lab.readPTC(Dev, 'Gain','raw', 'Orient','tiff');
    assert(numel(AI)==6 && height(Frames)==6);
    assert(isequal(Frames.FrameType.', {'B','B','D','D','D','ZE'}));
    assert(isequal(Frames.Step.', [1 2 1 3 3 1]) && isequal(Frames.FrameIndex.', [1 1 1 1 2 1]));
    assert(isequal(Frames.ExpTime.', [15 15 15 60 60 0]) && isequal(Frames.Intensity.', [1e-5 2e-5 0 0 0 0]));
    assert(isequal(AI(1).Image, 2*Im) && isequal(AI(6).Image, 6*Im));   % '#01_B_0001' was written 2nd
    H = AI(4).HeaderData;
    assert(strcmp(H.getVal('LOTID'),'TH00001') && H.getVal('WAFERID')==3 && H.getVal('DEVICE')==7);
    assert(strcmp(H.getVal('FRMTYPE'),'D') && H.getVal('STEP')==3 && H.getVal('EXPTIME')==60 && H.getVal('INTENS')==0);
    assert(H.getVal('CHUCKTMP')==-50 && H.getVal('VDDA')==3.3 && H.getVal('CAL_ADC')==2 && H.getVal('PASS')==1);
    assert(strcmp(H.getVal('TESTSTRT'),'2026-08-27T04:07:13') && strcmp(H.getVal('TESTNAME'),Test));
    assert(~isempty(H.getVal('DATE-OBS')) && H.getVal('NAXIS1')==20 && strcmp(H.getVal('GAINSEL'),'raw'));
    assert(S.Calib.adc.BestMatch==2 && numel(S.Calib.adc.MeasuredValues)==3);

    % readPTC: filters, headers only, CCDSEC, FlipUD (raw)
    [AI, Frames] = ultrasat.lab.readPTC(Dev, 'FrameType','D', 'Step',3, 'ReadImage',false);
    assert(numel(AI)==2 && all(strcmp(Frames.FrameType,'D')) && isempty(AI(1).Image));
    assert(AI(1).HeaderData.getVal('EXPTIME')==60 && AI(1).HeaderData.getVal('NAXIS1')==10);
    AI = ultrasat.lab.readPTC(Dev, 'FrameType',{'B','ZE'}, 'FrameIndex',1, 'CCDSEC',[2 5 3 4], 'Gain','raw', 'Orient','tiff');
    assert(numel(AI)==3 && isequal(AI(1).Image, 2*Im(3:4, 2:5)));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'FlipUD',true, 'Gain','raw', 'Orient','tiff');
    assert(isequal(AI.Image, flipud(6*Im)));

    % readPTC: gain halves and orientation
    High = Im(:, 11:20);  Low = Im(:, 1:10);
    D    = rot90(High.', 2);                                    % DESY orientation of the high-gain half
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE');           % defaults: high, desy
    assert(isequal(AI.Image, 6*D) && isequal(size(AI.Image), [10 10]));
    H = AI.HeaderData;
    assert(strcmp(H.getVal('GAINSEL'),'high') && strcmp(H.getVal('ORIENT'),'desy') && strcmp(H.getVal('RAWSEC'),'[11:20,1:10]'));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'Gain','low', 'Orient','tiff');
    assert(isequal(AI.Image, 6*Low) && strcmp(AI.HeaderData.getVal('RAWSEC'),'[1:10,1:10]'));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'Gain','low');
    assert(isequal(AI.Image, 6*rot90(Low.', 2)));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'CCDSEC',[2 5 3 4]);            % section in the DESY orientation
    assert(isequal(AI.Image, 6*D(3:4, 2:5)) && AI.HeaderData.getVal('NAXIS1')==4 && AI.HeaderData.getVal('NAXIS2')==2);
    assert(strcmp(AI.HeaderData.getVal('CCDSEC'),'[2:5,3:4]'));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'CCDSEC',[2 5 3 4], 'FlipUD',true);
    assert(isequal(AI.Image, flipud(6*D(3:4, 2:5))));
    AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'Gain','raw');                  % whole TIFF, DESY orientation
    assert(isequal(AI.Image, 6*rot90(Im.', 2)));

    io.msgStyle(LogLevel.Test, '@passed', 'ultrasat.lab test passed');
    Result = true;
end
