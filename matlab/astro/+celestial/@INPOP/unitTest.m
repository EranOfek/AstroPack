function Result = unitTest()
    % INPOP.unitTest
    
    io.msgLog(LogLevel.Test, 'INPOP test started');
    I = celestial.INPOP();

    % Download Constants

    ConstantFileName = 'inpop21a_TDB_m100_p100_asc_header.asc';
    I.download('URL',ConstantFileName,'Untar',false);
    % Download works but constant file can only be downloaded after initializing the class - and
    % then the 'Constant' field is empty until the class is cleared.
   
    assert(~isempty(I.Constant),'Could not load constants')
    
    % Download file
    Filename = I.inpopFileName;
    I.download('URL',Filename,'Untar',false);

    assert(isfile([I.Location,Filename]))

    % Populate tables from ascii files
    I = I.populateTables({'Sun','Earth'},'FileType','asc');
    assert(~isempty(I.PosTables.Sun) && ~isempty(I.PosTables.Earth))

    % calculate position and compare with VSOP87
    JD = mean(I.RangeShort);
    PosINPOP = getPos(I, 'Earth',JD);
 
    PosVSOP87 = celestial.SolarSys.calc_vsop87(JD,'Earth','e','E');

    assert(mean(abs(PosINPOP-PosVSOP87))<1e-3,'INPOP and VSOP87 do not agree')

    % Convert ascii to .mat files
    I.convertAscii2mat('TimePeriod',{'100'}); % add option to convert only if file exists
    assert(~isempty(dir([I.Location,'*.mat'])));

    % Populate velocity tables from mat files
    I = I.populateTables({'Jup'},'FileType','mat','FileData','vel');

    % calculate velocity and compare with VSOP87
    VelINPOP = getPos(I,'Jup',JD,'IsPos',false);
    [~,VelVSOP87] = celestial.SolarSys.calc_vsop87(JD,'Jupiter','e','E');
    assert(mean(abs(VelINPOP-VelVSOP87))<1e-3,'INPOP and VSOP87 do not agree')

    io.msgLog(LogLevel.Test, 'INPOP test passed');
    Result = true;
end

