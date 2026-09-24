function Result = unitTest()
    % INPOP.unitTest
    % Example: celestial.INPOP.unitTest
    
    %io.msgLog(LogLevel.Test, 'INPOP test started');
    I = celestial.INPOP();

    % Download Constants

    ConstantFileName = 'inpop21a_TDB_m100_p100_asc_header.asc';
    %I.download('URL',ConstantFileName,'Untar',false);
    
    % Download works but constant file can only be downloaded after initializing the class - and
    % then the 'Constant' field is empty until the class is cleared.
   
    if isempty(I.Constant)
        error('Could not load constants');
    end
    
    % Download file
    %Filename = I.inpopFileName;
    %I.download('URL',Filename,'Untar',false);

    %assert(isfile([I.Location,Filename]))

    % Populate tables from ascii files
    I = I.populateTables({'Sun','Ear'},'FileType','asc');
    assert(~isempty(I.PosTables.Sun) && ~isempty(I.PosTables.Ear))

    % calculate position and compare with VSOP87
    JD = mean(I.RangeShort);
    PosINPOP = getPos(I, 'Ear',JD);
 
    PosVSOP87 = celestial.SolarSys.calc_vsop87(JD,'Earth','e','E');

    assert(mean(abs(PosINPOP-PosVSOP87))<1e-3,'INPOP and VSOP87 do not agree')

    % Convert ascii to .mat files
    %I.convertAscii2mat('TimePeriod',{'100'}); % add option to convert only if file exists
    %assert(~isempty(dir([I.Location,'*.mat'])));

    % Populate velocity tables from mat files
    I = I.populateTables({'Jup'},'FileType','mat','FileData','vel');

    % calculate velocity and compare with VSOP87
    VelINPOP = getPos(I,'Jup',JD,'IsPos',false);
    [~,VelVSOP87] = celestial.SolarSys.calc_vsop87(JD,'Jupiter','e','E');
    assert(mean(abs(VelINPOP-VelVSOP87))<1e-3,'INPOP and VSOP87 do not agree')

    I = celestial.INPOP();
    Objects = {'Sun', 'Mer', 'Ven', 'Ear', 'EMB', 'Moo', 'Mar', 'Jup', 'Sat', 'Ura', 'Nep', 'Plu', 'Lib'};
    Nobj = numel(Objects);
    Passed = true;
    for i=1:Nobj
        try
            I.populateTables(Objects{i});   
            try
                PosINPOP = getPos(I, Objects{i},JD);
            catch
                disp(['Did not calculate position of ',Objects{i}]);
                Passed = false;
            end
        catch
            disp(['Did not populate ',Objects{i}]);
            Passed = false;
        end
    end
    assert(Passed,'Failed in populating and calculating objects')
    % Moon ('Moo') is not initialized and gives an error in isPopulated
    % 
    
    % Test  TimeSpan
    JD = 2460000.1+(0:0.1:100)';
    IN=celestial.INPOP;
    IN.populateAll('TimeSpan',[2459000, 2461000],'PopForce',true);
    xx=IN.getPos('Ear',JD);
    IN.populateAll('PopForce',true);                              
    xn=IN.getPos('Ear',JD);
    if max(abs(xx-xn),[],'all')>eps
        error('celestial.INPOP failed to evaluate ephemeris when TimeSpan option is used');
    end


    

    %io.msgLog(LogLevel.Test, 'INPOP test passed');
    Result = true;
end

