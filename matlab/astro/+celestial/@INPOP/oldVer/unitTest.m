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

    %% Test Vel / Pos
    JD = 2451545 + (-100:10:1e4);
    I = celestial.INPOP.init;
    VelINPOP = getPos(I,'Jup',JD,'IsPos',false);
    [~,VelVSOP87] = celestial.SolarSys.calc_vsop87(JD,'Jupiter','e','E');
    assert(mean(abs(VelINPOP-VelVSOP87),"all")<1e-8,'INPOP and VSOP87 do not agree')
    
    PosINPOP = getPos(I,'Mar',JD);
    [PosVSOP87] = celestial.SolarSys.calc_vsop87(JD,'Mars','e','E');
    assert(mean(abs(PosINPOP-PosVSOP87),"all")<1e-5,'INPOP and VSOP87 do not agree')
        
    %% Test  TimeSpan
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

