function Result = unitTest()
    % unitTest for imProc.cat
    % Example: imProc.cat.unitTest
   
    %io.msgLog(LogLevel.Test, 'imProc.CAT test started');

    RAD = 180./pi;
    
    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    
    % applyProperMotion
    C = catsHTM.cone_search('GAIADR2',1,1,1000,'OutType','astrocatalog');
    InE  = 2015;
    OutE = 2021;
    Result = imProc.cat.applyProperMotion(C, InE, OutE, 'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',false, 'CreateNewObj',true);

    % check PM in RA
    DRA = (Result.getCol('RA') - C.getCol('RA')).*RAD.*3600.*1000.*cos(1);  % mas
    PMRA = C.getCol('PMRA').*(OutE-InE);    % mas
    
    Plx = getCol(Result,'Plx');
    Plx(Plx<0.1) = 0.1;
    %[abs(DRA - PMRA), Plx]
    OK = abs(DRA - PMRA)<max(1,4.*Plx);  % compare - not clear why errors reach ~1 mas
    OK(isnan(DRA - PMRA)) = true;
    if ~all(OK) 
        error('Problem with apply PM in RA')
    end
    
    % check PM in Dec
    DDec = (Result.getCol('Dec') - C.getCol('Dec')).*RAD.*3600.*1000;  % mas
    PMDec = C.getCol('PMDec').*(OutE-InE);    % mas
    Plx = getCol(Result,'Plx');
    Plx(Plx<0.1) = 0.1;
    OK = abs(DDec - PMDec)<max(1,4.*Plx);  % compare - not clear why errors reach ~1 mas
    OK(isnan(DDec - PMDec)) = true;
    if ~all(OK) 
        error('Problem with apply PM in Dec')
    end
    
    % imProc.cat.filterForAstrometry
    [Cat,Ref] = imProc.cat.filterForAstrometry(rand(100,3).*1000,rand(200,3).*1000);
    
    % imProc.cat.fitPeakMultipleColumns
    X=rand(100,3);
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(X, 'Pos',[1 2 3])
    AC = AstroCatalog({X}, 'ColNames',{'SN_1','SN_2','SN_3'});
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AC, 'Pos',[1 2 3])
    AI=AstroImage('PTF_Cropped.fits');
    imProc.sources.findMeasureSources(AI, 'PsfFunPar',{[0.1; 1.2; 3]});
    [FitRes, Result] = imProc.cat.fitPeakMultipleColumns(AI, 'Pos',[0.1 1.2 3])

    
    cd(PWD);
    
    %io.msgStyle(LogLevel.Test, '@passed', 'imProc.CAT test passed');
    Result = true;    
end
    
    