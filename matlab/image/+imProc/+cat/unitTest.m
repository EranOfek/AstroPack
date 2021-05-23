function Result = unitTest()
    % unitTest for imProc.cat
    % Example: imProc.cat.unitTest
   
    RAD = 180./pi;
    
    % applyProperMotion
    C = catsHTM.cone_search('GAIADR2',1,1,1000,'OutType','astrocatalog');
    InE  = 2015;
    OutE = 2021;
    Result = imProc.cat.applyProperMotion(C, InE, OutE, 'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',0);

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
    
    
    
end
    
    