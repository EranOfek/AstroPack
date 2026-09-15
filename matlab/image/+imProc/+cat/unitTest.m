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

    % imProc.cat.fillEmptyCatColumns + AstroImage/write1 'WriteEmptyCat' (issue #1226)
    % A crop whose extraction produced no catalog (0x0, no column names)
    % borrows the column set of its neighbours, so that it can be written
    % as a zero-row Cat product; write1 does so only when asked.
    ColNames = {'X','Y','MAG_APER_3'};
    C1 = AstroCatalog({rand(5,3)}, 'ColNames',ColNames, 'ColUnits',{'pix','pix','mag'});
    A1 = AstroImage({single(rand(10))}); A1.CatData = C1.copy;
    A2 = AstroImage({single(rand(10))});                       % 0x0 catalog
    [Arr, Filled] = imProc.cat.fillEmptyCatColumns([A1, A2]);
    if ~isequal(Filled(:).', [false true])
        error('Problem with imProc.cat.fillEmptyCatColumns: Filled flag');
    end
    if ~isequal(size(Arr(2).CatData.Catalog), [0 3]) || ~isequal(Arr(2).CatData.ColNames(:).', ColNames) || ...
       ~isequal(size(Arr(1).CatData.Catalog), [5 3])
        error('Problem with imProc.cat.fillEmptyCatColumns: catalog shapes');
    end
    OutDir = fullfile(tempdir, 'unitTest_fillEmptyCat');
    if isfolder(OutDir), rmdir(OutDir,'s'); end
    Arr(2).HeaderData.replaceVal({'EXPTIME'},{20});
    F1 = fullfile(OutDir,'crop_Cat.fits');
    Arr(2).write1(F1, 'Cat', 'FileType','fits', 'WriteHeader',true, 'MkDir',true, 'WriteMethodTables','MexHeader', 'WriteEmptyCat',true);
    if ~isfile(F1)
        error('Problem with AstroImage/write1: zero-row Cat not written with WriteEmptyCat');
    end
    Info = fitsinfo(F1);
    if Info.BinaryTable(1).Rows~=0 || Info.BinaryTable(1).NFields~=3
        error('Problem with AstroImage/write1: zero-row Cat has wrong shape on disk');
    end
    F2 = fullfile(OutDir,'crop_Cat_default.fits');
    Status = Arr(2).write1(F2, 'Cat', 'FileType','fits', 'WriteHeader',true, 'MkDir',true, 'WriteMethodTables','MexHeader');
    if isfile(F2) || isempty(Status)
        error('Problem with AstroImage/write1: zero-row Cat must not be written by default');
    end
    % a catalog with no columns at all is refused, and a column-name /
    % column-count mismatch is refused without leaving a truncated file
    F3 = fullfile(OutDir,'nocol_Cat.fits');
    A4 = AstroImage({single(rand(10))});                       % 0x0 catalog, no column names
    Status = A4.write1(F3, 'Cat', 'FileType','fits', 'WriteHeader',true, 'MkDir',true, 'WriteEmptyCat',true);
    if isfile(F3) || isempty(Status) || ~contains(Status(1).Msg,'not saved')
        error('Problem with AstroImage/write1: column-less catalog must be refused');
    end
    A3 = AstroImage({single(rand(10))}); A3.CatData = C1.copy; A3.CatData.Catalog = zeros(0,0);
    F4 = fullfile(OutDir,'mismatch_Cat.fits');
    Status = A3.write1(F4, 'Cat', 'FileType','fits', 'WriteHeader',true, 'MkDir',true, 'WriteEmptyCat',true);
    if isfile(F4) || isempty(Status)
        error('Problem with AstroImage/write1: column-name/column mismatch must be refused');
    end
    rmdir(OutDir,'s');
    
    cd(PWD);
    
    %io.msgStyle(LogLevel.Test, '@passed', 'imProc.CAT test passed');
    Result = true;    
end
    
    