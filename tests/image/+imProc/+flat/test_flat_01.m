function Result = unitTest()
    % unitTest for imProc.flat
    % Example : Result = imProc.flat.unitTest;
    io.msgLog(LogLevel.Test, 'imProc.flat test started');

    AI = AstroImage({rand(100,100)});

    % isFlat
    [Result,Flag] = imProc.flat.isFlat(AI);
    if Result
        error('Problem with isFlat');
    end

    insertKey(AI.HeaderData, {'IMTYPE','Flat',''});
    [Result,Flag] = imProc.flat.isFlat(AI);
    if ~Result
        error('Problem with isFlat');
    end
    
    % flat
    AI=AstroImage({rand(100,100), rand(100,100), rand(100,100), rand(100,100), rand(100,100)});
    Res = imProc.flat.flat(AI,'FilterKey',[], 'IsFlat',[]); %should't this function add to header data {'IMTYPE','Flat',''})?

    AI.setKeyVal('FILTER','filter');
    Res2 = imProc.flat.flat(AI, 'IsFlat',[]);  
    
    if ~all(Res.Image==Res2.Image,'all')
        error('Problem with flat');
    end
    
    AI=AstroImage({ones(100,100), ones(100,100)});
    AI(1).Image(1,1) = 0;AI(2).Image(1,1) = 0;
    Res = imProc.flat.flat(AI,'FilterKey',[],'FlatLowVal_Threshold',4, 'IsFlat',[]);
    [ResultNaN, ~,~] = findBit(Res.MaskData, {'NaN'});
    [Resultlow, ~,~] = findBit(Res.MaskData, {'NaN','FlatLowVal'},'Method','any');
    if ~ResultNaN(1,1) && ~all(Resultlow,'all')
        error('Problem with flat');
    end
    
    % deflat
    %AI=AstroImage({rand(100,100), rand(100,100), rand(100,100), rand(100,100), rand(100,100)});
    %Res = imProc.flat.deflat(AI,'FlatArgs',{'FilterKey',[]},'IsFlat',[1 1 0 0]); 
    
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.flat test passed'); 
   
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.flat test passed');    
    Result = true;
end
