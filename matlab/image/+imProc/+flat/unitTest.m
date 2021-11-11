function Result = unitTest()
    % unitTest for imProc.image.Flat
    % Example : Result = imProc.image.Flat.unitTest;


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
    Res = imProc.flat.flat(AI,'FilterKey',[]); %should't this function add to header data {'IMTYPE','Flat',''})?

    AI.setKeyVal('FILTER','filter');
    Res2 = imProc.flat.flat(AI);  
    
    if ~all(Res.Image==Res2.Image,'all')
        error('Problem with flat');
    end
    
    % deflat
    AI=AstroImage({rand(100,100), rand(100,100), rand(100,100), rand(100,100), rand(100,100)});
    Res = imProc.flat.deflat(AI,'FlatArgs',{'FilterKey',[]},'IsFlat',@imProc.flat.isFlat); 
    
    Result = true;

end