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


    Result = true;

end