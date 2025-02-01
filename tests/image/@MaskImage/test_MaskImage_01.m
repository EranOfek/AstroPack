function Result = unitTest()
    M = MaskImage;

    M = MaskImage({uint32(zeros(3,3))});
    MI=MaskImage;

    MI.Dict=BitDictionary('BitMask.Image.Default');
    Flag = false(3,3); Flag(1,2)=true;
    Result = MI.maskSet(Flag,'Saturated');
    [BitInd,BitDec] = MI.Dict.name2bit('Saturated');
    if Result.Image(1,2)~=BitDec
        error('set bit was not sucessful');
    end
    Result = Result.maskSet(Flag,'Streak');
    [BitInd,BitDec2] = Result.Dict.name2bit('Streak');
    if Result.Image(1,2)~=(BitDec+BitDec2)
        error('set bit was not sucessful (2)');
    end

    IC=MaskImage({uint32(ones(100,100))});
    Result = bitwise_cutouts(IC, [1 1; 2 2; 10 10; 30 30], 'or');


    Result = true;
end
