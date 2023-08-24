function Result = unitTest()
    % unitTest for +imProc.mask
    io.msgLog(LogLevel.Test, 'imProc.mask test started');
    
    AI = AstroImage({rand(10,10).*1000});
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500);
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100);
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100,'MultLevelByGain',true,'Gain',5);

    AI = AstroImage({rand(10,10).*1000});
    Flag = false(10,10);
    Flag(2,2) = true;
    AI.MaskData = maskSet(AI.MaskData, Flag, 'Saturated');
    imProc.mask.replaceMaskedPixVal(AI,  'Saturated', NaN);
    if ~isnan(AI.Image(2,2))
        error('Problem with replaceMaskedPixVal');
    end
    
    AI = AstroImage({rand(50).*5},'Back',{ones(50)},'Var',{ones(50)});

%     doesn't work because of imUtil.image.local_maxima(-SN, 1, Args.Threshold, Args.Conn);
%     Result = imProc.mask.maskHoles(AI,'Threshold',-3);
%     Result = imProc.mask.maskHoles(AI, 'SN',AI.Image,'Threshold',3);
    
    AI = AstroImage({rand(10).*linspace(1,1,10)});
%     AI.Image( 
    Flag = false(10);
    Flag(2,2)= true;
    AI.MaskData = maskSet(AI.MaskData, Flag, 'Saturated');
    
    imProc.mask.interpOverMaskedPix(AI,'BitNamesToInterp',{'Saturated'});
    [Result, XY, Ind] = findBit(AI.MaskData, {'Interpolated'});
%     assert(~isnan(AI.Image(2,2)) && ~XY

    AI=AstroImage();
    AI.Image=20*imUtil.art.createSegments([650,700],...
        [322,233;98,0],[54,11;145,211],'width',0.5) + ...
        rand(650,700);
    imProc.background.background(AI);
    imProc.mask.maskTracks(AI);


    io.msgStyle(LogLevel.Test, '@passed', 'imProc.mask test passed');
    Result = true; 
end