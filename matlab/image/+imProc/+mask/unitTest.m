function Result = unitTest()
    % unitTest for +imProc.mask
    % Example: imProc.mask.unitTest
    
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
    
    Result = true;
    
end