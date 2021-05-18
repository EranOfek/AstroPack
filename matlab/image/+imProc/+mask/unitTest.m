function Result = unitTest()
    % unitTest for +imProc.mask
    % Example: imProc.mask.unitTest
    
    AI = AstroImage({rand(10,10).*1000});
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500);
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100);
    Result = imProc.mask.maskSaturated(AI, 'SatLevel',500, 'NonLinLeve',100,'MultLevelByGain',true,'Gain',5);

    Result = true;
    
end