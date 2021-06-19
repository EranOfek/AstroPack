function Result = unitTest
    % unitTest for imProc.instCharc
    % Example: Result = imProc.instCharc.unitTest
   
    AI = AstroImage({randn(100,100)+100, randn(100,100)+100, randn(100,100)+100, randn(100,100)+100, randn(100,100)+100});
    Result = imProc.instCharc.readNoiseFromBias(AI);
    if any(abs([Result.ReadNoise] - 1)>0.05)
        error('Problem with readNoiseFromBias block method');
    end
    Result = imProc.instCharc.readNoiseFromBias(AI, 'Method','pix');
    if abs(median(Result.ReadNoise(:)) - 1)>0.15
        error('Problem with readNoiseFromBias pix method');
    end
    Image = poissrnd(ones(100,100).*1e4)./2.2;
    AI = AstroImage({Image});
    Result = imProc.instCharc.gainFromFlat(AI);
    if abs(Result.Gain - 2.2)>0.2
        error('Problem with gainFromFlat block method');
    end
    for I=1:1:30
        Cell{I} = poissrnd(ones(100,100).*1e4)./2.2;
    end
    AI = AstroImage(Cell);
    Result = imProc.instCharc.gainFromFlat(AI, 'Method','pix');
    if abs(Result.GlobalGain - 2.2)>0.2
        error('Problem with gainFromFlat pix method');
    end
    
    Result = true;
    
end