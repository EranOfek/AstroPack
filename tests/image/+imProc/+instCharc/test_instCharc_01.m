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
    
    Im = ones(500,500);
    AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});
    AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});
    AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});
    Result = imProc.instCharc.linearity(AI);
    
    Result = true;    
end