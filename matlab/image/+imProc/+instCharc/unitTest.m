function Result = unitTest
    % unitTest for imProc.instCharc
    % Example: Result = imProc.instCharc.unitTest
   
    AI = AstroImage({randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100, randn(1000,1000)+100});
    Result = imProc.instCharc.readNoiseFromBias(AI);
    Result = imProc.instCharc.readNoiseFromBias(AI, 'Method''pix');

    Result = true;
    
end