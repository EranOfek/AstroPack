function Result = unitTest()
    % unitTest for +dark 
    % Example: Result = imProc.dark.unitTest
    io.msgLog(LogLevel.Test, 'imProc.dark test started');
    
    % identifySimilarImages
    AI = AstroImage({ones(100,100), ones(100,100)});
    [PassedThreshold, FracIdentical] = imProc.dark.identifySimilarImages(AI);
    if ~PassedThreshold
        error('identifySimilarImages failed on detecting identical pixels');
    end
    AI = AstroImage({ones(100,100), zeros(100,100)});
    [PassedThreshold, FracIdentical] = imProc.dark.identifySimilarImages(AI);
    if PassedThreshold
        error('identifySimilarImages failed on detecting identical pixels');
    end

    % compare2template
    AI = AstroImage({2.*randn(10,10)});
    Template = AstroImage({0},'Var',{4});
    [FlagBad, FracbadPixels, Z] = imProc.dark.compare2template(AI, Template);
    if FlagBad
        error('Possible problem with compare2template');
    end
    
    [FlagBad, FracbadPixels, Z] = imProc.dark.compare2template(AI, Template,'Nsigma',0.001);
    if ~FlagBad
        error('Possible problem with compare2template');
    end
    

    % identifyFlaringPixels
    Cube = randn(100,100,10);
    Cube(1,1,1)=30;
    [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube);
    [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube,'MeanFunArgs',{'all'});
    if ~Result(1,1) && all(Result(:,2))
        error('Possible problem with identifyFlaringPixels');
    end

    % isBias/isDark
    AI=AstroImage({rand(100,100)});
    [Result,Flag] = imProc.dark.isBias(AI);
    if Result
        error('isBias problem');
    end
    AI.HeaderData.insertKey({'IMTYPE','Dark'});
    [Result,Flag] = imProc.dark.isBias(AI);
    if Result
        error('isBias problem');
    end
    [Result,Flag] = imProc.dark.isDark(AI);
    if ~Result
        error('isBias problem');
    end

    % bias
    AI=AstroImage({rand(100,100), rand(100,100), rand(100,100), rand(100,100), rand(100,100)});
    AI.funHeader(@insertKey, {'IMTYPE','Bias'});
    AI = [AI, AstroImage({rand(100,100)})];
    Bias = imProc.dark.bias(AI);

    % debias
    %AI = imProc.dark.debias(AI);

    % overscan
    AI = AstroImage({rand(200,100)});
    [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[1 10 1 200]);
    [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[91 100 1 200]);
    [Result, OverScanAI] = imProc.dark.overscan(AI, 'OverScan',[91 100 1 200],'Method','medmedfilt');
    [y,x] = sizeImage(Result);
    if ~(x==90 && y==200)
        error('Problem with overscan subtraction');
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.dark test passed');
    Result = true;
end
