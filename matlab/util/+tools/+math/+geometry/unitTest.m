function Result = unitTest()
    % unit-Test for tools.math.geometry package

    %%

    Nsim = 10000;
    Nsrc = 100;
    tic;
    for Isim=1:1:Nsim
        X = rand(Nsrc,1);
        Y = rand(Nsrc,1);
        [BestXY, BestRadius] = tools.math.geometry.boundingCircle(X,Y,'UseMex',false);
        %[BestXY1, BestRadius1] = tools.math.geometry.boundingCircle(X,Y,'UseMex',true);
        if max(abs(BestXY - 0.5),[],"all")>0.15 || abs(BestRadius-1./sqrt(2))>0.25
            Isim
            BestXY
            BestRadius
            error('Problem with tools.math.geometry.boundingCircle');
        end

    end
    toc

    tic;
    for Isim=1:1:Nsim
        X = rand(Nsrc,1);
        Y = rand(Nsrc,1);
        [BestXY, BestRadius] = tools.math.geometry.boundingCircle(X,Y,'UseMex',true);
        if max(abs(BestXY - 0.5),[],"all")>0.15 || abs(BestRadius-1./sqrt(2))>0.25
            Isim
            BestXY
            BestRadius
            error('Problem with tools.math.geometry.boundingCircle');
        end

    end
    toc

    %%
	
	Result = true;
end

