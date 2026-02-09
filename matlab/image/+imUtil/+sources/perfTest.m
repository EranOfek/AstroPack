function [Result] = perfTest()
    % perfTest for imUtil.sources
    % Example: imUtil.sources.perfTest

    %% findLocalMax.m

    Image = randn(1716, 1716);

    
    Nsim = 30;
    tic;
    for I=1:Nsim
        [Pos, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','imregionalmax');
    end
    T1=toc;
    tic;
    for I=1:Nsim
        [Pos, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','findlocal');
    end
    T2=toc;
    tic;
    for I=1:Nsim
        [Pos, BW, MaxIsn] = imUtil.sources.findLocalMax(Image,'Variance',1,'Threshold',3, 'Algo','findlocalmex');
    end
    T3=toc;
    %tic;
    % for I1=1:Nsim
    %     [Ind,I,J,BW] = findLocalMaxAboveThreshold_mex(Image,3);
    % end
    % T4=toc

    fprintf('imUtil.sources.findLocalMax: findlocal option is x %f faster than imregionalmax option\n',T1./T2);
    fprintf('imUtil.sources.findLocalMax: mex option is x %f faster than imregionalmax option\n',T1./T3);



    %                   'findlocal' - use findLocalMaxAboveThreshold.
    %                   'findlocalmex' - use
    %                           findLocalMaxAboveThreshold_mex_single/double.
    %                   'imregionalmax' - use imregionalmax.
    %                   Default is 'findlocalmex'.


    %%

    Result = true;

end
