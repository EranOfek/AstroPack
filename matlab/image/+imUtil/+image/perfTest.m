function [Result] = perfTest()
    % perfTest for imUtil.image
    % Example: imUtil.image.perfTest

    %% imUtil.image.ind2sub_fast / imUtil.image.mex.ind2sub_mex
    Size = [100 120];
    Npt  = 1e3;
    LI = randi(prod(Size),Npt,1);
    Nsim = 1e4;
    tic;
    for Isim=1:Nsim
        [I1, J1]=ind2sub(Size, LI);
    end
    T1=toc;
    tic;
    for Isim=1:Nsim
        [I2, J2]=imUtil.image.ind2sub_fast(Size, LI);
    end
    T2=toc;

    tic;
    for Isim=1:Nsim
        [I3, J3]=imUtil.image.mex.ind2sub_mex(Size, LI);
    end
    T3=toc;
    fprintf('imUtil.image.ind2sub_fast is x %f faster than ind2sub (with %d points)\n',T1./T2,Npt);
    fprintf('imUtil.image.mex.ind2sub_mex is x %f faster than ind2sub (with %d points)\n',T1./T3,Npt);

    %% imUtil.image.sub2ind_fast / imUtil.image.mex.sub2ind_mex
    Size = [100 120];
    Npt  = 1e3;
    LI = randi(prod(Size),Npt,1);
    [II, JJ]=ind2sub(Size, LI);
    Nsim = 1e4;
    tic;
    for Isim=1:Nsim
        [LI1]=sub2ind(Size, II, JJ);
    end
    T1=toc;
    tic;
    for Isim=1:Nsim
        [LI2]=imUtil.image.sub2ind_fast(Size, II, JJ);
    end
    T2=toc;

    tic;
    for Isim=1:Nsim
        [LI3]=imUtil.image.mex.sub2ind_mex(Size, II, JJ);
    end
    T3=toc;
    fprintf('imUtil.image.sub2ind_fast is x %f faster than sub2ind (with %d points)\n',T1./T2,Npt);
    fprintf('imUtil.image.mex.sub2ind_mex is x %f faster than sub2ind (with %d points)\n',T1./T3,Npt);



    %%

    Result = true;
end
