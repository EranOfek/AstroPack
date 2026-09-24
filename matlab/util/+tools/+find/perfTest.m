function [Result] = perfTest(X, Y, Args)
    % perfTest for tools.find
    % Example: tools.find.perfTest


    %% tols.find.binarySearch

    Vec = sort(rand(1e3,1));
    Tar = rand(1,2000);
   
    Nsim=1e3;
    tic; for Isim=1:1:Nsim, Res = tools.find.mfind_bin(Vec,Tar,false); end, T1=toc;
    tic; for Isim=1:1:Nsim, Res = binarySearch(Vec,Tar); end, T2=toc;
    tic; for Isim=1:1:Nsim, Res = tools.find.mex.binarySearch(Vec,Tar); end, T3=toc;
    fprintf('tools.find.mex.binarySearch is %f times faster than tools.find.mfind_bin (old mex)\n', T2./T3);
    fprintf('tools.find.mex.binarySearch is x %f faster than tools.find.mfind_bin (no mex)\n',T1./T3);



end
