function [Result] = perfTest()
    % perfTest for: tools.array

    %% tools.array.mex.countNaN
    
    A=rand(1000,1000);
    A(11:20)=NaN;
    Nsim=1e3;
    tic; for I=1:1:Nsim, S=sum(isnan(A)); end, T1=toc;
    tic; for I=1:1:Nsim, S1=tools.array.mex.countNaN(A); end, T2=toc;
    fprintf('tools.array.mex.countNaN is x %f times faster than sum(isnan(A))\n',T1./T2)

    %% tools.array.bitor_array.m
    Array = uint32(randi(2^16,1600,1600,20));      
    %Array = uint32(randi(2^16,1e4,1));
    Nsim = 1;
    tic; for I=1:1:Nsim, Val1 = tools.array.bitor_array(Array,3,[false false]); end, T1=toc;
    tic; for I=1:1:Nsim, Val2 = tools.array.bitor_array(Array,3,false); end, T2=toc;
    tic; for I=1:1:Nsim, Val3 = tools.array.bitor_array(Array,3,true); end, T3=toc;
    tic; for I=1:1:Nsim, Val4 = tools.array.mex.bitor_dim(Array,3); end, T4=toc;
    %tic; for I=1:1:Nsim, Val2 = bitor_dim(Array,3); end, T2=toc;
    fprintf('bitor_array mex is x %f times faster than non mex\n', T1./T2);
    fprintf('bitor_dim mex is x %f times faster than non mex\n', T1./T3);
    fprintf('bitor_dim mex direct call is x %f times faster than non mex\n', T1./T4);


    %% tools.array.bitand_array.m
    Array = uint32(randi(2^16,1600,1600,20));      
    %Array = uint32(randi(2^16,1e4,1));
    Nsim = 1;
    tic; for I=1:1:Nsim, Val1 = tools.array.bitand_array(Array,3,[false false]); end, T1=toc;
    tic; for I=1:1:Nsim, Val2 = tools.array.bitand_array(Array,3,false); end, T2=toc;
    tic; for I=1:1:Nsim, Val3 = tools.array.bitand_array(Array,3,true); end, T3=toc;
    %tic; for I=1:1:Nsim, Val2 = bitand_dim(Array,3); end, T2=toc;
    fprintf('bitand_array mex is x %f times faster than non mex\n', T1./T2);
    fprintf('bitand_dim mex is x %f times faster than non mex\n', T1./T3);


    %% bitsetFlag
    Array = uint32(zeros(1716,1716));
    Flag  = rand(1716,1716)>0.95;
       
    Nsim = 100;

    tic; for I=1:Nsim, Res1 = tools.array.bitsetFlag(Array, Flag, 13, true, [false false]); end, T1=toc;
    tic; for I=1:Nsim, Res2 = tools.array.bitsetFlag(Array, Flag, 13, true, false); end, T2=toc;
    tic; for I=1:Nsim, Res3 = tools.array.bitsetFlag(Array, Flag, 13, true, true); end, T3=toc;
    fprintf('tools.array.bitsetFlag old mex is x %f faster than matlab\n',T1./T2);
    fprintf('tools.array.bitsetFlag new mex is x %f faster than matlab\n',T1./T3);

    %%
    Result = true;
end
