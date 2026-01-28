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
    tic; for I=1:1:Nsim, Val1 = tools.array.bitor_array(Array,3,false); end, T1=toc;
    tic; for I=1:1:Nsim, Val2 = tools.array.bitor_array(Array,3,true); end, T2=toc;
    %tic; for I=1:1:Nsim, Val2 = bitor_dim(Array,3); end, T2=toc;
    fprintf('bitor_array mex is x %f times faster than non mex\n', T1./T2);

    %% tools.array.bitand_array.m
    Array = uint32(randi(2^16,1600,1600,20));      
    %Array = uint32(randi(2^16,1e4,1));
    Nsim = 1;
    tic; for I=1:1:Nsim, Val1 = tools.array.bitand_array(Array,3,false); end, T1=toc;
    tic; for I=1:1:Nsim, Val2 = tools.array.bitand_array(Array,3,true); end, T2=toc;
    %tic; for I=1:1:Nsim, Val2 = bitand_dim(Array,3); end, T2=toc;
    fprintf('bitand_array mex is x %f times faster than non mex\n', T1./T2);


    %%
    Result = true;
end
