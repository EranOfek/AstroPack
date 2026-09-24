function [Result] = perfTest()
    % Performence tests for celestial.coo
    % Example: celestial.coo.perfTest

    %% cosined2coo_mex
    CD1=rand(1e6,2).*2-1;
    CD2=rand(1e6,2).*2-1;
    CD3=rand(1e6,2).*2-1;
    tic;[Long,Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3, true);T1=toc;           
    tic;[Long1,Lat1] = celestial.coo.cosined2coo(CD1, CD2, CD3, false);T2=toc;
    fprintf('celestial.coo.mex.cosined2coo is X %f times faster than matlab\n',T2./T1);


    Result = true;
end
