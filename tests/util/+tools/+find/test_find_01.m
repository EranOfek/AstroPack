function Result = unitTest()
    % unitTest for package: tools.find
    % Example: tools.find.unitTest
    

    % tools.find.mfind_bin
    X = rand(1e4,1);
    X = sort(X);
    Vals = rand(1,1e3);
    I1 = tools.find.mfind_bin(X,Vals);
    I2 = tools.find.mfind_bin(X,Vals,true);
    if sum(I1~=I2)>0
        error('tools.find.mfind_bin mex inconsistent');
    end
    X = rand(1e4,1);
    X(1:100) = NaN;
    X = sort(X);
    Vals = rand(1,1e3);
    I1 = tools.find.mfind_bin(X,Vals);
    I2 = tools.find.mfind_bin(X,Vals,true);
    if sum(I1~=I2)>0
        error('tools.find.mfind_bin mex inconsistent');
    end
    
    tic; for I=1:1000, I1 = tools.find.mfind_bin(X,Vals); end, toc
    tic; for I=1:1000, I2 = tools.find.mfind_bin(X,Vals,true); end, toc

    
    A=single(rand(1000,1000));                                  
    A(1000:2000)=single(1);
    V=single(1);
    tic;for J=1:1:1e3, IM=tools.find.mex.findEqual(A,V); end, toc
    tic;for J=1:1:1e3, I=find(A==1); end, toc                   
    if sum(abs(IM-I))>0
        error('Error in tools,find.mex.findEqual');
    end

    A=single(rand(1000,1000));                                  
    A(1000:2000)=single(1);
    A(1e5) = 1.1;
    V=single(1);
    tic;for J=1:1:1e3, IM=tools.find.mex.findLargerFirst(A,V); end, toc
    tic;for J=1:1:1e3, I=find(A>1,1); end, toc                   
    if sum(abs(IM-I))>0
        error('Error in tools,find.mex.findLargerFirst');
    end
    
    A=(rand(1000,1000));                                  
    A(1e5) = -1;
    V=(0);
    tic;for J=1:1:1e3, IM=tools.find.mex.findSmallerFirst(A,V); end, toc
    tic;for J=1:1:1e3, I=find(A<0,1); end, toc                   
    if sum(abs(IM-I))>0
        error('Error in tools,find.mex.findSmallerFirst');
    end
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % test_mfind_bin();
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end


function Result = test_mfind_bin()
    io.msgLog(LogLevel.Test, 'tools.find.mfind_bin test started');

    iters = 10;
    
    arr_size = power(10,3);
    
    for var_types=1:2

        MatlabTimeTotal = 0;
        MexTimeTotal = 0;
        MexMPTimeTotal = 0;            
        MatlabTime = 0;
        MexTime = 0;
        MexMPTime = 0;

        for iter=1:iters

            R = [NaN;NaN;rand(1e4,1);NaN;NaN];
            R = sort(R);
            X = rand(10,1)';

            switch var_types
                case 1
                    R = single(R);
                    X = single(X);
                    var_name = 'single';
                case 2
                    R = double(R);
                    X = double(X);
                    var_name = 'double';
            end

            UseMex = 0;
            UseMP = 0;
            t = tic;
            matlab_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MatlabTime = toc(t);
            MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

            UseMex = 1;
            UseMP = 0;
            t = tic;
            mex_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MexTime = toc(t);
            MexTimeTotal = MexTimeTotal + MexTime;

            UseMex = 1;
            UseMP = 1;
            t = tic;
            mex_mp_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MexMPTime = toc(t);
            MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                    
            assert(isequal(matlab_res, mex_res));
        end

        MatlabTime = MatlabTimeTotal / iters;
        MexTime = MexTimeTotal / iters;
        MexMPTime = MexMPTimeTotal / iters;

        fprintf('Var_type: %s, Matlab: %.6fs, Mex: %.6fs, MexMP: %.6fs\n', var_name, MatlabTime, MexTime, MexMPTime);

    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.find.mfind_bin passed');
    Result = true;    
end   

